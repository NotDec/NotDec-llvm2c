#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"

#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <llvm/ADT/SmallString.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

namespace notdec::backend::structuring {
namespace {

struct PendingPhiAssignment {
  VVarId Target = InvalidVVarId;
  PayloadRef Payload;
  std::string IncomingName;
};

struct PayloadCache {
  std::map<const llvm::Value *, PayloadRef> Conditions;
  std::map<const llvm::ConstantInt *, PayloadRef> SwitchCases;
};

std::string valueName(const llvm::Value &V, llvm::StringRef Prefix) {
  if (V.hasName()) {
    return V.getName().str();
  }

  if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(&V)) {
    llvm::SmallString<32> Text;
    C->getValue().toString(Text, 10, /*isSigned=*/false);
    return Text.str().str();
  }

  return Prefix.str();
}

PayloadRef cachedCondition(LLVMFunctionCFGBuilder::PayloadProvider &Provider,
                           PayloadCache &Cache, const llvm::Value &V,
                           llvm::StringRef FallbackName) {
  auto It = Cache.Conditions.find(&V);
  if (It != Cache.Conditions.end()) {
    return It->second;
  }
  PayloadRef Payload = Provider.getCondition(V, FallbackName);
  Cache.Conditions.emplace(&V, Payload);
  return Payload;
}

PayloadRef cachedSwitchCase(LLVMFunctionCFGBuilder::PayloadProvider &Provider,
                            PayloadCache &Cache, const llvm::ConstantInt &V) {
  auto It = Cache.SwitchCases.find(&V);
  if (It != Cache.SwitchCases.end()) {
    return It->second;
  }
  PayloadRef Payload = Provider.getSwitchCase(V);
  Cache.SwitchCases.emplace(&V, Payload);
  return Payload;
}

std::optional<ConditionCompareKind>
conditionCompareKindFromICmp(llvm::CmpInst::Predicate Predicate) {
  switch (Predicate) {
  case llvm::CmpInst::ICMP_EQ:
    return ConditionCompareKind::Equal;
  case llvm::CmpInst::ICMP_NE:
    return ConditionCompareKind::NotEqual;
  case llvm::CmpInst::ICMP_UGT:
  case llvm::CmpInst::ICMP_SGT:
    return ConditionCompareKind::GreaterThan;
  case llvm::CmpInst::ICMP_UGE:
  case llvm::CmpInst::ICMP_SGE:
    return ConditionCompareKind::GreaterEqual;
  case llvm::CmpInst::ICMP_ULT:
  case llvm::CmpInst::ICMP_SLT:
    return ConditionCompareKind::LessThan;
  case llvm::CmpInst::ICMP_ULE:
  case llvm::CmpInst::ICMP_SLE:
    return ConditionCompareKind::LessEqual;
  default:
    return std::nullopt;
  }
}

ConditionCompareKind swappedConditionCompareKind(ConditionCompareKind Kind) {
  switch (Kind) {
  case ConditionCompareKind::GreaterThan:
    return ConditionCompareKind::LessThan;
  case ConditionCompareKind::GreaterEqual:
    return ConditionCompareKind::LessEqual;
  case ConditionCompareKind::LessThan:
    return ConditionCompareKind::GreaterThan;
  case ConditionCompareKind::LessEqual:
    return ConditionCompareKind::GreaterEqual;
  case ConditionCompareKind::Equal:
  case ConditionCompareKind::NotEqual:
    return Kind;
  }
  return Kind;
}

bool isSignedICmp(llvm::CmpInst::Predicate Predicate) {
  switch (Predicate) {
  case llvm::CmpInst::ICMP_SGT:
  case llvm::CmpInst::ICMP_SGE:
  case llvm::CmpInst::ICMP_SLT:
  case llvm::CmpInst::ICMP_SLE:
    return true;
  default:
    return false;
  }
}

std::optional<ConditionCompare>
conditionCompareFromICmp(const llvm::Value &V,
                         LLVMFunctionCFGBuilder::PayloadProvider &Provider,
                         PayloadCache &Cache) {
  const auto *ICmp = llvm::dyn_cast<llvm::ICmpInst>(&V);
  if (ICmp == nullptr) {
    return std::nullopt;
  }

  std::optional<ConditionCompareKind> InitialKind =
      conditionCompareKindFromICmp(ICmp->getPredicate());
  if (!InitialKind.has_value()) {
    return std::nullopt;
  }
  ConditionCompareKind Kind = *InitialKind;

  const llvm::Value *Compared = ICmp->getOperand(0);
  const llvm::ConstantInt *Constant =
      llvm::dyn_cast<llvm::ConstantInt>(ICmp->getOperand(1));
  if (Constant == nullptr) {
    Constant = llvm::dyn_cast<llvm::ConstantInt>(ICmp->getOperand(0));
    Compared = ICmp->getOperand(1);
    Kind = swappedConditionCompareKind(Kind);
  }
  if (Constant == nullptr) {
    return std::nullopt;
  }

  bool HasIntegerValue = Constant->getBitWidth() <= 64;
  // LLVM's BasicBlock successor iteration currently matches the existing
  // shared-CFG convention used by this builder: index 1 is the condition-true
  // edge. Keep that local here so range metadata follows the same convention as
  // the older eq/ne metadata.
  std::size_t TrueTargetIndex = 1;
  return ConditionCompare{
      .ComparedValue = cachedCondition(Provider, Cache, *Compared, "switch"),
      .ConstantValue = cachedSwitchCase(Provider, Cache, *Constant),
      .HasIntegerValue = HasIntegerValue,
      .SignedPredicate = isSignedICmp(ICmp->getPredicate()),
      .SignedIntegerValue = HasIntegerValue ? Constant->getSExtValue() : 0,
      .UnsignedIntegerValue = HasIntegerValue ? Constant->getZExtValue() : 0,
      .TrueTargetIndex = TrueTargetIndex,
      .EqualTargetIndex = Kind == ConditionCompareKind::NotEqual
                              ? std::size_t{0}
                              : TrueTargetIndex,
      .Kind = Kind};
}

} // namespace

StructuredCFG
LLVMFunctionCFGBuilder::build(const llvm::Function &F,
                              LLVMFunctionCFGBuilder::PayloadProvider &Provider) {
  StructuredCFG Cfg;
  PayloadCache Cache;
  std::map<const llvm::BasicBlock *, BlockId> BlockIds;

  for (const llvm::BasicBlock &BB : F) {
    BlockId Id = static_cast<BlockId>(BlockIds.size());
    BlockIds[&BB] = Id;
  }

  for (const llvm::BasicBlock &BB : F) {
    CFGBlock Block;
    Block.Id = BlockIds[&BB];
    Provider.collectStatements(BB, Block.Statements);
    for (const llvm::Instruction &I : BB) {
      if (llvm::isa<llvm::CallBase>(&I)) {
        ++Block.CallCount;
      }
    }

    const llvm::Instruction *Term = BB.getTerminator();
    if (const auto *Br = llvm::dyn_cast_or_null<llvm::BranchInst>(Term)) {
      if (Br->isConditional()) {
        Block.Terminator = TerminatorKind::Branch;
        Block.Condition =
            cachedCondition(Provider, Cache, *Br->getCondition(), "cond");
        if (std::optional<ConditionCompare> Compare =
                conditionCompareFromICmp(*Br->getCondition(), Provider,
                                         Cache)) {
          Cfg.setConditionCompare(Block.Condition, *Compare);
        }
      } else {
        Block.Terminator = TerminatorKind::Fallthrough;
      }
      for (const llvm::BasicBlock *Succ : Br->successors()) {
        Block.Successors.push_back(BlockIds[Succ]);
      }
    } else if (const auto *Sw = llvm::dyn_cast_or_null<llvm::SwitchInst>(Term)) {
      Block.Terminator = TerminatorKind::Switch;
      Block.Condition =
          cachedCondition(Provider, Cache, *Sw->getCondition(), "switch");
      for (auto Case : Sw->cases()) {
        SwitchCase OutCase;
        OutCase.Value =
            cachedSwitchCase(Provider, Cache, *Case.getCaseValue());
        OutCase.Target = BlockIds[Case.getCaseSuccessor()];
        Block.Cases.push_back(OutCase);
      }
      for (unsigned I = 0; I < Sw->getNumSuccessors(); ++I) {
        Block.Successors.push_back(BlockIds[Sw->getSuccessor(I)]);
      }
    } else if (llvm::isa_and_nonnull<llvm::ReturnInst>(Term)) {
      Block.Terminator = TerminatorKind::Return;
    } else if (llvm::isa_and_nonnull<llvm::UnreachableInst>(Term)) {
      Block.Terminator = TerminatorKind::Unreachable;
    } else {
      Block.Terminator = TerminatorKind::Unreachable;
    }

    Cfg.addBlock(std::move(Block));
  }

  std::map<std::pair<BlockId, BlockId>, std::vector<PendingPhiAssignment>>
      PhiAssignments;
  for (const llvm::BasicBlock &BB : F) {
    auto MergeIt = BlockIds.find(&BB);
    if (MergeIt == BlockIds.end()) {
      continue;
    }
    BlockId Merge = MergeIt->second;

    for (const llvm::Instruction &I : BB) {
      const auto *Phi = llvm::dyn_cast<llvm::PHINode>(&I);
      if (Phi == nullptr) {
        break;
      }

      std::string PhiName = valueName(*Phi, "phi");
      VVarId VVar = Cfg.addDephicationVVar(PhiName, Merge);
      for (unsigned Idx = 0; Idx < Phi->getNumIncomingValues(); ++Idx) {
        const llvm::BasicBlock *IncomingBlock = Phi->getIncomingBlock(Idx);
        auto PredIt = BlockIds.find(IncomingBlock);
        if (PredIt == BlockIds.end()) {
          continue;
        }
        BlockId Pred = PredIt->second;
        const llvm::Value *IncomingValue = Phi->getIncomingValue(Idx);
        std::string IncomingName = valueName(*IncomingValue, "incoming");
        PayloadRef Payload = Provider.getPhiAssignment(
            *Phi, *IncomingValue, PhiName, IncomingName);
        PhiAssignments[{Pred, Merge}].push_back(
            {.Target = VVar, .Payload = Payload, .IncomingName = IncomingName});
      }
    }
  }

  for (const auto &Entry : PhiAssignments) {
    BlockId Pred = Entry.first.first;
    BlockId Merge = Entry.first.second;
    if (!Cfg.hasEdge(Pred, Merge)) {
      continue;
    }

    CFGBlock EdgeBlock;
    EdgeBlock.Origin = CFGBlockOrigin::Synthetic;
    EdgeBlock.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
    EdgeBlock.CreatedBy = CFGBlockCreator::SAILRDephication;
    EdgeBlock.SyntheticSource = Pred;
    EdgeBlock.SyntheticTarget = Merge;
    EdgeBlock.Statements.reserve(Entry.second.size());
    for (const PendingPhiAssignment &Assignment : Entry.second) {
      EdgeBlock.Statements.push_back(Assignment.Payload);
    }
    EdgeBlock.Terminator = TerminatorKind::Fallthrough;
    EdgeBlock.Successors = {Merge};
    BlockId Edge = Cfg.addBlock(std::move(EdgeBlock));
    Cfg.replaceEdge(Pred, Merge, Edge);
    for (const PendingPhiAssignment &Assignment : Entry.second) {
      Cfg.addDephicationIncoming(Assignment.Target, Pred, Merge, Edge,
                                 Assignment.Payload, Assignment.IncomingName);
    }
  }

  return Cfg;
}

} // namespace notdec::backend::structuring
