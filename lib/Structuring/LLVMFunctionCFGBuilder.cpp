#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"

#include <map>
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

} // namespace

StructuredCFG
LLVMFunctionCFGBuilder::build(const llvm::Function &F,
                              LLVMFunctionCFGBuilder::PayloadProvider &Provider) {
  StructuredCFG Cfg;
  std::map<const llvm::BasicBlock *, BlockId> BlockIds;

  for (const llvm::BasicBlock &BB : F) {
    BlockId Id = static_cast<BlockId>(BlockIds.size());
    BlockIds[&BB] = Id;
  }

  for (const llvm::BasicBlock &BB : F) {
    CFGBlock Block;
    Block.Id = BlockIds[&BB];
    Provider.collectStatements(BB, Block.Statements);

    const llvm::Instruction *Term = BB.getTerminator();
    if (const auto *Br = llvm::dyn_cast_or_null<llvm::BranchInst>(Term)) {
      if (Br->isConditional()) {
        Block.Terminator = TerminatorKind::Branch;
        Block.Condition = Provider.getCondition(*Br->getCondition(), "cond");
      } else {
        Block.Terminator = TerminatorKind::Fallthrough;
      }
      for (const llvm::BasicBlock *Succ : Br->successors()) {
        Block.Successors.push_back(BlockIds[Succ]);
      }
    } else if (const auto *Sw = llvm::dyn_cast_or_null<llvm::SwitchInst>(Term)) {
      Block.Terminator = TerminatorKind::Switch;
      Block.Condition = Provider.getCondition(*Sw->getCondition(), "switch");
      for (auto Case : Sw->cases()) {
        SwitchCase OutCase;
        OutCase.Value = Provider.getSwitchCase(*Case.getCaseValue());
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
