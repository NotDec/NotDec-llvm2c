#include "notdec-backends/Solidity/BodyBuilder.h"
#include "notdec-backends/Structuring/GotoStructurer.h"

#include <cctype>
#include <map>
#include <utility>

#include <llvm/ADT/SmallString.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>

namespace notdec::backend::solidity {

namespace {

using structuring::BlockId;
using structuring::CFGBlock;
using structuring::GotoStructurer;
using structuring::InvalidNodeId;
using structuring::PayloadRef;
using structuring::StructuredCFG;
using structuring::StructuredNode;
using structuring::StructuredNodeKind;
using structuring::StructuredTree;
using structuring::SwitchCase;
using structuring::TerminatorKind;

PayloadRef addPayload(std::vector<std::string> &Payloads, std::string Text) {
  Payloads.push_back(std::move(Text));
  return PayloadRef{Payloads.size() - 1};
}

std::string blockName(BlockId Id) { return "block_" + std::to_string(Id); }

std::string payloadText(const std::vector<std::string> &Payloads,
                        PayloadRef Ref) {
  if (!Ref.isValid() || Ref.Id >= Payloads.size()) {
    return "unknown";
  }
  return Payloads[Ref.Id];
}

void renderStructuredNode(const StructuredTree &Tree,
                          const std::vector<std::string> &Payloads,
                          structuring::NodeId Id,
                          std::vector<std::string> &Out) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return;
  }

  switch (Node->Kind) {
  case StructuredNodeKind::Sequence:
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  case StructuredNodeKind::Label:
    Out.push_back("// " + blockName(Node->Block) + ":");
    break;
  case StructuredNodeKind::BasicBlock:
    for (PayloadRef Ref : Node->Statements) {
      Out.push_back(payloadText(Payloads, Ref));
    }
    break;
  case StructuredNodeKind::If:
    Out.push_back("// if " + payloadText(Payloads, Node->Condition));
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  case StructuredNodeKind::Switch:
    Out.push_back("// switch " + payloadText(Payloads, Node->Condition));
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  case StructuredNodeKind::Goto:
    Out.push_back("// goto " + blockName(Node->Target));
    break;
  case StructuredNodeKind::Return:
    Out.push_back("// return");
    break;
  case StructuredNodeKind::Unreachable:
    Out.push_back("// unreachable");
    break;
  case StructuredNodeKind::Break:
    Out.push_back("// break");
    break;
  case StructuredNodeKind::Continue:
    Out.push_back("// continue");
    break;
  case StructuredNodeKind::While:
    Out.push_back("// while " + payloadText(Payloads, Node->Condition));
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  case StructuredNodeKind::DoWhile:
    Out.push_back("// do-while " + payloadText(Payloads, Node->Condition));
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  case StructuredNodeKind::InfiniteLoop:
    Out.push_back("// infinite loop");
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out);
    }
    break;
  }
}

std::string llvmValueName(const llvm::Value &V, llvm::StringRef Prefix) {
  if (V.hasName()) {
    return V.getName().str();
  }
  return Prefix.str();
}

} // namespace

std::vector<std::string> BodyBuilder::readBody(const llvm::Function &F) {
  StructuredCFG Cfg;
  std::vector<std::string> Payloads;
  std::map<const llvm::BasicBlock *, BlockId> BlockIds;

  for (const llvm::BasicBlock &BB : F) {
    BlockId Id = static_cast<BlockId>(BlockIds.size());
    BlockIds[&BB] = Id;
  }

  for (const llvm::BasicBlock &BB : F) {
    CFGBlock Block;
    Block.Id = BlockIds[&BB];

    for (const llvm::Instruction &I : BB) {
      if (std::optional<std::string> Kind =
              getStringMetadata(I, "notdec.solidity.revert")) {
        Block.Statements.push_back(
            addPayload(Payloads, formatRevertStatement(I, *Kind)));
        continue;
      }
      if (std::optional<std::string> Kind =
              getStringMetadata(I, "notdec.solidity.event")) {
        Block.Statements.push_back(
            addPayload(Payloads, formatEventStatement(I, *Kind)));
      }
    }

    const llvm::Instruction *Term = BB.getTerminator();
    if (const auto *Br = llvm::dyn_cast_or_null<llvm::BranchInst>(Term)) {
      if (Br->isConditional()) {
        Block.Terminator = TerminatorKind::Branch;
        Block.Condition =
            addPayload(Payloads, llvmValueName(*Br->getCondition(), "cond"));
      } else {
        Block.Terminator = TerminatorKind::Fallthrough;
      }
      for (const llvm::BasicBlock *Succ : Br->successors()) {
        Block.Successors.push_back(BlockIds[Succ]);
      }
    } else if (const auto *Sw = llvm::dyn_cast_or_null<llvm::SwitchInst>(Term)) {
      Block.Terminator = TerminatorKind::Switch;
      Block.Condition =
          addPayload(Payloads, llvmValueName(*Sw->getCondition(), "switch"));
      for (auto Case : Sw->cases()) {
        SwitchCase OutCase;
        llvm::SmallString<32> Text;
        Case.getCaseValue()->getValue().toString(Text, 10,
                                                 /*isSigned=*/false);
        OutCase.Value = addPayload(Payloads, Text.str().str());
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

  StructuredTree Tree = GotoStructurer().structure(Cfg);
  std::vector<std::string> Result;
  if (Tree.root() != InvalidNodeId) {
    renderStructuredNode(Tree, Payloads, Tree.root(), Result);
  }

  Result.push_back(Payloads.empty() ? "// TODO: recover body"
                                    : "// TODO: recover remaining body");
  return Result;
}

std::optional<std::string>
BodyBuilder::getStringMetadata(const llvm::Instruction &I,
                               llvm::StringRef Kind) {
  const llvm::MDNode *Node = I.getMetadata(Kind);
  if (Node == nullptr || Node->getNumOperands() != 1) {
    return std::nullopt;
  }
  const auto *Value = llvm::dyn_cast<llvm::MDString>(Node->getOperand(0));
  if (Value == nullptr) {
    return std::nullopt;
  }
  return Value->getString().str();
}

std::optional<std::string>
BodyBuilder::getEventName(const llvm::Instruction &I, llvm::StringRef Kind) {
  const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I);
  if (Call == nullptr || !Call->getCalledFunction() ||
      !Call->getCalledFunction()->getName().starts_with("evm_log") ||
      Call->getCalledFunction()->getName().size() != 8 ||
      Call->arg_size() < 3) {
    return std::nullopt;
  }
  if (Kind.empty()) {
    return std::nullopt;
  }

  std::string Name = ("Event_" + Kind).str();
  if (Call->arg_size() >= 4) {
    const auto *Topic0 =
        llvm::dyn_cast<llvm::ConstantInt>(Call->getArgOperand(3));
    if (Topic0 != nullptr) {
      llvm::SmallString<64> Text;
      Topic0->getValue().toString(Text, 16, /*isSigned=*/false);
      Name = "Event_0x" + Text.str().str();
    }
  }
  return sanitizeIdentifier(Name);
}

std::string BodyBuilder::formatRevertStatement(const llvm::Instruction &I,
                                               llvm::StringRef Kind) {
  std::string Result = "revert();";
  std::string Comment = Kind.str();

  if (std::optional<std::string> Code =
          getStringMetadata(I, "notdec.solidity_revert.panic_code")) {
    Comment += ", panic=" + *Code;
  }
  if (std::optional<std::string> Selector =
          getStringMetadata(I, "notdec.solidity_revert.selector")) {
    Comment += ", selector=0x" + *Selector;
  }
  if (std::optional<std::string> Count = getStringMetadata(
          I, "notdec.solidity_revert.custom_error_arg_count")) {
    Comment += ", args=" + *Count;
  }
  if (std::optional<std::string> Length =
          getStringMetadata(I, "notdec.solidity_revert.error_string_length")) {
    Comment += ", string_length=" + *Length;
  }

  return Result + " // " + Comment;
}

std::string BodyBuilder::formatEventStatement(const llvm::Instruction &I,
                                              llvm::StringRef Kind) {
  std::string Name =
      getEventName(I, Kind).value_or(sanitizeIdentifier(("Event_" + Kind).str()));
  return "emit " + Name + "(); // TODO: recover event signature";
}

std::string BodyBuilder::sanitizeIdentifier(llvm::StringRef Name) {
  std::string Result;
  Result.reserve(Name.size());
  for (char C : Name) {
    unsigned char UC = static_cast<unsigned char>(C);
    if (std::isalnum(UC) || C == '_') {
      Result.push_back(C);
    } else {
      Result.push_back('_');
    }
  }
  if (Result.empty()) {
    return "public_unknown";
  }
  if (std::isdigit(static_cast<unsigned char>(Result.front()))) {
    Result.insert(Result.begin(), '_');
  }
  return Result;
}

} // namespace notdec::backend::solidity
