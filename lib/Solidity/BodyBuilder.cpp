#include "notdec-backends/Solidity/BodyBuilder.h"
#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"

#include <cctype>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <utility>

#include <llvm/ADT/SmallString.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>

namespace notdec::backend::solidity {

namespace {

using structuring::BlockId;
using structuring::InvalidNodeId;
using structuring::LLVMFunctionCFGBuilder;
using structuring::PayloadMaterializeContext;
using structuring::PayloadMaterializeKind;
using structuring::PayloadRef;
using structuring::StructuredCFG;
using structuring::StructuredNode;
using structuring::StructuredNodeKind;
using structuring::StructuredTree;
using structuring::VVarId;

PayloadRef addPayload(std::vector<std::string> &Payloads, std::string Text) {
  Payloads.push_back(std::move(Text));
  return PayloadRef{Payloads.size() - 1};
}

bool isIdentifierChar(char C) {
  return std::isalnum(static_cast<unsigned char>(C)) || C == '_';
}

std::string replaceIdentifier(std::string Text, llvm::StringRef From,
                              llvm::StringRef To) {
  if (From.empty() || From == To) {
    return Text;
  }

  std::size_t Pos = 0;
  while ((Pos = Text.find(From.str(), Pos)) != std::string::npos) {
    bool HasLeft = Pos > 0 && isIdentifierChar(Text[Pos - 1]);
    std::size_t End = Pos + From.size();
    bool HasRight = End < Text.size() && isIdentifierChar(Text[End]);
    if (HasLeft || HasRight) {
      Pos = End;
      continue;
    }
    Text.replace(Pos, From.size(), To.str());
    Pos += To.size();
  }
  return Text;
}

std::string copiedVVarName(llvm::StringRef SourceName, VVarId Copy) {
  return (SourceName + "_copy" + std::to_string(Copy)).str();
}

std::string payloadText(const std::vector<std::string> &Payloads,
                        PayloadRef Ref) {
  if (!Ref.isValid() || Ref.Id >= Payloads.size()) {
    return "unknown";
  }
  return Payloads[Ref.Id];
}

std::string conditionText(const std::vector<std::string> &Payloads,
                          const StructuredNode &Node) {
  std::string Text = payloadText(Payloads, Node.Condition);
  return Node.ConditionNegated ? "!(" + Text + ")" : Text;
}

std::string indent(unsigned Depth) { return std::string(Depth * 2, ' '); }

// The backend does not own expression recovery yet. Keep unknown low-level
// predicates as comments while emitting a valid Solidity boolean expression.
std::string todoCondition(llvm::StringRef Text) {
  std::string Comment = Text.str();
  std::string::size_type Pos = 0;
  while ((Pos = Comment.find("*/", Pos)) != std::string::npos) {
    Comment.replace(Pos, 2, "* /");
    Pos += 3;
  }
  return "false /* TODO: " + Comment + " */";
}

std::string solidityStringLiteral(llvm::StringRef Text) {
  std::string Result = "\"";
  for (char C : Text) {
    switch (C) {
    case '\\':
      Result += "\\\\";
      break;
    case '"':
      Result += "\\\"";
      break;
    case '\n':
      Result += "\\n";
      break;
    case '\r':
      Result += "\\r";
      break;
    case '\t':
      Result += "\\t";
      break;
    default:
      if (static_cast<unsigned char>(C) >= 0x20 &&
          static_cast<unsigned char>(C) < 0x7f) {
        Result.push_back(C);
      } else {
        Result += "\\x";
        constexpr char Hex[] = "0123456789abcdef";
        Result.push_back(Hex[(static_cast<unsigned char>(C) >> 4) & 0xf]);
        Result.push_back(Hex[static_cast<unsigned char>(C) & 0xf]);
      }
      break;
    }
  }
  Result += "\"";
  return Result;
}

void renderStructuredNode(const StructuredTree &Tree,
                          const std::vector<std::string> &Payloads,
                          structuring::NodeId Id,
                          std::vector<std::string> &Out,
                          unsigned Depth = 0,
                          bool InLoop = false) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return;
  }

  switch (Node->Kind) {
  case StructuredNodeKind::Sequence:
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out, Depth, InLoop);
    }
    break;
  case StructuredNodeKind::Label:
    Out.push_back(indent(Depth) + "// block_" + std::to_string(Node->Block) +
                  ":");
    break;
  case StructuredNodeKind::BasicBlock:
    for (PayloadRef Ref : Node->Statements) {
      Out.push_back(indent(Depth) + payloadText(Payloads, Ref));
    }
    break;
  case StructuredNodeKind::If:
    Out.push_back(indent(Depth) + "if (" +
                  todoCondition(conditionText(Payloads, *Node)) + ") {");
    if (Node->Children.empty() &&
        (Node->Then != InvalidNodeId || Node->Else != InvalidNodeId)) {
      renderStructuredNode(Tree, Payloads, Node->Then, Out, Depth + 1, InLoop);
      Out.push_back(indent(Depth) + "}");
      if (Node->Else != InvalidNodeId) {
        Out.push_back(indent(Depth) + "else {");
        renderStructuredNode(Tree, Payloads, Node->Else, Out, Depth + 1,
                             InLoop);
        Out.push_back(indent(Depth) + "}");
      }
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Out, Depth + 1, InLoop);
      }
      Out.push_back(indent(Depth) + "}");
    }
    break;
  case StructuredNodeKind::Switch:
    Out.push_back(indent(Depth) + "{");
    Out.push_back(indent(Depth + 1) + "/* TODO: switch " +
                  payloadText(Payloads, Node->Condition) + " */");
    if (Node->Children.empty() &&
        (!Node->StructuredCases.empty() || Node->Default != InvalidNodeId)) {
      for (const auto &Case : Node->StructuredCases) {
        Out.push_back(indent(Depth + 1) + "/* case " +
                      payloadText(Payloads, Case.Value) + " */");
        renderStructuredNode(Tree, Payloads, Case.Body, Out, Depth + 1,
                             InLoop);
      }
      if (Node->Default != InvalidNodeId) {
        Out.push_back(indent(Depth + 1) + "/* default */");
        renderStructuredNode(Tree, Payloads, Node->Default, Out, Depth + 1,
                             InLoop);
      }
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Out, Depth + 1, InLoop);
      }
    }
    Out.push_back(indent(Depth) + "}");
    break;
  case StructuredNodeKind::Goto:
    Out.push_back(indent(Depth) + "// goto block_" +
                  std::to_string(Node->Target));
    break;
  case StructuredNodeKind::Return:
    break;
  case StructuredNodeKind::Unreachable:
    break;
  case StructuredNodeKind::Break:
    if (InLoop) {
      Out.push_back(indent(Depth) + "break;");
    }
    break;
  case StructuredNodeKind::Continue:
    if (InLoop) {
      Out.push_back(indent(Depth) + "continue;");
    }
    break;
  case StructuredNodeKind::While:
    Out.push_back(indent(Depth) + "while (" +
                  todoCondition(conditionText(Payloads, *Node)) + ") {");
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, Out, Depth + 1,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Out, Depth + 1,
                             /*InLoop=*/true);
      }
    }
    Out.push_back(indent(Depth) + "}");
    break;
  case StructuredNodeKind::DoWhile:
    Out.push_back(indent(Depth) + "do {");
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, Out, Depth + 1,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Out, Depth + 1,
                             /*InLoop=*/true);
      }
    }
    Out.push_back(indent(Depth) + "} while (" +
                  todoCondition(conditionText(Payloads, *Node)) + ");");
    break;
  case StructuredNodeKind::InfiniteLoop:
    Out.push_back(indent(Depth) + "while (true) {");
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, Out, Depth + 1,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Out, Depth + 1,
                             /*InLoop=*/true);
      }
    }
    Out.push_back(indent(Depth) + "}");
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
  std::vector<std::string> Payloads;
  class SolidityPayloadProvider : public LLVMFunctionCFGBuilder::PayloadProvider {
  public:
    explicit SolidityPayloadProvider(std::vector<std::string> &Payloads)
        : Payloads(Payloads) {}

    void collectStatements(const llvm::BasicBlock &BB,
                           std::vector<PayloadRef> &Out) override {
      for (const llvm::Instruction &I : BB) {
        if (std::optional<std::string> Kind =
                BodyBuilder::getStringMetadata(I, "notdec.solidity.revert")) {
          Out.push_back(addPayload(
              Payloads, BodyBuilder::formatRevertStatement(I, *Kind)));
          continue;
        }
        if (std::optional<std::string> Kind =
                BodyBuilder::getStringMetadata(I, "notdec.solidity.event")) {
          Out.push_back(addPayload(
              Payloads, BodyBuilder::formatEventStatement(I, *Kind)));
        }
      }
    }

    PayloadRef getCondition(const llvm::Value &V,
                            llvm::StringRef FallbackName) override {
      return addPayload(Payloads, llvmValueName(V, FallbackName));
    }

    PayloadRef getSwitchCase(const llvm::ConstantInt &V) override {
      llvm::SmallString<32> Text;
      V.getValue().toString(Text, 10, /*isSigned=*/false);
      return addPayload(Payloads, Text.str().str());
    }

    PayloadRef getPhiAssignment(const llvm::PHINode &Phi,
                                const llvm::Value &IncomingValue,
                                llvm::StringRef PhiName,
                                llvm::StringRef IncomingName) override {
      (void)Phi;
      (void)IncomingValue;
      return addPayload(Payloads,
                        (PhiName + " = " + IncomingName + ";").str());
    }

  private:
    std::vector<std::string> &Payloads;
  };

  SolidityPayloadProvider Provider(Payloads);
  StructuredCFG Cfg = LLVMFunctionCFGBuilder::build(F, Provider);
  std::map<VVarId, std::string> DephicationVVarNames;
  for (const structuring::DephicationVVar &VVar : Cfg.dephicationVVars()) {
    DephicationVVarNames.emplace(VVar.Id, VVar.Name);
  }

  // The Solidity payload store is string-based. Copies still use shared
  // dephication context; the backend only rewrites concrete payload text.
  Cfg.setPayloadMaterializeHook(
      [&Payloads, &DephicationVVarNames](const PayloadMaterializeContext &Context,
                  PayloadMaterializeKind, PayloadRef Payload,
                  std::size_t) -> std::optional<PayloadRef> {
        if (!Payload.isValid()) {
          return Payload;
        }
        std::string Text = Payloads[Payload.Id];
        std::vector<std::pair<std::string, std::string>> Copies;
        for (const auto &Copy : Context.DephicationVVarCopies) {
          auto SourceIt = DephicationVVarNames.find(Copy.first);
          if (SourceIt == DephicationVVarNames.end()) {
            continue;
          }
          Copies.push_back(
              {SourceIt->second, copiedVVarName(SourceIt->second, Copy.second)});
        }
        Text = BodyBuilder::rewriteCopiedDephicationVVars(std::move(Text),
                                                          Copies);
        Payloads.push_back(std::move(Text));
        return PayloadRef{Payloads.size() - 1};
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  std::unique_ptr<structuring::Structurer> Structurer =
      structuring::createStructurer(structuring::DefaultStructurerName);
  StructuredTree Tree = Structurer->structure(Cfg);
  std::vector<std::string> Result = renderStructuredBody(Tree, Payloads);

  Result.push_back(Payloads.empty() ? "// TODO: recover body"
                                    : "// TODO: recover remaining body");
  return Result;
}

std::string BodyBuilder::rewriteCopiedDephicationVVars(
    std::string Text,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  for (const auto &Copy : Copies) {
    Text = replaceIdentifier(std::move(Text), Copy.first, Copy.second);
  }
  return Text;
}

std::vector<std::string>
BodyBuilder::renderStructuredBody(const structuring::StructuredTree &Tree,
                                  const std::vector<std::string> &Payloads) {
  std::vector<std::string> Result;
  if (Tree.root() != InvalidNodeId) {
    renderStructuredNode(Tree, Payloads, Tree.root(), Result);
  }

  if (Result.empty()) {
    Result.push_back("/* TODO: recover body */");
  }
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
  if (Kind == "error_string") {
    if (std::optional<std::string> Literal = getStringMetadata(
            I, "notdec.solidity_revert.error_string_literal")) {
      return "require(false, " + solidityStringLiteral(*Literal) + ");";
    }
  }

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
