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

// This is only a print-time guard for statements the Solidity backend already
// emitted as unconditional exits. It should not guess structured control flow.
bool isTerminalStatement(llvm::StringRef Text) {
  Text = Text.trim();
  return Text == "return;" || Text.starts_with("return ") ||
         Text.starts_with("revert();") ||
         Text.starts_with("require(false,");
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

std::optional<llvm::APInt> constantIntValue(const llvm::Value *V) {
  if (const auto *Int = llvm::dyn_cast_or_null<llvm::ConstantInt>(V)) {
    return Int->getValue();
  }
  return std::nullopt;
}

bool isAllOnesConstant(const llvm::Value *V) {
  std::optional<llvm::APInt> Int = constantIntValue(V);
  return Int.has_value() && Int->isAllOnes();
}

bool isZeroConstant(const llvm::Value *V) {
  std::optional<llvm::APInt> Int = constantIntValue(V);
  return Int.has_value() && Int->isZero();
}

std::optional<llvm::APInt> constantIntToPtrValue(const llvm::Value *V) {
  if (const auto *Inst = llvm::dyn_cast_or_null<llvm::IntToPtrInst>(V)) {
    return constantIntValue(Inst->getOperand(0));
  }
  if (const auto *Expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(V);
      Expr != nullptr && Expr->getOpcode() == llvm::Instruction::IntToPtr &&
      Expr->getNumOperands() == 1) {
    return constantIntValue(Expr->getOperand(0));
  }
  return std::nullopt;
}

std::optional<std::string> evmEnvBuiltinName(llvm::StringRef Name) {
  if (Name == "evm_basefee") {
    return "block.basefee";
  }
  if (Name == "evm_blobbasefee") {
    return "block.blobbasefee";
  }
  if (Name == "evm_caller") {
    return "msg.sender";
  }
  return std::nullopt;
}

std::string formatInteger(const llvm::APInt &Value) {
  llvm::SmallString<64> Text;
  Value.toString(Text, 10, /*isSigned=*/false);
  return Text.str().str();
}

std::optional<llvm::StringRef> binaryOperatorText(unsigned Opcode) {
  switch (Opcode) {
  case llvm::Instruction::Add:
    return "+";
  case llvm::Instruction::Sub:
    return "-";
  case llvm::Instruction::Mul:
    return "*";
  case llvm::Instruction::And:
    return "&";
  case llvm::Instruction::Xor:
    return "^";
  case llvm::Instruction::Or:
    return "|";
  default:
    return std::nullopt;
  }
}

std::optional<llvm::StringRef> logicalOperatorText(unsigned Opcode) {
  switch (Opcode) {
  case llvm::Instruction::And:
    return "&&";
  case llvm::Instruction::Or:
    return "||";
  default:
    return std::nullopt;
  }
}

std::optional<llvm::StringRef>
icmpPredicateText(llvm::CmpInst::Predicate Predicate) {
  switch (Predicate) {
  case llvm::CmpInst::ICMP_EQ:
    return "==";
  case llvm::CmpInst::ICMP_NE:
    return "!=";
  case llvm::CmpInst::ICMP_ULT:
  case llvm::CmpInst::ICMP_SLT:
    return "<";
  case llvm::CmpInst::ICMP_ULE:
  case llvm::CmpInst::ICMP_SLE:
    return "<=";
  case llvm::CmpInst::ICMP_UGT:
  case llvm::CmpInst::ICMP_SGT:
    return ">";
  case llvm::CmpInst::ICMP_UGE:
  case llvm::CmpInst::ICMP_SGE:
    return ">=";
  default:
    return std::nullopt;
  }
}

unsigned icmpPredicatePrecedence(llvm::CmpInst::Predicate Predicate) {
  switch (Predicate) {
  case llvm::CmpInst::ICMP_EQ:
  case llvm::CmpInst::ICMP_NE:
    return 3;
  default:
    return 4;
  }
}

std::optional<unsigned> logicalOperatorPrecedence(unsigned Opcode) {
  switch (Opcode) {
  case llvm::Instruction::Or:
    return 1;
  case llvm::Instruction::And:
    return 2;
  default:
    return std::nullopt;
  }
}

std::optional<unsigned> binaryOperatorPrecedence(unsigned Opcode) {
  switch (Opcode) {
  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
    return 10;
  case llvm::Instruction::Mul:
    return 20;
  case llvm::Instruction::And:
    return 7;
  case llvm::Instruction::Xor:
    return 6;
  case llvm::Instruction::Or:
    return 5;
  default:
    return std::nullopt;
  }
}

std::optional<llvm::StringRef> evmBinaryOperatorText(llvm::StringRef Name) {
  if (Name == "evm_div") {
    return "/";
  }
  if (Name == "evm_sdiv") {
    return "/";
  }
  if (Name == "evm_mod") {
    return "%";
  }
  if (Name == "evm_smod") {
    return "%";
  }
  if (Name == "evm_exp") {
    return "**";
  }
  return std::nullopt;
}

unsigned evmBinaryOperatorPrecedence(llvm::StringRef Name) {
  if (Name == "evm_exp") {
    return 25;
  }
  return 20;
}

std::optional<llvm::StringRef> evmShiftOperatorText(llvm::StringRef Name) {
  if (Name == "evm_shl") {
    return "<<";
  }
  if (Name == "evm_shr") {
    return ">>";
  }
  if (Name == "evm_sar") {
    return ">>";
  }
  return std::nullopt;
}

std::optional<std::string> evmSignExtendType(const llvm::Value *ByteIndex) {
  std::optional<llvm::APInt> Index = constantIntValue(ByteIndex);
  if (!Index.has_value() || Index->ugt(31)) {
    return std::nullopt;
  }
  return "int" + std::to_string((Index->getZExtValue() + 1) * 8);
}

std::optional<llvm::StringRef> evmTernaryBuiltinName(llvm::StringRef Name) {
  if (Name == "evm_addmod") {
    return "addmod";
  }
  if (Name == "evm_mulmod") {
    return "mulmod";
  }
  return std::nullopt;
}

const llvm::Value *bitwiseNotOperand(const llvm::BinaryOperator &Op) {
  if (Op.getOpcode() != llvm::Instruction::Xor) {
    return nullptr;
  }
  if (isAllOnesConstant(Op.getOperand(0))) {
    return Op.getOperand(1);
  }
  if (isAllOnesConstant(Op.getOperand(1))) {
    return Op.getOperand(0);
  }
  return nullptr;
}

bool rightOperandNeedsSamePrecedenceParentheses(llvm::StringRef Operator) {
  return Operator == "-" || Operator == "*" || Operator == "/" ||
         Operator == "%" ||
         Operator == "<<" || Operator == ">>";
}

bool leftOperandNeedsSamePrecedenceParentheses(llvm::StringRef Operator) {
  return Operator == "**";
}

bool needsParentheses(unsigned ChildPrecedence, unsigned ParentPrecedence,
                      bool ParenthesizeSamePrecedenceOperand) {
  if (ChildPrecedence < ParentPrecedence) {
    return true;
  }
  return ParenthesizeSamePrecedenceOperand &&
         ChildPrecedence == ParentPrecedence;
}

std::string formatReturnValue(const llvm::Value &V,
                              unsigned ParentPrecedence = 0,
                              bool IsRightOperand = false,
                              bool ParenthesizeSamePrecedenceOperand = false) {
  if (std::optional<llvm::APInt> Int = constantIntValue(&V)) {
    return formatInteger(*Int);
  }
  if (const auto *Cast = llvm::dyn_cast<llvm::ZExtInst>(&V);
      Cast != nullptr && Cast->getSrcTy()->isIntegerTy(1)) {
    return formatReturnValue(*Cast->getOperand(0), ParentPrecedence,
                             IsRightOperand,
                             ParenthesizeSamePrecedenceOperand);
  }
  if (const auto *Cmp = llvm::dyn_cast<llvm::ICmpInst>(&V)) {
    if (Cmp->getPredicate() == llvm::CmpInst::ICMP_EQ ||
        Cmp->getPredicate() == llvm::CmpInst::ICMP_NE) {
      // EVM zero checks around OR represent all-zero / any-nonzero bools.
      const llvm::Value *Compared = nullptr;
      if (isZeroConstant(Cmp->getOperand(0))) {
        Compared = Cmp->getOperand(1);
      } else if (isZeroConstant(Cmp->getOperand(1))) {
        Compared = Cmp->getOperand(0);
      }
      const auto *Or = llvm::dyn_cast_or_null<llvm::BinaryOperator>(Compared);
      if (Or != nullptr && Or->getOpcode() == llvm::Instruction::Or) {
        constexpr unsigned EqualityPrecedence = 3;
        unsigned LogicalPrecedence = Cmp->getPredicate() ==
                                             llvm::CmpInst::ICMP_EQ
                                         ? 2
                                         : 1;
        llvm::StringRef CompareOp =
            Cmp->getPredicate() == llvm::CmpInst::ICMP_EQ ? "==" : "!=";
        llvm::StringRef LogicalOp =
            Cmp->getPredicate() == llvm::CmpInst::ICMP_EQ ? "&&" : "||";
        std::string Text = formatReturnValue(*Or->getOperand(0),
                                             EqualityPrecedence) +
                           " " + CompareOp.str() + " 0 " +
                           LogicalOp.str() + " " +
                           formatReturnValue(*Or->getOperand(1),
                                             EqualityPrecedence) +
                           " " + CompareOp.str() + " 0";
        if (needsParentheses(LogicalPrecedence, ParentPrecedence,
                             ParenthesizeSamePrecedenceOperand)) {
          return "(" + Text + ")";
        }
        return Text;
      }
    }
    if (std::optional<llvm::StringRef> Operator =
            icmpPredicateText(Cmp->getPredicate())) {
      unsigned Precedence = icmpPredicatePrecedence(Cmp->getPredicate());
      std::string Text =
          formatReturnValue(*Cmp->getOperand(0), Precedence) + " " +
          Operator->str() + " " +
          formatReturnValue(*Cmp->getOperand(1), Precedence,
                            /*IsRightOperand=*/true,
                            /*ParenthesizeSamePrecedenceOperand=*/true);
      if (needsParentheses(Precedence, ParentPrecedence,
                           ParenthesizeSamePrecedenceOperand)) {
        return "(" + Text + ")";
      }
      return Text;
    }
  }
  if (const auto *Op = llvm::dyn_cast<llvm::BinaryOperator>(&V)) {
    if (const llvm::Value *Operand = bitwiseNotOperand(*Op)) {
      constexpr unsigned Precedence = 30;
      std::string Text = "~" + formatReturnValue(*Operand, Precedence);
      if (needsParentheses(Precedence, ParentPrecedence,
                           ParenthesizeSamePrecedenceOperand)) {
        return "(" + Text + ")";
      }
      return Text;
    }
    if (std::optional<llvm::StringRef> Operator =
            logicalOperatorText(Op->getOpcode());
        Op->getType()->isIntegerTy(1) && Operator.has_value()) {
      // LLVM keeps pure bool connectives as i1 and/or.
      unsigned Precedence = *logicalOperatorPrecedence(Op->getOpcode());
      std::string Text = formatReturnValue(*Op->getOperand(0), Precedence,
                                           /*IsRightOperand=*/false,
                                           leftOperandNeedsSamePrecedenceParentheses(
                                               *Operator)) +
                         " " + Operator->str() + " " +
                         formatReturnValue(*Op->getOperand(1), Precedence,
                                           /*IsRightOperand=*/true,
                                           rightOperandNeedsSamePrecedenceParentheses(
                                               *Operator));
      if (needsParentheses(Precedence, ParentPrecedence,
                           ParenthesizeSamePrecedenceOperand)) {
        return "(" + Text + ")";
      }
      return Text;
    }
    if (std::optional<llvm::StringRef> Operator =
            binaryOperatorText(Op->getOpcode())) {
      unsigned Precedence = *binaryOperatorPrecedence(Op->getOpcode());
      std::string Text = formatReturnValue(*Op->getOperand(0), Precedence,
                                           /*IsRightOperand=*/false,
                                           leftOperandNeedsSamePrecedenceParentheses(
                                               *Operator)) +
                         " " + Operator->str() + " " +
                         formatReturnValue(*Op->getOperand(1), Precedence,
                                           /*IsRightOperand=*/true,
                                           rightOperandNeedsSamePrecedenceParentheses(
                                               *Operator));
      if (needsParentheses(Precedence, ParentPrecedence,
                           ParenthesizeSamePrecedenceOperand)) {
        return "(" + Text + ")";
      }
      return Text;
    }
  }
  if (const auto *Call = llvm::dyn_cast<llvm::CallBase>(&V)) {
    const llvm::Function *Callee = Call->getCalledFunction();
    if (Callee != nullptr && Call->arg_size() == 1) {
      if (std::optional<std::string> Builtin =
              evmEnvBuiltinName(Callee->getName())) {
        return *Builtin;
      }
    }
    if (Callee != nullptr && Call->arg_size() == 3) {
      if (std::optional<llvm::StringRef> Builtin =
              evmTernaryBuiltinName(Callee->getName())) {
        constexpr unsigned Precedence = 30;
        std::string Text =
            Builtin->str() + "(" +
            formatReturnValue(*Call->getArgOperand(0)) + ", " +
            formatReturnValue(*Call->getArgOperand(1)) + ", " +
            formatReturnValue(*Call->getArgOperand(2)) + ")";
        if (needsParentheses(Precedence, ParentPrecedence,
                             ParenthesizeSamePrecedenceOperand)) {
          return "(" + Text + ")";
        }
        return Text;
      }
    }
    if (Callee != nullptr && Call->arg_size() == 2) {
      if (Callee->getName() == "evm_signextend") {
        if (std::optional<std::string> Type =
                evmSignExtendType(Call->getArgOperand(0))) {
          constexpr unsigned Precedence = 30;
          std::string Text =
              *Type + "(" + formatReturnValue(*Call->getArgOperand(1)) + ")";
          if (needsParentheses(Precedence, ParentPrecedence,
                               ParenthesizeSamePrecedenceOperand)) {
            return "(" + Text + ")";
          }
          return Text;
        }
      }
      if (std::optional<llvm::StringRef> Operator =
              evmShiftOperatorText(Callee->getName())) {
        constexpr unsigned Precedence = 8;
        std::string Text = formatReturnValue(*Call->getArgOperand(1),
                                             Precedence,
                                             /*IsRightOperand=*/false,
                                             leftOperandNeedsSamePrecedenceParentheses(
                                                 *Operator)) +
                           " " + Operator->str() + " " +
                           formatReturnValue(*Call->getArgOperand(0),
                                             Precedence,
                                             /*IsRightOperand=*/true,
                                             rightOperandNeedsSamePrecedenceParentheses(
                                                 *Operator));
        if (needsParentheses(Precedence, ParentPrecedence,
                             ParenthesizeSamePrecedenceOperand)) {
          return "(" + Text + ")";
        }
        return Text;
      }
      if (std::optional<llvm::StringRef> Operator =
              evmBinaryOperatorText(Callee->getName())) {
        unsigned Precedence = evmBinaryOperatorPrecedence(Callee->getName());
        std::string Text = formatReturnValue(*Call->getArgOperand(0),
                                             Precedence,
                                             /*IsRightOperand=*/false,
                                             leftOperandNeedsSamePrecedenceParentheses(
                                                 *Operator)) +
                           " " + Operator->str() + " " +
                           formatReturnValue(*Call->getArgOperand(1),
                                             Precedence,
                                             /*IsRightOperand=*/true,
                                             rightOperandNeedsSamePrecedenceParentheses(
                                                 *Operator));
        if (needsParentheses(Precedence, ParentPrecedence,
                             ParenthesizeSamePrecedenceOperand)) {
          return "(" + Text + ")";
        }
        return Text;
      }
    }
  }
  return llvmValueName(V, "ret0");
}

const llvm::Value *ptrToIntPointerValue(const llvm::Value *V) {
  if (const auto *Inst = llvm::dyn_cast_or_null<llvm::PtrToIntInst>(V)) {
    return Inst->getOperand(0);
  }
  if (const auto *Expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(V);
      Expr != nullptr && Expr->getOpcode() == llvm::Instruction::PtrToInt &&
      Expr->getNumOperands() == 1) {
    return Expr->getOperand(0);
  }
  return nullptr;
}

const llvm::Value *findStoredValueBeforeReturn(const llvm::CallBase &Call,
                                               const llvm::Value *StorePointer) {
  if (StorePointer == nullptr) {
    return nullptr;
  }
  for (auto It = llvm::BasicBlock::const_iterator(&Call), Begin =
                                                      Call.getParent()->begin();
       It != Begin;) {
    --It;
    const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&*It);
    if (Store != nullptr && Store->getPointerOperand() == StorePointer) {
      return Store->getValueOperand();
    }
  }
  return nullptr;
}

const llvm::Value *findAllocatedSingleWordReturnValue(const llvm::CallBase &Call) {
  // MemoryBufferAnalysis rewrites dynamic ABI buffers to calloc-backed
  // pointers before Solidity printing.  For one-word returns, the returned
  // pointer and preceding store identify the high-level return expression.
  const llvm::Value *StorePointer = ptrToIntPointerValue(Call.getArgOperand(1));
  return findStoredValueBeforeReturn(Call, StorePointer);
}

std::optional<std::string> formatSingleWordReturn(const llvm::CallBase &Call) {
  const llvm::Function *Callee = Call.getCalledFunction();
  if (Callee == nullptr || Callee->getName() != "evm_return" ||
      Call.arg_size() < 3) {
    return std::nullopt;
  }
  std::optional<llvm::APInt> ReturnOffset =
      constantIntValue(Call.getArgOperand(1));
  std::optional<llvm::APInt> ReturnLength =
      constantIntValue(Call.getArgOperand(2));
  if (!ReturnLength.has_value()) {
    return std::nullopt;
  }
  if (*ReturnLength != 32) {
    return std::nullopt;
  }
  if (!ReturnOffset.has_value()) {
    if (const llvm::Value *Stored = findAllocatedSingleWordReturnValue(Call)) {
      return "return " + formatReturnValue(*Stored) + ";";
    }
    return std::nullopt;
  }

  // Solidity ABI returns a single static word by storing it in memory and
  // returning that 32-byte range. Keep this first rule local to the same block.
  for (auto It = llvm::BasicBlock::const_iterator(&Call), Begin =
                                                      Call.getParent()->begin();
       It != Begin;) {
    --It;
    const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&*It);
    if (Store == nullptr) {
      continue;
    }
    std::optional<llvm::APInt> StoreOffset =
        constantIntToPtrValue(Store->getPointerOperand());
    if (!StoreOffset.has_value() || *StoreOffset != *ReturnOffset) {
      continue;
    }
    return "return " + formatReturnValue(*Store->getValueOperand()) + ";";
  }

  return std::nullopt;
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
        if (const auto *Ret = llvm::dyn_cast<llvm::ReturnInst>(&I)) {
          if (const llvm::Value *Value = Ret->getReturnValue()) {
            Out.push_back(addPayload(
                Payloads, "return " + llvmValueName(*Value, "ret") + ";"));
          }
          continue;
        }
        if (const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I)) {
          if (std::optional<std::string> Return =
                  formatSingleWordReturn(*Call)) {
            Out.push_back(addPayload(Payloads, *Return));
            continue;
          }
        }
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

  if (Payloads.empty()) {
    return Result;
  }
  if (!Result.empty() && !isTerminalStatement(Result.back())) {
    Result.push_back("// TODO: recover remaining body");
  }
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

std::vector<std::string>
BodyBuilder::getEventTopicArguments(const llvm::Instruction &I) {
  const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I);
  if (Call == nullptr || !Call->getCalledFunction() ||
      !Call->getCalledFunction()->getName().starts_with("evm_log") ||
      Call->arg_size() <= 4) {
    return {};
  }

  std::vector<std::string> Args;
  for (unsigned Arg = 4; Arg < Call->arg_size(); ++Arg) {
    Args.push_back(formatReturnValue(*Call->getArgOperand(Arg)));
  }
  return Args;
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
  std::vector<std::string> Args = getEventTopicArguments(I);
  std::string Text = "emit " + Name + "(";
  for (std::size_t Index = 0; Index < Args.size(); ++Index) {
    if (Index != 0) {
      Text += ", ";
    }
    Text += Args[Index];
  }
  return Text + "); // TODO: recover event signature";
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
