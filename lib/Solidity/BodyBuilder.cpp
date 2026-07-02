#include "notdec-backends/Solidity/BodyBuilder.h"
#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"

#include <cctype>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
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

PayloadRef addPayload(std::vector<BodyBuilder::Payload> &Payloads,
                      BodyBuilder::Payload Payload) {
  Payloads.push_back(std::move(Payload));
  return PayloadRef{Payloads.size() - 1};
}

ExprPtr makeExpr(ExpressionNode Node) {
  return std::make_shared<Expression>(Expression{std::move(Node)});
}

BlockPtr makeBlock(Block BlockValue) {
  return std::make_shared<Block>(std::move(BlockValue));
}

Statement makeStmt(StatementNode Node) { return Statement{std::move(Node)}; }

Statement commentStmt(std::string Text) {
  return makeStmt(CommentStatement{std::move(Text)});
}

bool isIdentifierChar(char C) {
  return std::isalnum(static_cast<unsigned char>(C)) || C == '_';
}

std::string copiedVVarName(llvm::StringRef SourceName, VVarId Copy) {
  return (SourceName + "_copy" + std::to_string(Copy)).str();
}

std::string replaceIdentifierText(
    std::string Text,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  for (const auto &Copy : Copies) {
    llvm::StringRef From = Copy.first;
    llvm::StringRef To = Copy.second;
    if (From.empty() || From == To) {
      continue;
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
  }
  return Text;
}

ExprPtr rewriteExpr(const ExprPtr &Expr,
                    const std::vector<std::pair<std::string, std::string>> &Copies);
Statement rewriteStatement(
    const Statement &Stmt,
    const std::vector<std::pair<std::string, std::string>> &Copies);

NamedArgument rewriteNamedArgument(
    const NamedArgument &Arg,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  return NamedArgument{Arg.Name, rewriteExpr(Arg.Value, Copies)};
}

std::vector<ExprPtr> rewriteExprs(
    const std::vector<ExprPtr> &Exprs,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  std::vector<ExprPtr> Result;
  Result.reserve(Exprs.size());
  for (const ExprPtr &Expr : Exprs) {
    Result.push_back(rewriteExpr(Expr, Copies));
  }
  return Result;
}

std::vector<NamedArgument> rewriteNamedArguments(
    const std::vector<NamedArgument> &Args,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  std::vector<NamedArgument> Result;
  Result.reserve(Args.size());
  for (const NamedArgument &Arg : Args) {
    Result.push_back(rewriteNamedArgument(Arg, Copies));
  }
  return Result;
}

Expression rewriteExpression(
    const Expression &Expr,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  return std::visit([&](const auto &Node) -> Expression {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, IdentifierExpr>) {
      std::string Name = Node.Name;
      for (const auto &Copy : Copies) {
        if (Name == Copy.first) {
          Name = Copy.second;
          break;
        }
      }
      return Expression{IdentifierExpr{std::move(Name)}};
    } else if constexpr (std::is_same_v<T, TodoConditionExpr>) {
      return Expression{TodoConditionExpr{
          replaceIdentifierText(Node.Text, Copies)}};
    } else if constexpr (std::is_same_v<T, MemberAccessExpr>) {
      return Expression{MemberAccessExpr{rewriteExpr(Node.Base, Copies),
                                         Node.Member}};
    } else if constexpr (std::is_same_v<T, IndexAccessExpr>) {
      return Expression{IndexAccessExpr{rewriteExpr(Node.Base, Copies),
                                        rewriteExpr(Node.Index, Copies)}};
    } else if constexpr (std::is_same_v<T, IndexRangeAccessExpr>) {
      return Expression{IndexRangeAccessExpr{rewriteExpr(Node.Base, Copies),
                                             rewriteExpr(Node.Start, Copies),
                                             rewriteExpr(Node.End, Copies)}};
    } else if constexpr (std::is_same_v<T, FunctionCallOptionsExpr>) {
      return Expression{FunctionCallOptionsExpr{
          rewriteExpr(Node.Callee, Copies),
          rewriteNamedArguments(Node.Options, Copies)}};
    } else if constexpr (std::is_same_v<T, UnaryExpr>) {
      return Expression{UnaryExpr{Node.Operator,
                                  rewriteExpr(Node.Operand, Copies),
                                  Node.Prefix, Node.Precedence}};
    } else if constexpr (std::is_same_v<T, BinaryExpr>) {
      return Expression{BinaryExpr{rewriteExpr(Node.Left, Copies),
                                   Node.Operator,
                                   rewriteExpr(Node.Right, Copies),
                                   Node.Precedence,
                                   Node.ParenthesizeLeftOnEqual,
                                   Node.ParenthesizeRightOnEqual}};
    } else if constexpr (std::is_same_v<T, ConditionalExpr>) {
      return Expression{ConditionalExpr{rewriteExpr(Node.Condition, Copies),
                                        rewriteExpr(Node.TrueValue, Copies),
                                        rewriteExpr(Node.FalseValue, Copies)}};
    } else if constexpr (std::is_same_v<T, AssignmentExpr>) {
      return Expression{AssignmentExpr{rewriteExpr(Node.Left, Copies),
                                       Node.Operator,
                                       rewriteExpr(Node.Right, Copies)}};
    } else if constexpr (std::is_same_v<T, CallExpr>) {
      return Expression{CallExpr{rewriteExpr(Node.Callee, Copies),
                                 rewriteExprs(Node.Arguments, Copies),
                                 rewriteNamedArguments(Node.NamedArguments,
                                                       Copies)}};
    } else if constexpr (std::is_same_v<T, TupleExpr>) {
      return Expression{TupleExpr{rewriteExprs(Node.Elements, Copies)}};
    } else if constexpr (std::is_same_v<T, InlineArrayExpr>) {
      return Expression{InlineArrayExpr{rewriteExprs(Node.Elements, Copies)}};
    } else {
      return Expr;
    }
  }, Expr.Node);
}

ExprPtr rewriteExpr(
    const ExprPtr &Expr,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  if (!Expr) {
    return nullptr;
  }
  return std::make_shared<Expression>(rewriteExpression(*Expr, Copies));
}

BlockPtr rewriteBlockPtr(
    const BlockPtr &Body,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  if (!Body) {
    return nullptr;
  }
  Block Result;
  Result.Statements.reserve(Body->Statements.size());
  for (const Statement &Stmt : Body->Statements) {
    Result.Statements.push_back(rewriteStatement(Stmt, Copies));
  }
  return makeBlock(std::move(Result));
}

StmtPtr rewriteStmtPtr(
    const StmtPtr &Stmt,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  if (!Stmt) {
    return nullptr;
  }
  return std::make_shared<Statement>(rewriteStatement(*Stmt, Copies));
}

Statement rewriteStatement(
    const Statement &Stmt,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  return std::visit([&](const auto &Node) -> Statement {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, CommentStatement>) {
      return Statement{CommentStatement{
          replaceIdentifierText(Node.Text, Copies)}};
    } else if constexpr (std::is_same_v<T, ReturnStatement>) {
      return makeStmt(ReturnStatement{rewriteExpr(Node.Value, Copies)});
    } else if constexpr (std::is_same_v<T, ExpressionStatement>) {
      return makeStmt(ExpressionStatement{rewriteExpr(Node.Value, Copies)});
    } else if constexpr (std::is_same_v<T, VariableDeclarationStatement>) {
      return makeStmt(VariableDeclarationStatement{
          Node.Variables, rewriteExpr(Node.InitialValue, Copies)});
    } else if constexpr (std::is_same_v<T, RevertStatement>) {
      return makeStmt(RevertStatement{
          rewriteExpr(Node.Error, Copies), rewriteExprs(Node.Arguments, Copies),
          Node.Comment});
    } else if constexpr (std::is_same_v<T, RequireStatement>) {
      return makeStmt(RequireStatement{
          rewriteExpr(Node.Condition, Copies),
          rewriteExprs(Node.Arguments, Copies)});
    } else if constexpr (std::is_same_v<T, EmitStatement>) {
      return makeStmt(EmitStatement{
          rewriteExpr(Node.Event, Copies), rewriteExprs(Node.Arguments, Copies),
          Node.Comment});
    } else if constexpr (std::is_same_v<T, IfStatement>) {
      return makeStmt(IfStatement{
          rewriteExpr(Node.Condition, Copies), rewriteBlockPtr(Node.Then, Copies),
          rewriteBlockPtr(Node.Else, Copies)});
    } else if constexpr (std::is_same_v<T, ForStatement>) {
      return makeStmt(ForStatement{
          rewriteStmtPtr(Node.Init, Copies), rewriteExpr(Node.Condition, Copies),
          rewriteExpr(Node.Loop, Copies), rewriteStmtPtr(Node.Body, Copies)});
    } else if constexpr (std::is_same_v<T, WhileStatement>) {
      return makeStmt(WhileStatement{
          rewriteExpr(Node.Condition, Copies), rewriteBlockPtr(Node.Body, Copies)});
    } else if constexpr (std::is_same_v<T, DoWhileStatement>) {
      return makeStmt(DoWhileStatement{
          rewriteStmtPtr(Node.Body, Copies),
          rewriteExpr(Node.Condition, Copies)});
    } else if constexpr (std::is_same_v<T, TryStatement>) {
      TryStatement Result = Node;
      Result.ExternalCall = rewriteExpr(Node.ExternalCall, Copies);
      Result.Body = rewriteBlockPtr(Node.Body, Copies);
      for (TryCatchClause &Catch : Result.Catches) {
        Catch.Body = rewriteBlockPtr(Catch.Body, Copies);
      }
      return makeStmt(std::move(Result));
    } else if constexpr (std::is_same_v<T, UncheckedBlockStatement>) {
      return makeStmt(UncheckedBlockStatement{
          rewriteBlockPtr(Node.Body, Copies)});
    } else if constexpr (std::is_same_v<T, BlockStatement>) {
      return makeStmt(BlockStatement{rewriteBlockPtr(Node.Body, Copies)});
    } else {
      return Statement{Node};
    }
  }, Stmt.Node);
}

BodyBuilder::Payload replaceIdentifiers(
    const BodyBuilder::Payload &Payload,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  return std::visit([&](const auto &Node) -> BodyBuilder::Payload {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, Expression>) {
      return rewriteExpression(Node, Copies);
    } else {
      return rewriteStatement(Node, Copies);
    }
  }, Payload);
}

std::string exprDebugText(const ExprPtr &Expr);

std::string expressionDebugText(const Expression &Expr) {
  return std::visit([](const auto &Node) -> std::string {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, IdentifierExpr>) {
      return Node.Name;
    } else if constexpr (std::is_same_v<T, TypeNameExpr>) {
      return Node.Type.Name;
    } else if constexpr (std::is_same_v<T, LiteralExpr>) {
      if (Node.SubDenomination.empty()) {
        return Node.Text;
      }
      return Node.Text + " " + Node.SubDenomination;
    } else if constexpr (std::is_same_v<T, TodoConditionExpr>) {
      return Node.Text;
    } else if constexpr (std::is_same_v<T, MemberAccessExpr>) {
      return exprDebugText(Node.Base) + "." + Node.Member;
    } else if constexpr (std::is_same_v<T, IndexAccessExpr>) {
      return exprDebugText(Node.Base) + "[" + exprDebugText(Node.Index) + "]";
    } else if constexpr (std::is_same_v<T, IndexRangeAccessExpr>) {
      return exprDebugText(Node.Base) + "[" + exprDebugText(Node.Start) + ":" +
             exprDebugText(Node.End) + "]";
    } else if constexpr (std::is_same_v<T, FunctionCallOptionsExpr>) {
      return exprDebugText(Node.Callee) + "{...}";
    } else if constexpr (std::is_same_v<T, UnaryExpr>) {
      if (Node.Prefix) {
        return Node.Operator + exprDebugText(Node.Operand);
      }
      return exprDebugText(Node.Operand) + Node.Operator;
    } else if constexpr (std::is_same_v<T, BinaryExpr>) {
      return exprDebugText(Node.Left) + " " + Node.Operator + " " +
             exprDebugText(Node.Right);
    } else if constexpr (std::is_same_v<T, ConditionalExpr>) {
      return exprDebugText(Node.Condition) + " ? " +
             exprDebugText(Node.TrueValue) + " : " +
             exprDebugText(Node.FalseValue);
    } else if constexpr (std::is_same_v<T, AssignmentExpr>) {
      return exprDebugText(Node.Left) + " " + Node.Operator + " " +
             exprDebugText(Node.Right);
    } else if constexpr (std::is_same_v<T, CallExpr>) {
      return exprDebugText(Node.Callee) + "(...)";
    } else if constexpr (std::is_same_v<T, NewExpr>) {
      return "new " + Node.Type.Name;
    } else if constexpr (std::is_same_v<T, MetaTypeExpr>) {
      return "type(" + Node.Type.Name + ")";
    } else {
      return "expr";
    }
  }, Expr.Node);
}

std::string exprDebugText(const ExprPtr &Expr) {
  if (!Expr) {
    return "expr";
  }
  return expressionDebugText(*Expr);
}

std::string payloadDebugText(const std::vector<BodyBuilder::Payload> &Payloads,
                             PayloadRef Ref) {
  if (!Ref.isValid() || Ref.Id >= Payloads.size()) {
    return "unknown";
  }
  const BodyBuilder::Payload &Payload = Payloads[Ref.Id];
  return std::visit([](const auto &Node) -> std::string {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, Expression>) {
      return expressionDebugText(Node);
    } else {
      if (const auto *Comment = std::get_if<CommentStatement>(&Node.Node)) {
        return Comment->Text;
      }
      return "stmt";
    }
  }, Payload);
}

ExprPtr conditionExpr(const std::vector<BodyBuilder::Payload> &Payloads,
                      const StructuredNode &Node) {
  std::string Text = payloadDebugText(Payloads, Node.Condition);
  if (Node.ConditionNegated) {
    Text = "!(" + Text + ")";
  }
  std::string Comment = Text;
  std::string::size_type Pos = 0;
  while ((Pos = Comment.find("*/", Pos)) != std::string::npos) {
    Comment.replace(Pos, 2, "* /");
    Pos += 3;
  }
  return makeExpr(TodoConditionExpr{Comment});
}

const Statement *payloadStatement(const std::vector<BodyBuilder::Payload> &Payloads,
                                  PayloadRef Ref) {
  if (!Ref.isValid() || Ref.Id >= Payloads.size()) {
    return nullptr;
  }
  return std::get_if<Statement>(&Payloads[Ref.Id]);
}

bool isTerminalStatement(const Statement &Stmt) {
  return std::visit([](const auto &Node) -> bool {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, ReturnStatement> ||
                  std::is_same_v<T, RevertStatement> ||
                  std::is_same_v<T, RequireStatement>) {
      return true;
    } else if constexpr (std::is_same_v<T, BlockStatement>) {
      return Node.Body != nullptr && !Node.Body->Statements.empty() &&
             isTerminalStatement(Node.Body->Statements.back());
    } else if constexpr (std::is_same_v<T, IfStatement>) {
      return Node.Then != nullptr && Node.Else != nullptr &&
             !Node.Then->Statements.empty() && !Node.Else->Statements.empty() &&
             isTerminalStatement(Node.Then->Statements.back()) &&
             isTerminalStatement(Node.Else->Statements.back());
    } else {
      return false;
    }
  }, Stmt.Node);
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
                          const std::vector<BodyBuilder::Payload> &Payloads,
                          structuring::NodeId Id,
                          Block &Out,
                          bool InLoop = false) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return;
  }

  switch (Node->Kind) {
  case StructuredNodeKind::Sequence:
    for (structuring::NodeId Child : Node->Children) {
      renderStructuredNode(Tree, Payloads, Child, Out, InLoop);
    }
    break;
  case StructuredNodeKind::Label:
    Out.Statements.push_back(commentStmt("// block_" +
                                         std::to_string(Node->Block) + ":"));
    break;
  case StructuredNodeKind::BasicBlock:
    for (PayloadRef Ref : Node->Statements) {
      if (const Statement *Stmt = payloadStatement(Payloads, Ref)) {
        Out.Statements.push_back(*Stmt);
      }
    }
    break;
  case StructuredNodeKind::If:
  {
    IfStatement If;
    If.Condition = conditionExpr(Payloads, *Node);
    If.Then = makeBlock(Block{});
    if (Node->Children.empty() &&
        (Node->Then != InvalidNodeId || Node->Else != InvalidNodeId)) {
      renderStructuredNode(Tree, Payloads, Node->Then, *If.Then, InLoop);
      if (Node->Else != InvalidNodeId) {
        If.Else = makeBlock(Block{});
        renderStructuredNode(Tree, Payloads, Node->Else, *If.Else, InLoop);
      }
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, *If.Then, InLoop);
      }
    }
    Out.Statements.push_back(makeStmt(std::move(If)));
    break;
  }
  case StructuredNodeKind::Switch:
  {
    Block Inner;
    Inner.Statements.push_back(commentStmt("/* TODO: switch " +
                                           payloadDebugText(Payloads, Node->Condition) +
                                           " */"));
    if (Node->Children.empty() &&
        (!Node->StructuredCases.empty() || Node->Default != InvalidNodeId)) {
      for (const auto &Case : Node->StructuredCases) {
        Inner.Statements.push_back(commentStmt("/* case " +
                                               payloadDebugText(Payloads, Case.Value) +
                                               " */"));
        renderStructuredNode(Tree, Payloads, Case.Body, Inner, InLoop);
      }
      if (Node->Default != InvalidNodeId) {
        Inner.Statements.push_back(commentStmt("/* default */"));
        renderStructuredNode(Tree, Payloads, Node->Default, Inner, InLoop);
      }
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Inner, InLoop);
      }
    }
    Out.Statements.push_back(makeStmt(BlockStatement{makeBlock(std::move(Inner))}));
    break;
  }
  case StructuredNodeKind::Goto:
    Out.Statements.push_back(commentStmt("// goto block_" +
                                         std::to_string(Node->Target)));
    break;
  case StructuredNodeKind::Return:
    break;
  case StructuredNodeKind::Unreachable:
    break;
  case StructuredNodeKind::Break:
    if (InLoop) {
      Out.Statements.push_back(makeStmt(BreakStatement{}));
    }
    break;
  case StructuredNodeKind::Continue:
    if (InLoop) {
      Out.Statements.push_back(makeStmt(ContinueStatement{}));
    }
    break;
  case StructuredNodeKind::While:
  {
    WhileStatement While;
    While.Condition = conditionExpr(Payloads, *Node);
    While.Body = makeBlock(Block{});
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, *While.Body,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, *While.Body,
                             /*InLoop=*/true);
      }
    }
    Out.Statements.push_back(makeStmt(std::move(While)));
    break;
  }
  case StructuredNodeKind::DoWhile:
  {
    DoWhileStatement DoWhile;
    Block Body;
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, Body,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, Body,
                             /*InLoop=*/true);
      }
    }
    DoWhile.Body = std::make_shared<Statement>(
        makeStmt(BlockStatement{makeBlock(std::move(Body))}));
    DoWhile.Condition = conditionExpr(Payloads, *Node);
    Out.Statements.push_back(makeStmt(std::move(DoWhile)));
    break;
  }
  case StructuredNodeKind::InfiniteLoop:
  {
    WhileStatement While;
    While.Condition = makeExpr(LiteralExpr{"true", ""});
    While.Body = makeBlock(Block{});
    if (Node->Body != InvalidNodeId) {
      renderStructuredNode(Tree, Payloads, Node->Body, *While.Body,
                           /*InLoop=*/true);
    } else {
      for (structuring::NodeId Child : Node->Children) {
        renderStructuredNode(Tree, Payloads, Child, *While.Body,
                             /*InLoop=*/true);
      }
    }
    Out.Statements.push_back(makeStmt(std::move(While)));
    break;
  }
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

std::optional<ExprPtr> evmEnvBuiltinExpr(llvm::StringRef Name) {
  if (Name == "evm_basefee") {
    return makeExpr(MemberAccessExpr{makeExpr(IdentifierExpr{"block"}),
                                     "basefee"});
  }
  if (Name == "evm_blobbasefee") {
    return makeExpr(MemberAccessExpr{makeExpr(IdentifierExpr{"block"}),
                                     "blobbasefee"});
  }
  if (Name == "evm_gas") {
    return makeExpr(CallExpr{makeExpr(IdentifierExpr{"gasleft"}), {}, {}});
  }
  if (Name == "evm_caller") {
    return makeExpr(MemberAccessExpr{makeExpr(IdentifierExpr{"msg"}),
                                     "sender"});
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

ExprPtr valueExpr(const llvm::Value &V, llvm::StringRef FallbackName = "ret0");

ExprPtr makeBinaryExpr(ExprPtr Left, llvm::StringRef Operator, ExprPtr Right,
                       unsigned Precedence) {
  return makeExpr(BinaryExpr{std::move(Left), Operator.str(), std::move(Right),
                             Precedence,
                             leftOperandNeedsSamePrecedenceParentheses(Operator),
                             rightOperandNeedsSamePrecedenceParentheses(Operator)});
}

ExprPtr makeZeroCompareExpr(const llvm::Value &V, llvm::StringRef Operator) {
  return makeBinaryExpr(valueExpr(V), Operator, makeExpr(LiteralExpr{"0", ""}),
                        3);
}

std::vector<ExprPtr> callArgExprs(const llvm::CallBase &Call) {
  std::vector<ExprPtr> Args;
  Args.reserve(Call.arg_size());
  for (const llvm::Use &Arg : Call.args()) {
    Args.push_back(valueExpr(*Arg.get()));
  }
  return Args;
}

ExprPtr valueExpr(const llvm::Value &V, llvm::StringRef FallbackName) {
  if (std::optional<llvm::APInt> Int = constantIntValue(&V)) {
    return makeExpr(LiteralExpr{formatInteger(*Int), ""});
  }
  if (const auto *Cast = llvm::dyn_cast<llvm::ZExtInst>(&V);
      Cast != nullptr && Cast->getSrcTy()->isIntegerTy(1)) {
    return valueExpr(*Cast->getOperand(0), FallbackName);
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
        unsigned LogicalPrecedence = Cmp->getPredicate() ==
                                             llvm::CmpInst::ICMP_EQ
                                         ? 2
                                         : 1;
        llvm::StringRef CompareOp =
            Cmp->getPredicate() == llvm::CmpInst::ICMP_EQ ? "==" : "!=";
        llvm::StringRef LogicalOp =
            Cmp->getPredicate() == llvm::CmpInst::ICMP_EQ ? "&&" : "||";
        return makeBinaryExpr(makeZeroCompareExpr(*Or->getOperand(0), CompareOp),
                              LogicalOp,
                              makeZeroCompareExpr(*Or->getOperand(1), CompareOp),
                              LogicalPrecedence);
      }
    }
    if (std::optional<llvm::StringRef> Operator =
            icmpPredicateText(Cmp->getPredicate())) {
      unsigned Precedence = icmpPredicatePrecedence(Cmp->getPredicate());
      return makeExpr(BinaryExpr{valueExpr(*Cmp->getOperand(0)),
                                 Operator->str(),
                                 valueExpr(*Cmp->getOperand(1)), Precedence,
                                 false, true});
    }
  }
  if (const auto *Op = llvm::dyn_cast<llvm::BinaryOperator>(&V)) {
    if (const llvm::Value *Operand = bitwiseNotOperand(*Op)) {
      return makeExpr(UnaryExpr{"~", valueExpr(*Operand), true, 30});
    }
    if (std::optional<llvm::StringRef> Operator =
            logicalOperatorText(Op->getOpcode());
        Op->getType()->isIntegerTy(1) && Operator.has_value()) {
      // LLVM keeps pure bool connectives as i1 and/or.
      unsigned Precedence = *logicalOperatorPrecedence(Op->getOpcode());
      return makeBinaryExpr(valueExpr(*Op->getOperand(0)), *Operator,
                            valueExpr(*Op->getOperand(1)), Precedence);
    }
    if (std::optional<llvm::StringRef> Operator =
            binaryOperatorText(Op->getOpcode())) {
      unsigned Precedence = *binaryOperatorPrecedence(Op->getOpcode());
      return makeBinaryExpr(valueExpr(*Op->getOperand(0)), *Operator,
                            valueExpr(*Op->getOperand(1)), Precedence);
    }
  }
  if (const auto *Call = llvm::dyn_cast<llvm::CallBase>(&V)) {
    const llvm::Function *Callee = Call->getCalledFunction();
    if (Callee != nullptr && Call->arg_size() == 1) {
      if (std::optional<ExprPtr> Builtin =
              evmEnvBuiltinExpr(Callee->getName())) {
        return *Builtin;
      }
    }
    if (Callee != nullptr && Call->arg_size() == 3) {
      if (std::optional<llvm::StringRef> Builtin =
              evmTernaryBuiltinName(Callee->getName())) {
        return makeExpr(CallExpr{makeExpr(IdentifierExpr{Builtin->str()}),
                                 callArgExprs(*Call), {}});
      }
    }
    if (Callee != nullptr && Call->arg_size() == 2) {
      if (Callee->getName() == "evm_signextend") {
        if (std::optional<std::string> Type =
                evmSignExtendType(Call->getArgOperand(0))) {
          return makeExpr(CallExpr{makeExpr(IdentifierExpr{std::move(*Type)}),
                                   {valueExpr(*Call->getArgOperand(1))}, {}});
        }
      }
      if (std::optional<llvm::StringRef> Operator =
              evmShiftOperatorText(Callee->getName())) {
        constexpr unsigned Precedence = 8;
        return makeBinaryExpr(valueExpr(*Call->getArgOperand(1)), *Operator,
                              valueExpr(*Call->getArgOperand(0)), Precedence);
      }
      if (std::optional<llvm::StringRef> Operator =
              evmBinaryOperatorText(Callee->getName())) {
        unsigned Precedence = evmBinaryOperatorPrecedence(Callee->getName());
        return makeBinaryExpr(valueExpr(*Call->getArgOperand(0)), *Operator,
                              valueExpr(*Call->getArgOperand(1)), Precedence);
      }
    }
  }
  return makeExpr(IdentifierExpr{llvmValueName(V, FallbackName)});
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

std::optional<Statement> formatSingleWordReturn(const llvm::CallBase &Call) {
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
      return makeStmt(ReturnStatement{valueExpr(*Stored)});
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
    return makeStmt(ReturnStatement{valueExpr(*Store->getValueOperand())});
  }

  return std::nullopt;
}

} // namespace

Block BodyBuilder::readBody(const llvm::Function &F) {
  std::vector<Payload> Payloads;
  class SolidityPayloadProvider : public LLVMFunctionCFGBuilder::PayloadProvider {
  public:
    explicit SolidityPayloadProvider(std::vector<Payload> &Payloads)
        : Payloads(Payloads) {}

    void collectStatements(const llvm::BasicBlock &BB,
                           std::vector<PayloadRef> &Out) override {
      for (const llvm::Instruction &I : BB) {
        if (const auto *Ret = llvm::dyn_cast<llvm::ReturnInst>(&I)) {
          if (const llvm::Value *Value = Ret->getReturnValue()) {
            Out.push_back(addPayload(Payloads,
                                      makeStmt(ReturnStatement{
                                          valueExpr(*Value, "ret")})));
          }
          continue;
        }
        if (const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I)) {
          if (std::optional<Statement> Return =
                  formatSingleWordReturn(*Call)) {
            Out.push_back(addPayload(Payloads, std::move(*Return)));
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
      return addPayload(
          Payloads,
          Expression{TodoConditionExpr{llvmValueName(V, FallbackName)}});
    }

    PayloadRef getSwitchCase(const llvm::ConstantInt &V) override {
      llvm::SmallString<32> Text;
      V.getValue().toString(Text, 10, /*isSigned=*/false);
      return addPayload(Payloads, Expression{LiteralExpr{Text.str().str(), ""}});
    }

    PayloadRef getPhiAssignment(const llvm::PHINode &Phi,
                                const llvm::Value &IncomingValue,
                                llvm::StringRef PhiName,
                                llvm::StringRef IncomingName) override {
      (void)Phi;
      (void)IncomingValue;
      return addPayload(
          Payloads,
          makeStmt(ExpressionStatement{
              makeExpr(AssignmentExpr{
                  makeExpr(IdentifierExpr{PhiName.str()}), "=",
                  makeExpr(IdentifierExpr{IncomingName.str()})})}));
    }

  private:
    std::vector<Payload> &Payloads;
  };

  SolidityPayloadProvider Provider(Payloads);
  StructuredCFG Cfg = LLVMFunctionCFGBuilder::build(F, Provider);
  std::map<VVarId, std::string> DephicationVVarNames;
  for (const structuring::DephicationVVar &VVar : Cfg.dephicationVVars()) {
    DephicationVVarNames.emplace(VVar.Id, VVar.Name);
  }

  Cfg.setPayloadMaterializeHook(
      [&Payloads, &DephicationVVarNames](const PayloadMaterializeContext &Context,
                  PayloadMaterializeKind, PayloadRef Payload,
                  std::size_t) -> std::optional<PayloadRef> {
        if (!Payload.isValid()) {
          return Payload;
        }
        std::vector<std::pair<std::string, std::string>> Copies;
        for (const auto &Copy : Context.DephicationVVarCopies) {
          auto SourceIt = DephicationVVarNames.find(Copy.first);
          if (SourceIt == DephicationVVarNames.end()) {
            continue;
          }
          Copies.push_back(
              {SourceIt->second, copiedVVarName(SourceIt->second, Copy.second)});
        }
        Payloads.push_back(
            BodyBuilder::rewriteCopiedDephicationVVars(Payloads[Payload.Id],
                                                       Copies));
        return PayloadRef{Payloads.size() - 1};
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  std::unique_ptr<structuring::Structurer> Structurer =
      structuring::createStructurer(structuring::DefaultStructurerName);
  StructuredTree Tree = Structurer->structure(Cfg);
  Block Result = renderStructuredBody(Tree, Payloads);

  if (Payloads.empty()) {
    return Result;
  }
  if (!Result.Statements.empty() &&
      !isTerminalStatement(Result.Statements.back())) {
    Result.Statements.push_back(commentStmt("// TODO: recover remaining body"));
  }
  return Result;
}

BodyBuilder::Payload BodyBuilder::rewriteCopiedDephicationVVars(
    const Payload &Payload,
    const std::vector<std::pair<std::string, std::string>> &Copies) {
  return replaceIdentifiers(Payload, Copies);
}

Block
BodyBuilder::renderStructuredBody(const structuring::StructuredTree &Tree,
                                  const std::vector<Payload> &Payloads) {
  Block Result;
  if (Tree.root() != InvalidNodeId) {
    renderStructuredNode(Tree, Payloads, Tree.root(), Result);
  }

  if (Result.Statements.empty()) {
    Result.Statements.push_back(commentStmt("/* TODO: recover body */"));
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

std::vector<ExprPtr>
BodyBuilder::getEventTopicArguments(const llvm::Instruction &I) {
  const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I);
  if (Call == nullptr || !Call->getCalledFunction() ||
      !Call->getCalledFunction()->getName().starts_with("evm_log") ||
      Call->arg_size() <= 4) {
    return {};
  }

  std::vector<ExprPtr> Args;
  for (unsigned Arg = 4; Arg < Call->arg_size(); ++Arg) {
    Args.push_back(valueExpr(*Call->getArgOperand(Arg)));
  }
  return Args;
}

Statement BodyBuilder::formatRevertStatement(const llvm::Instruction &I,
                                             llvm::StringRef Kind) {
  if (Kind == "error_string") {
    if (std::optional<std::string> Literal = getStringMetadata(
            I, "notdec.solidity_revert.error_string_literal")) {
      return makeStmt(RequireStatement{
          makeExpr(LiteralExpr{"false", ""}),
          {makeExpr(LiteralExpr{solidityStringLiteral(*Literal), ""})}});
    }
  }

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

  return makeStmt(RevertStatement{nullptr, {}, std::move(Comment)});
}

Statement BodyBuilder::formatEventStatement(const llvm::Instruction &I,
                                            llvm::StringRef Kind) {
  std::string Name =
      getEventName(I, Kind).value_or(sanitizeIdentifier(("Event_" + Kind).str()));
  return makeStmt(EmitStatement{
      makeExpr(IdentifierExpr{std::move(Name)}), getEventTopicArguments(I),
      "TODO: recover event signature"});
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
