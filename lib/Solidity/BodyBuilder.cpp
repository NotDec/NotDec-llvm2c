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
using structuring::CFGBlock;
using structuring::InvalidNodeId;
using structuring::LLVMFunctionCFGBuilder;
using structuring::PayloadMaterializeContext;
using structuring::PayloadMaterializeKind;
using structuring::PayloadRef;
using structuring::StructuredCFG;
using structuring::StructuredNode;
using structuring::StructuredNodeKind;
using structuring::StructuredTree;
using structuring::TerminatorKind;
using structuring::VVarId;

// Active contract-level slot map while one Solidity function body is being
// built.  The Solidity backend currently reads/writes one module at a time;
// thread_local keeps parallel backend invocations from sharing slot state.
thread_local const StorageSlotMap *ActiveStorageSlots = nullptr;
thread_local const std::vector<std::string> *ActiveArgumentNames = nullptr;
thread_local const ParameterTypeMap *ActiveParameterTypes = nullptr;
thread_local const EventParamTypeMap *ActiveEventParamTypes = nullptr;

class ActiveStorageSlotsScope {
public:
  ActiveStorageSlotsScope(const StorageSlotMap *StorageSlots,
                          const std::vector<std::string> *ArgumentNames,
                          const ParameterTypeMap *ParameterTypes,
                          const EventParamTypeMap *EventParamTypes)
      : PreviousStorageSlots(ActiveStorageSlots),
        PreviousArgumentNames(ActiveArgumentNames),
        PreviousParameterTypes(ActiveParameterTypes),
        PreviousEventParamTypes(ActiveEventParamTypes) {
    ActiveStorageSlots = StorageSlots;
    ActiveArgumentNames = ArgumentNames;
    ActiveParameterTypes = ParameterTypes;
    ActiveEventParamTypes = EventParamTypes;
  }

  ~ActiveStorageSlotsScope() {
    ActiveStorageSlots = PreviousStorageSlots;
    ActiveArgumentNames = PreviousArgumentNames;
    ActiveParameterTypes = PreviousParameterTypes;
    ActiveEventParamTypes = PreviousEventParamTypes;
  }

private:
  const StorageSlotMap *PreviousStorageSlots;
  const std::vector<std::string> *PreviousArgumentNames;
  const ParameterTypeMap *PreviousParameterTypes;
  const EventParamTypeMap *PreviousEventParamTypes;
};

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
    } else if constexpr (std::is_same_v<T, UnresolvedValueExpr>) {
      return Expression{UnresolvedValueExpr{
          replaceIdentifierText(Node.Text, Copies)}};
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
    } else if constexpr (std::is_same_v<T, UnresolvedValueExpr>) {
      return "0 /* TODO: unresolved value: " + Node.Text + " */";
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

const Expression *payloadExpression(
    const std::vector<BodyBuilder::Payload> &Payloads, PayloadRef Ref) {
  if (!Ref.isValid() || Ref.Id >= Payloads.size()) {
    return nullptr;
  }
  return std::get_if<Expression>(&Payloads[Ref.Id]);
}

ExprPtr conditionExpr(const std::vector<BodyBuilder::Payload> &Payloads,
                      const StructuredNode &Node) {
  if (const Expression *Condition =
          payloadExpression(Payloads, Node.Condition)) {
    ExprPtr Result = std::make_shared<Expression>(*Condition);
    if (Node.ConditionNegated) {
      Result = makeExpr(
          UnaryExpr{"!", std::move(Result), /*Prefix=*/true, 30});
    }
    return Result;
  }

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

// The trailing "recover remaining body" marker means the rendered statements
// may not cover the whole function.  Control reaches the exit when the last
// rendered block's CFG terminator is Return/Unreachable on every path, and the
// structurer emits a Goto node for branches it could not structure.  Use those
// two facts instead of the last printed statement, because a recovered
// assignment is not terminal even though the implicit Solidity return after it
// is.
struct BodyCompletion {
  bool HasGoto = false;
  bool ReachesExit = false;
};

BodyCompletion analyzeBodyCompletion(const StructuredCFG &Cfg,
                                     const StructuredTree &Tree,
                                     structuring::NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return {};
  }
  switch (Node->Kind) {
  case StructuredNodeKind::Goto:
    return {true, false};
  case StructuredNodeKind::Return:
  case StructuredNodeKind::Unreachable:
    return {false, true};
  case StructuredNodeKind::BasicBlock: {
    // The Phoenix structurer materializes explicit Return/Unreachable nodes
    // only on some paths, so read terminality from the CFG terminator of the
    // block this node renders.  appendBlockBody() may render a synthetic
    // block's merged body, hence the getBodyBlock() fallback.
    auto IsExitBlock = [&](const CFGBlock *Block) {
      return Block != nullptr &&
             (Block->Terminator == TerminatorKind::Return ||
              Block->Terminator == TerminatorKind::Unreachable);
    };
    bool ReachesExit = IsExitBlock(Cfg.getBlock(Node->Block)) ||
                       IsExitBlock(Cfg.getBodyBlock(Node->Block));
    return {false, ReachesExit};
  }
  case StructuredNodeKind::Sequence: {
    BodyCompletion Result;
    for (structuring::NodeId Child : Node->Children) {
      Result.HasGoto |= analyzeBodyCompletion(Cfg, Tree, Child).HasGoto;
    }
    if (!Node->Children.empty()) {
      Result.ReachesExit =
          analyzeBodyCompletion(Cfg, Tree, Node->Children.back()).ReachesExit;
    }
    return Result;
  }
  case StructuredNodeKind::If: {
    BodyCompletion Then = analyzeBodyCompletion(Cfg, Tree, Node->Then);
    BodyCompletion Else = analyzeBodyCompletion(Cfg, Tree, Node->Else);
    return {Then.HasGoto || Else.HasGoto, Then.ReachesExit && Else.ReachesExit};
  }
  case StructuredNodeKind::Switch: {
    BodyCompletion Result;
    bool AllReachExit = Node->Default != InvalidNodeId;
    for (const structuring::StructuredSwitchCase &Case :
         Node->StructuredCases) {
      BodyCompletion CaseResult = analyzeBodyCompletion(Cfg, Tree, Case.Body);
      Result.HasGoto |= CaseResult.HasGoto;
      AllReachExit &= CaseResult.ReachesExit;
    }
    if (Node->Default != InvalidNodeId) {
      BodyCompletion Default =
          analyzeBodyCompletion(Cfg, Tree, Node->Default);
      Result.HasGoto |= Default.HasGoto;
      AllReachExit &= Default.ReachesExit;
    }
    Result.ReachesExit = AllReachExit;
    return Result;
  }
  case StructuredNodeKind::While:
  case StructuredNodeKind::DoWhile:
  case StructuredNodeKind::InfiniteLoop: {
    BodyCompletion Result;
    if (Node->Body != InvalidNodeId) {
      Result.HasGoto = analyzeBodyCompletion(Cfg, Tree, Node->Body).HasGoto;
    }
    for (structuring::NodeId Child : Node->Children) {
      Result.HasGoto |= analyzeBodyCompletion(Cfg, Tree, Child).HasGoto;
    }
    // A loop only reaches the exit through a break; stay conservative here.
    return Result;
  }
  default:
    return {};
  }
}

// Solidity lowers require(cond) to a conditional revert(0, 0) guard in front of
// the body.  The revert pass marks a plain empty revert with kind "empty" and a
// recovered Error(string) as require(false, "..."); both can be rebuilt from
// the negated guard condition.  Panic, custom error and returndata-bubble
// reverts keep their explicit if because their payload cannot be reproduced by
// a negated condition.
const Statement *soleNonCommentStatement(const BlockPtr &Branch) {
  if (Branch == nullptr) {
    return nullptr;
  }
  const Statement *Only = nullptr;
  for (const Statement &Stmt : Branch->Statements) {
    if (std::holds_alternative<CommentStatement>(Stmt.Node)) {
      continue;
    }
    if (Only != nullptr) {
      return nullptr;
    }
    Only = &Stmt;
  }
  return Only;
}

std::optional<llvm::StringRef> invertedComparisonOperator(llvm::StringRef Op) {
  if (Op == "==") {
    return "!=";
  }
  if (Op == "!=") {
    return "==";
  }
  if (Op == "<") {
    return ">=";
  }
  if (Op == "<=") {
    return ">";
  }
  if (Op == ">") {
    return "<=";
  }
  if (Op == ">=") {
    return "<";
  }
  return std::nullopt;
}

// Negate a guard condition for require(): comparisons flip to their inverse so
// the common "if (a < b) revert" reads back as "require(a >= b)".  Other
// conditions fall back to a parenthesized "!".
ExprPtr negateConditionExpr(const ExprPtr &Condition) {
  if (!Condition) {
    return makeExpr(UnaryExpr{"!", Condition, /*Prefix=*/true, 30});
  }
  if (const auto *Unary = std::get_if<UnaryExpr>(&Condition->Node)) {
    if (Unary->Prefix && Unary->Operator == "!" && Unary->Operand) {
      return Unary->Operand;
    }
  }
  if (const auto *Binary = std::get_if<BinaryExpr>(&Condition->Node)) {
    if (std::optional<llvm::StringRef> Inverted =
            invertedComparisonOperator(Binary->Operator)) {
      return makeExpr(BinaryExpr{Binary->Left, Inverted->str(), Binary->Right,
                                 Binary->Precedence,
                                 Binary->ParenthesizeLeftOnEqual,
                                 Binary->ParenthesizeRightOnEqual});
    }
  }
  return makeExpr(UnaryExpr{"!", Condition, /*Prefix=*/true, 30});
}

std::optional<RequireStatement>
guardRevertRequire(const BlockPtr &Branch, const ExprPtr &Condition) {
  const Statement *Only = soleNonCommentStatement(Branch);
  if (Only == nullptr) {
    return std::nullopt;
  }
  if (const auto *Req = std::get_if<RequireStatement>(&Only->Node)) {
    const auto *Literal =
        Req->Condition == nullptr
            ? nullptr
            : std::get_if<LiteralExpr>(&Req->Condition->Node);
    if (Literal == nullptr || Literal->Text != "false") {
      return std::nullopt;
    }
    return RequireStatement{negateConditionExpr(Condition), Req->Arguments};
  }
  const auto *Rev = std::get_if<RevertStatement>(&Only->Node);
  if (Rev == nullptr || Rev->Error != nullptr || !Rev->Arguments.empty() ||
      Rev->Comment != "empty") {
    return std::nullopt;
  }
  return RequireStatement{negateConditionExpr(Condition), {}};
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

    // "if (cond) { revert(); } else { body }" is the recovered form of
    // "require(!cond); body".  Fold it when exactly one branch is a pure guard
    // revert, so the guard reads like the original Solidity instead of an if
    // with an empty continuation branch.
    std::optional<RequireStatement> ThenGuard =
        guardRevertRequire(If.Then, If.Condition);
    std::optional<RequireStatement> ElseGuard =
        guardRevertRequire(If.Else, If.Condition);
    if (ThenGuard.has_value() && !ElseGuard.has_value()) {
      Out.Statements.push_back(makeStmt(std::move(*ThenGuard)));
      if (If.Else != nullptr) {
        for (const Statement &Stmt : If.Else->Statements) {
          Out.Statements.push_back(Stmt);
        }
      }
      break;
    }
    if (ElseGuard.has_value() && !ThenGuard.has_value()) {
      Out.Statements.push_back(makeStmt(std::move(*ElseGuard)));
      for (const Statement &Stmt : If.Then->Statements) {
        Out.Statements.push_back(Stmt);
      }
      break;
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

// Raw LLVM value name for diagnostics/TODO comments.  This intentionally keeps
// characters such as '.' so the comment still points at the original IR value.
std::string llvmValueDebugName(const llvm::Value &V, llvm::StringRef Prefix) {
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
  if (Name == "evm_calldatasize") {
    return makeExpr(MemberAccessExpr{
        makeExpr(MemberAccessExpr{makeExpr(IdentifierExpr{"msg"}), "data"}),
        "length"});
  }
  return std::nullopt;
}

std::string formatInteger(const llvm::APInt &Value) {
  // A one-bit value is a Solidity bool.  Emitting 0/1 would make recovered
  // conditions such as "if (0)" or "require(1)" fail to compile.
  if (Value.getBitWidth() == 1) {
    return Value.isZero() ? "false" : "true";
  }
  // Solidity decimal literals cannot express i256 values with the sign bit
  // set (Gigahorse emits masks such as i256 -1 / -256 directly).  Emit them
  // as uint256 arithmetic around type(uint256).max instead of an oversized
  // decimal literal.
  if (Value.getBitWidth() >= 256 && Value.isNegative()) {
    llvm::APInt Complement = ~Value;
    if (Complement.isZero()) {
      return "type(uint256).max";
    }
    llvm::SmallString<64> ComplementText;
    Complement.toString(ComplementText, 10, /*isSigned=*/false);
    return "(type(uint256).max - " + ComplementText.str().str() + ")";
  }
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
ExprPtr wordCastAddressExpr(ExprPtr Expr);

const StorageSlotInfo *storageSlotInfoForValue(const llvm::Value &V) {
  if (ActiveStorageSlots == nullptr) {
    return nullptr;
  }
  const auto *Slot = llvm::dyn_cast<llvm::ConstantInt>(&V);
  if (Slot == nullptr || Slot->getValue().getActiveBits() > 64) {
    return nullptr;
  }
  auto It = ActiveStorageSlots->find(Slot->getZExtValue());
  return It == ActiveStorageSlots->end() ? nullptr : &It->second;
}

const StorageSlotInfo *storageSlotInfoForRef(const llvm::Value &V) {
  if (const StorageSlotInfo *Info = storageSlotInfoForValue(V)) {
    return Info;
  }
  const auto *Call = llvm::dyn_cast<llvm::CallBase>(&V);
  const llvm::Function *Callee =
      Call == nullptr ? nullptr : Call->getCalledFunction();
  if (Callee != nullptr && Callee->getName() == "evm.storage.slot" &&
      Call->arg_size() >= 1) {
    return storageSlotInfoForValue(*Call->getArgOperand(0));
  }
  return nullptr;
}

struct StorageContainerShape {
  bool IsMapping = false;
  bool IsArray = false;
};

StorageContainerShape storageContainerShapeForRef(const llvm::Value &V) {
  const auto *Call = llvm::dyn_cast<llvm::CallBase>(&V);
  const llvm::Function *Callee =
      Call == nullptr ? nullptr : Call->getCalledFunction();
  if (Callee == nullptr) {
    return {};
  }
  llvm::StringRef Name = Callee->getName();
  if (Name == "evm.storage.slot" && Call->arg_size() >= 1) {
    if (const StorageSlotInfo *Info =
            storageSlotInfoForValue(*Call->getArgOperand(0))) {
      return {Info->IsMapping, Info->IsArray};
    }
    return {};
  }
  // Nested map.value / array.elem results are not necessarily indexable by the
  // declaration we emitted (mapping values are often flattened to uint256 by
  // TypePrinter).  Only a direct slot carries reliable container shape.
  return {};
}

std::optional<ExprPtr> storageRefExpr(const llvm::Value &V);

std::optional<ExprPtr> storageHelperExpr(const llvm::CallBase &Call) {
  const llvm::Function *Callee = Call.getCalledFunction();
  if (Callee == nullptr) {
    return std::nullopt;
  }
  llvm::StringRef Name = Callee->getName();

  if ((Name == "evm.storage.slot" || Name == "evm_sload") &&
      Call.arg_size() >= 1) {
    if (const StorageSlotInfo *Info =
            storageSlotInfoForValue(*Call.getArgOperand(0))) {
      return makeExpr(IdentifierExpr{Info->Name});
    }
    return std::nullopt;
  }

  if ((Name == "evm.storage.load" || Name == "evm.storage.packed.load" ||
       Name == "evm.storage.field") &&
      Call.arg_size() >= 1) {
    std::optional<ExprPtr> Base = storageRefExpr(*Call.getArgOperand(0));
    if (!Base.has_value()) {
      return std::nullopt;
    }
    // A plain storage load on a dynamic-array slot reads its length word.
    if (Name == "evm.storage.load" &&
        storageContainerShapeForRef(*Call.getArgOperand(0)).IsArray) {
      return makeExpr(MemberAccessExpr{*Base, "length"});
    }
    return *Base;
  }

  if (Name == "evm.storage.map.value" && Call.arg_size() >= 2) {
    std::optional<ExprPtr> Base = storageRefExpr(*Call.getArgOperand(0));
    if (!Base.has_value()) {
      return std::nullopt;
    }
    if (storageContainerShapeForRef(*Call.getArgOperand(0)).IsMapping) {
      // State-variable types recovered by TypePrinter are always integer or
      // array keyed, so an address-valued index (msg.sender / an address ABI
      // parameter) needs the explicit uint256(uint160(...)) conversion.
      return makeExpr(IndexAccessExpr{
          *Base, wordCastAddressExpr(valueExpr(*Call.getArgOperand(1)))});
    }
    // The declared slot type is not known to be indexable; keep a readable
    // base expression instead of emitting an undeclared SSA name.
    return *Base;
  }

  if (Name == "evm.storage.dynamic_array.elem" ||
      Name == "evm.storage.static_array.elem") {
    if (Call.arg_size() < 2) {
      return std::nullopt;
    }
    std::optional<ExprPtr> Base = storageRefExpr(*Call.getArgOperand(0));
    if (!Base.has_value()) {
      return std::nullopt;
    }
    if (storageContainerShapeForRef(*Call.getArgOperand(0)).IsArray) {
      return makeExpr(IndexAccessExpr{
          *Base, wordCastAddressExpr(valueExpr(*Call.getArgOperand(1)))});
    }
    return *Base;
  }

  if (Name == "evm.storage.dynamic_array.length.load" &&
      Call.arg_size() >= 1) {
    std::optional<ExprPtr> Base = storageRefExpr(*Call.getArgOperand(0));
    if (!Base.has_value()) {
      return std::nullopt;
    }
    if (storageContainerShapeForRef(*Call.getArgOperand(0)).IsArray) {
      return makeExpr(MemberAccessExpr{*Base, "length"});
    }
    return *Base;
  }

  if (Name.starts_with("evm.storage.bytes.") && Call.arg_size() >= 1) {
    return storageRefExpr(*Call.getArgOperand(0));
  }

  return std::nullopt;
}

std::optional<ExprPtr> storageRefExpr(const llvm::Value &V) {
  if (const auto *Call = llvm::dyn_cast<llvm::CallBase>(&V)) {
    return storageHelperExpr(*Call);
  }
  return std::nullopt;
}

// Recognize the canonical checked-calldata word load produced by
// EvmCalldataAccessPass:
//   %checked = call ptr @notdec_evm_calldata_min_size(calldata, min_size)
//   %addr    = add (ptrtoint %checked), 4 + 32*i
//   %load    = load i256, inttoptr %addr
// and map it back to the i-th Solidity ABI argument.
std::optional<unsigned>
matchCalldataArgumentIndex(const llvm::Value &V) {
  const auto *Load = llvm::dyn_cast<llvm::LoadInst>(&V);
  const auto *ITP = Load == nullptr
                        ? nullptr
                        : llvm::dyn_cast<llvm::IntToPtrInst>(
                              Load->getPointerOperand());
  const auto *Add = ITP == nullptr
                        ? nullptr
                        : llvm::dyn_cast<llvm::BinaryOperator>(ITP->getOperand(0));
  if (Add == nullptr || Add->getOpcode() != llvm::Instruction::Add) {
    return std::nullopt;
  }
  const auto *Offset = llvm::dyn_cast<llvm::ConstantInt>(Add->getOperand(1));
  const auto *Base = Add->getOperand(0);
  if (Offset == nullptr) {
    Offset = llvm::dyn_cast<llvm::ConstantInt>(Add->getOperand(0));
    Base = Add->getOperand(1);
  }
  const auto *PTI = llvm::dyn_cast<llvm::PtrToIntInst>(Base);
  const auto *Call = PTI == nullptr
                         ? nullptr
                         : llvm::dyn_cast<llvm::CallBase>(PTI->getOperand(0));
  if (Offset == nullptr || Call == nullptr ||
      Call->getCalledFunction() == nullptr ||
      Call->getCalledFunction()->getName() !=
          "notdec_evm_calldata_min_size") {
    return std::nullopt;
  }
  if (Offset->getValue().getActiveBits() > 64) {
    return std::nullopt;
  }
  std::uint64_t ByteOffset = Offset->getZExtValue();
  if (ByteOffset < 4 || (ByteOffset - 4) % 32 != 0) {
    return std::nullopt;
  }
  return static_cast<unsigned>((ByteOffset - 4) / 32);
}

// Metadata written by EvmCalldataAccessPass carries the ABI argument index as
// a decimal MDString.  Keep the parser strict so a malformed annotation falls
// back to the IR shape matcher instead of naming the wrong parameter.
std::optional<unsigned> parseUnsignedDecimal(llvm::StringRef Text) {
  if (Text.empty()) {
    return std::nullopt;
  }
  unsigned Value = 0;
  for (char C : Text) {
    if (C < '0' || C > '9') {
      return std::nullopt;
    }
    Value = Value * 10 + static_cast<unsigned>(C - '0');
    if (Value > 4096) {
      return std::nullopt;
    }
  }
  return Value;
}

// A raw calldata word can only stand in for an ABI parameter whose Solidity
// type is word-like.  bool/bytesN/bytes/string/array parameters need explicit
// lowering (bool -> x ? 1 : 0, bytesN -> uintN(x), ...), so keep the unresolved
// placeholder until that exists.  Address is integer-valued and is already cast
// by wordCastAddressExpr() at arithmetic/comparison/index sites.
bool isWordLikeParameter(llvm::StringRef Name) {
  if (ActiveParameterTypes == nullptr) {
    return true;
  }
  auto It = ActiveParameterTypes->find(Name.str());
  if (It == ActiveParameterTypes->end()) {
    return true;
  }
  llvm::StringRef Type = It->second;
  if (Type == "address") {
    return true;
  }
  if (!Type.starts_with("uint") && !Type.starts_with("int")) {
    return false;
  }
  return Type.find('[') == llvm::StringRef::npos;
}

// Resolve a pass-provided "notdec.solidity.calldata.index" annotation to the
// Solidity parameter identifier.  Shared by direct calldata loads and the
// results of outlined ABI decoder helpers.
std::optional<ExprPtr> annotatedAbiArgumentExpr(const llvm::Instruction &I) {
  std::optional<std::string> ArgumentIndex =
      BodyBuilder::getStringMetadata(I, "notdec.solidity.calldata.index");
  if (!ArgumentIndex.has_value()) {
    return std::nullopt;
  }
  std::optional<unsigned> Parsed = parseUnsignedDecimal(*ArgumentIndex);
  if (!Parsed.has_value()) {
    return std::nullopt;
  }
  constexpr unsigned kRuntimeArgs = 4;
  const unsigned NameIndex = kRuntimeArgs + *Parsed;
  if (ActiveArgumentNames == nullptr ||
      NameIndex >= ActiveArgumentNames->size()) {
    return std::nullopt;
  }
  const std::string &Name = (*ActiveArgumentNames)[NameIndex];
  if (Name.empty() || !isWordLikeParameter(Name)) {
    return std::nullopt;
  }
  return makeExpr(IdentifierExpr{Name});
}

bool isMsgSenderExpr(const ExprPtr &Expr) {
  const auto *Member =
      Expr == nullptr ? nullptr : std::get_if<MemberAccessExpr>(&Expr->Node);
  if (Member == nullptr || Member->Member != "sender" || !Member->Base) {
    return false;
  }
  const auto *Base = std::get_if<IdentifierExpr>(&Member->Base->Node);
  return Base != nullptr && Base->Name == "msg";
}

bool isAddressTypedIdentifier(const ExprPtr &Expr) {
  const auto *Id =
      Expr == nullptr ? nullptr : std::get_if<IdentifierExpr>(&Expr->Node);
  if (Id == nullptr || ActiveParameterTypes == nullptr) {
    return false;
  }
  auto It = ActiveParameterTypes->find(Id->Name);
  return It != ActiveParameterTypes->end() && It->second == "address";
}

ExprPtr wordCastAddressExpr(ExprPtr Expr) {
  if (!isMsgSenderExpr(Expr) && !isAddressTypedIdentifier(Expr)) {
    return Expr;
  }
  return makeExpr(CallExpr{makeExpr(IdentifierExpr{"uint256"}),
                           {makeExpr(CallExpr{
                               makeExpr(IdentifierExpr{"uint160"}),
                               {std::move(Expr)}, {}})},
                           {}});
}

bool isComparisonOperator(llvm::StringRef Operator) {
  return Operator == "==" || Operator == "!=" || Operator == "<" ||
         Operator == "<=" || Operator == ">" || Operator == ">=";
}

bool needsWordOperandCast(llvm::StringRef Operator) {
  // Address-valued expressions such as msg.sender are only valid in
  // comparisons/arithmetic after an explicit uint conversion.  Slot state is
  // currently emitted as uint256, so casting the address side keeps generated
  // comparisons compilable.
  return Operator == "+" || Operator == "-" || Operator == "*" ||
         Operator == "/" || Operator == "%" || Operator == "&" ||
         Operator == "|" || Operator == "^" || Operator == "<<" ||
         Operator == ">>" || Operator == "**" || Operator == "==" ||
         Operator == "!=" || Operator == "<" || Operator == "<=" ||
         Operator == ">" || Operator == ">=";
}

bool isUnresolvedExpr(const ExprPtr &Expr) {
  return Expr != nullptr &&
         std::holds_alternative<UnresolvedValueExpr>(Expr->Node);
}

// Recursively make arithmetic/bitwise operands word-typed.  Solidity's
// arbitrary-precision int_const arithmetic can otherwise turn an EVM mask
// such as `0 - (1 << 160)` into a negative int_const and reject `uint256 &`.
ExprPtr wordifyOperand(ExprPtr Expr) {
  if (Expr == nullptr) {
    return Expr;
  }
  if (auto *Binary = std::get_if<BinaryExpr>(&Expr->Node)) {
    Binary->Left = wordifyOperand(Binary->Left);
    Binary->Right = wordifyOperand(Binary->Right);
    return Expr;
  }
  if (std::holds_alternative<LiteralExpr>(Expr->Node)) {
    return makeExpr(CallExpr{makeExpr(IdentifierExpr{"uint256"}),
                             {std::move(Expr)}, {}});
  }
  return Expr;
}

bool containsUnresolvedValue(const ExprPtr &Expr);

bool expressionContainsUnresolvedValue(const Expression &Expr) {
  return std::visit(
      [](const auto &Node) -> bool {
        using T = std::decay_t<decltype(Node)>;
        if constexpr (std::is_same_v<T, UnresolvedValueExpr> ||
                      std::is_same_v<T, TodoConditionExpr>) {
          return true;
        } else if constexpr (std::is_same_v<T, MemberAccessExpr>) {
          return containsUnresolvedValue(Node.Base);
        } else if constexpr (std::is_same_v<T, IndexAccessExpr>) {
          return containsUnresolvedValue(Node.Base) ||
                 containsUnresolvedValue(Node.Index);
        } else if constexpr (std::is_same_v<T, IndexRangeAccessExpr>) {
          return containsUnresolvedValue(Node.Base) ||
                 containsUnresolvedValue(Node.Start) ||
                 containsUnresolvedValue(Node.End);
        } else if constexpr (std::is_same_v<T, FunctionCallOptionsExpr>) {
          if (containsUnresolvedValue(Node.Callee)) {
            return true;
          }
          for (const NamedArgument &Arg : Node.Options) {
            if (containsUnresolvedValue(Arg.Value)) {
              return true;
            }
          }
          return false;
        } else if constexpr (std::is_same_v<T, UnaryExpr>) {
          return containsUnresolvedValue(Node.Operand);
        } else if constexpr (std::is_same_v<T, BinaryExpr>) {
          return containsUnresolvedValue(Node.Left) ||
                 containsUnresolvedValue(Node.Right);
        } else if constexpr (std::is_same_v<T, ConditionalExpr>) {
          return containsUnresolvedValue(Node.Condition) ||
                 containsUnresolvedValue(Node.TrueValue) ||
                 containsUnresolvedValue(Node.FalseValue);
        } else if constexpr (std::is_same_v<T, AssignmentExpr>) {
          return containsUnresolvedValue(Node.Left) ||
                 containsUnresolvedValue(Node.Right);
        } else if constexpr (std::is_same_v<T, CallExpr>) {
          if (containsUnresolvedValue(Node.Callee)) {
            return true;
          }
          for (const ExprPtr &Arg : Node.Arguments) {
            if (containsUnresolvedValue(Arg)) {
              return true;
            }
          }
          for (const NamedArgument &Arg : Node.NamedArguments) {
            if (containsUnresolvedValue(Arg.Value)) {
              return true;
            }
          }
          return false;
        } else if constexpr (std::is_same_v<T, NewExpr>) {
          return false;
        } else if constexpr (std::is_same_v<T, TupleExpr>) {
          for (const ExprPtr &Element : Node.Elements) {
            if (containsUnresolvedValue(Element)) {
              return true;
            }
          }
          return false;
        } else if constexpr (std::is_same_v<T, InlineArrayExpr>) {
          for (const ExprPtr &Element : Node.Elements) {
            if (containsUnresolvedValue(Element)) {
              return true;
            }
          }
          return false;
        } else {
          return false;
        }
      },
      Expr.Node);
}

bool containsUnresolvedValue(const ExprPtr &Expr) {
  return Expr != nullptr && expressionContainsUnresolvedValue(*Expr);
}

bool containsIdentifierOfType(const ExprPtr &Expr, llvm::StringRef Type) {
  if (Expr == nullptr || ActiveParameterTypes == nullptr) {
    return false;
  }
  if (const auto *Id = std::get_if<IdentifierExpr>(&Expr->Node)) {
    auto It = ActiveParameterTypes->find(Id->Name);
    return It != ActiveParameterTypes->end() && It->second == Type;
  }
  if (const auto *Member = std::get_if<MemberAccessExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Member->Base, Type);
  }
  if (const auto *Index = std::get_if<IndexAccessExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Index->Base, Type) ||
           containsIdentifierOfType(Index->Index, Type);
  }
  if (const auto *Unary = std::get_if<UnaryExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Unary->Operand, Type);
  }
  if (const auto *Binary = std::get_if<BinaryExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Binary->Left, Type) ||
           containsIdentifierOfType(Binary->Right, Type);
  }
  if (const auto *Cond = std::get_if<ConditionalExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Cond->Condition, Type) ||
           containsIdentifierOfType(Cond->TrueValue, Type) ||
           containsIdentifierOfType(Cond->FalseValue, Type);
  }
  if (const auto *Assign = std::get_if<AssignmentExpr>(&Expr->Node)) {
    return containsIdentifierOfType(Assign->Left, Type) ||
           containsIdentifierOfType(Assign->Right, Type);
  }
  if (const auto *Call = std::get_if<CallExpr>(&Expr->Node)) {
    if (containsIdentifierOfType(Call->Callee, Type)) {
      return true;
    }
    for (const ExprPtr &Arg : Call->Arguments) {
      if (containsIdentifierOfType(Arg, Type)) {
        return true;
      }
    }
  }
  return false;
}

ExprPtr makeBinaryExpr(ExprPtr Left, llvm::StringRef Operator, ExprPtr Right,
                       unsigned Precedence) {
  if (needsWordOperandCast(Operator)) {
    if (isComparisonOperator(Operator)) {
      Left = wordCastAddressExpr(std::move(Left));
      Right = wordCastAddressExpr(std::move(Right));
    } else {
      Left = wordCastAddressExpr(wordifyOperand(std::move(Left)));
      Right = wordCastAddressExpr(wordifyOperand(std::move(Right)));
    }
  }
  if ((Operator == "/" || Operator == "%") && isUnresolvedExpr(Right)) {
    return makeExpr(UnresolvedValueExpr{"unresolved divisor"});
  }
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

std::optional<llvm::APInt> evalConstantWord(const llvm::Value *V,
                                            unsigned Depth = 0) {
  if (V == nullptr || Depth > 8) {
    return std::nullopt;
  }
  if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(V)) {
    return C->getValue();
  }
  const auto *Op = llvm::dyn_cast<llvm::BinaryOperator>(V);
  if (Op == nullptr) {
    return std::nullopt;
  }
  std::optional<llvm::APInt> L = evalConstantWord(Op->getOperand(0), Depth + 1);
  std::optional<llvm::APInt> R = evalConstantWord(Op->getOperand(1), Depth + 1);
  if (!L.has_value() || !R.has_value() ||
      L->getBitWidth() != R->getBitWidth()) {
    return std::nullopt;
  }
  const unsigned Width = L->getBitWidth();
  switch (Op->getOpcode()) {
  case llvm::Instruction::Add:
    return *L + *R;
  case llvm::Instruction::Sub:
    return *L - *R;
  case llvm::Instruction::Mul:
    return *L * *R;
  case llvm::Instruction::And:
    return *L & *R;
  case llvm::Instruction::Or:
    return *L | *R;
  case llvm::Instruction::Xor:
    return *L ^ *R;
  case llvm::Instruction::Shl: {
    if (R->uge(Width)) return std::nullopt;
    return L->shl(R->getZExtValue());
  }
  case llvm::Instruction::LShr: {
    if (R->uge(Width)) return std::nullopt;
    return L->lshr(R->getZExtValue());
  }
  case llvm::Instruction::AShr: {
    if (R->uge(Width)) return std::nullopt;
    return L->ashr(R->getZExtValue());
  }
  case llvm::Instruction::UDiv:
    if (R->isZero()) return std::nullopt;
    return L->udiv(*R);
  case llvm::Instruction::URem:
    if (R->isZero()) return std::nullopt;
    return L->urem(*R);
  default:
    return std::nullopt;
  }
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
      ExprPtr Left = valueExpr(*Cmp->getOperand(0));
      ExprPtr Right = valueExpr(*Cmp->getOperand(1));
      if (needsWordOperandCast(*Operator)) {
        Left = wordCastAddressExpr(std::move(Left));
        Right = wordCastAddressExpr(std::move(Right));
      }
      return makeExpr(BinaryExpr{std::move(Left), Operator->str(),
                                 std::move(Right), Precedence, false, true});
    }
  }
  if (const auto *Op = llvm::dyn_cast<llvm::BinaryOperator>(&V)) {
    if (std::optional<llvm::APInt> Folded = evalConstantWord(Op)) {
      return makeExpr(LiteralExpr{formatInteger(*Folded), ""});
    }
    if ((Op->getOpcode() == llvm::Instruction::SDiv ||
         Op->getOpcode() == llvm::Instruction::UDiv ||
         Op->getOpcode() == llvm::Instruction::SRem ||
         Op->getOpcode() == llvm::Instruction::URem)) {
      if (const auto *Divisor =
              llvm::dyn_cast<llvm::ConstantInt>(Op->getOperand(1));
          Divisor != nullptr && Divisor->isZero()) {
        return makeExpr(UnresolvedValueExpr{"division by zero"});
      }
    }
    if (const llvm::Value *Operand = bitwiseNotOperand(*Op)) {
      // Solidity's `~` on an int_const produces a signed literal and then
      // fails when combined with uint256 operands.  Wordify the operand tree
      // (literals become uint256(...)) while keeping identifier arithmetic
      // readable, e.g. `~(arg0 + arg1)`.
      ExprPtr OperandExpr = wordifyOperand(valueExpr(*Operand));
      return makeExpr(UnaryExpr{"~", std::move(OperandExpr), true, 30});
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
    if (std::optional<ExprPtr> StorageExpr = storageHelperExpr(*Call)) {
      return *StorageExpr;
    }
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
    if (!Call->getType()->isVoidTy()) {
      // Scalar ABI decoder results carry the same argument-index annotation as
      // the extractvalue path.
      if (std::optional<ExprPtr> Argument =
              annotatedAbiArgumentExpr(*Call)) {
        return *Argument;
      }
      std::string Text =
          Callee == nullptr ? std::string("<indirect call>")
                            : Callee->getName().str();
      return makeExpr(UnresolvedValueExpr{std::move(Text)});
    }
  }
  if (const auto *Extract = llvm::dyn_cast<llvm::ExtractValueInst>(&V)) {
    if (std::optional<ExprPtr> Argument =
            annotatedAbiArgumentExpr(*Extract)) {
      return *Argument;
    }
    llvm::StringRef Name = Extract->getName();
    return makeExpr(UnresolvedValueExpr{
        Name.empty() ? std::string("extractvalue") : Name.str()});
  }
  if (const auto *Load = llvm::dyn_cast<llvm::LoadInst>(&V)) {
    // Prefer the pass-provided ABI argument index: it also covers plain
    // calldata loads and helper offsets the local shape matcher cannot see.
    if (std::optional<ExprPtr> Argument =
            annotatedAbiArgumentExpr(*Load)) {
      return *Argument;
    }
    if (std::optional<unsigned> ArgIndex = matchCalldataArgumentIndex(*Load)) {
      constexpr unsigned kRuntimeArgs = 4;
      const unsigned NameIndex = kRuntimeArgs + *ArgIndex;
      if (ActiveArgumentNames != nullptr &&
          NameIndex < ActiveArgumentNames->size() &&
          !(*ActiveArgumentNames)[NameIndex].empty() &&
          isWordLikeParameter((*ActiveArgumentNames)[NameIndex])) {
        return makeExpr(IdentifierExpr{(*ActiveArgumentNames)[NameIndex]});
      }
    }
    // Loads without a recovered variable/materialized payload are fallback
    // values; do not leak the raw SSA name into Solidity.
    llvm::StringRef Name = Load->getName();
    return makeExpr(UnresolvedValueExpr{
        Name.empty() ? std::string("load") : Name.str()});
  }
  if (const auto *Arg = llvm::dyn_cast<llvm::Argument>(&V)) {
    if (ActiveArgumentNames != nullptr &&
        Arg->getArgNo() < ActiveArgumentNames->size()) {
      const std::string &Name = (*ActiveArgumentNames)[Arg->getArgNo()];
      if (!Name.empty()) {
        return makeExpr(IdentifierExpr{Name});
      }
    }
  }

  // Any remaining SSA name is not declared in the generated Solidity: the
  // backend does not materialize locals.  Emit an explicit unresolved
  // placeholder instead of an undeclared identifier, and let condition
  // recovery fall back to TODO when such a value reaches a branch.
  std::string Name = V.hasName() ? V.getName().str() : FallbackName.str();
  if (ActiveParameterTypes != nullptr &&
      ActiveParameterTypes->count(Name) != 0) {
    return makeExpr(IdentifierExpr{std::move(Name)});
  }
  if (ActiveStorageSlots != nullptr) {
    for (const auto &[Slot, Info] : *ActiveStorageSlots) {
      (void)Slot;
      if (Info.Name == Name) {
        return makeExpr(IdentifierExpr{std::move(Name)});
      }
    }
  }
  return makeExpr(UnresolvedValueExpr{std::move(Name)});
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

std::optional<Statement>
formatStorageStore(const llvm::CallBase &Call) {
  const llvm::Function *Callee = Call.getCalledFunction();
  if (Callee == nullptr) {
    return std::nullopt;
  }
  llvm::StringRef Name = Callee->getName();
  if ((Name != "evm.storage.store" && Name != "evm_sstore") ||
      Call.arg_size() < 2) {
    return std::nullopt;
  }

  const StorageSlotInfo *Info =
      Name == "evm_sstore" ? storageSlotInfoForValue(*Call.getArgOperand(0))
                           : storageSlotInfoForRef(*Call.getArgOperand(0));
  if (Info == nullptr) {
    return std::nullopt;
  }
  // Mapping/array slots need a real lvalue path; packed stores need
  // read-modify-write typing.  Keep those disabled for now.
  if (Info->IsMapping || Info->IsArray) {
    return std::nullopt;
  }

  ExprPtr Value = valueExpr(*Call.getArgOperand(1));
  if (containsUnresolvedValue(Value)) {
    return std::nullopt;
  }

  return makeStmt(ExpressionStatement{makeExpr(AssignmentExpr{
      makeExpr(IdentifierExpr{Info->Name}), "=", std::move(Value)})});
}

} // namespace

Block BodyBuilder::readBody(const llvm::Function &F,
                            const StorageSlotMap *StorageSlots,
                            const std::vector<std::string> *ArgumentNames,
                            const ParameterTypeMap *ParameterTypes,
                            const EventParamTypeMap *EventParamTypes) {
  ActiveStorageSlotsScope StorageScope(StorageSlots, ArgumentNames,
                                       ParameterTypes, EventParamTypes);
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
          if (std::optional<Statement> Store = formatStorageStore(*Call)) {
            Out.push_back(addPayload(Payloads, std::move(*Store)));
            continue;
          }
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
      if (ExprPtr Condition = valueExpr(V)) {
        if (!containsUnresolvedValue(Condition) &&
            !containsIdentifierOfType(Condition, "bytes") &&
            !containsIdentifierOfType(Condition, "string")) {
          return addPayload(Payloads, Expression{*Condition});
        }
      }
      return addPayload(
          Payloads,
          Expression{TodoConditionExpr{llvmValueDebugName(V, FallbackName)}});
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
    BodyCompletion Completion =
        analyzeBodyCompletion(Cfg, Tree, Tree.root());
    if (Completion.HasGoto || !Completion.ReachesExit) {
      Result.Statements.push_back(
          commentStmt("// TODO: recover remaining body"));
    }
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
  std::vector<ExprPtr> Args = getEventTopicArguments(I);
  if (ActiveEventParamTypes != nullptr) {
    auto It = ActiveEventParamTypes->find(Name);
    if (It != ActiveEventParamTypes->end()) {
      const std::vector<std::string> &Types = It->second;
      for (std::size_t Arg = 0; Arg < Args.size() && Arg < Types.size();
           ++Arg) {
        const std::string &Type = Types[Arg];
        if (Type == "address" && !isMsgSenderExpr(Args[Arg])) {
          Args[Arg] = makeExpr(CallExpr{
              makeExpr(IdentifierExpr{"address"}),
              {makeExpr(CallExpr{makeExpr(IdentifierExpr{"uint160"}),
                                 {Args[Arg]}, {}})},
              {}});
        } else if (Type == "uint256") {
          Args[Arg] = wordCastAddressExpr(std::move(Args[Arg]));
        }
      }
    }
  }
  return makeStmt(EmitStatement{makeExpr(IdentifierExpr{std::move(Name)}),
                                std::move(Args),
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
