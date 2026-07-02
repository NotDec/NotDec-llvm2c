#ifndef NOTDEC_BACKENDS_SOLIDITY_AST_NODES_H
#define NOTDEC_BACKENDS_SOLIDITY_AST_NODES_H

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace notdec::backend::solidity {

struct Expression;
struct Statement;
struct Block;
struct TypeName;

using ExprPtr = std::shared_ptr<Expression>;
using StmtPtr = std::shared_ptr<Statement>;
using BlockPtr = std::shared_ptr<Block>;
using TypeNamePtr = std::shared_ptr<TypeName>;

struct IdentifierPath {
  std::vector<std::string> Parts;
};

// TypeRef keeps the stable spelling used by the current decompiler while also
// leaving room for a grammar-shaped TypeName tree as type recovery improves.
struct TypeRef {
  std::string Name;
  TypeNamePtr Syntax;
};

struct ElementaryTypeName {
  std::string Name;
};

struct FunctionTypeName {
  std::vector<TypeRef> Parameters;
  std::string Visibility;
  std::string StateMutability;
  std::vector<TypeRef> Returns;
};

struct MappingTypeName {
  TypeRef Key;
  std::string KeyName;
  TypeRef Value;
  std::string ValueName;
};

struct UserDefinedTypeName {
  IdentifierPath Name;
};

struct ArrayTypeName {
  TypeRef Base;
  ExprPtr Size;
};

using TypeNameNode = std::variant<ElementaryTypeName, FunctionTypeName,
                                  MappingTypeName, UserDefinedTypeName,
                                  ArrayTypeName>;

struct TypeName {
  TypeNameNode Node;
};

struct Parameter {
  TypeRef Type;
  std::string Name;
  std::string DataLocation;
  bool Indexed = false;
};

struct NamedArgument {
  std::string Name;
  ExprPtr Value;
};

struct IdentifierExpr {
  std::string Name;
};

struct TypeNameExpr {
  TypeRef Type;
};

struct LiteralExpr {
  std::string Text;
  std::string SubDenomination;
};

struct UnknownExpr {
  std::string Text;
};

struct TodoConditionExpr {
  std::string Text;
};

struct MemberAccessExpr {
  ExprPtr Base;
  std::string Member;
};

struct IndexAccessExpr {
  ExprPtr Base;
  ExprPtr Index;
};

struct IndexRangeAccessExpr {
  ExprPtr Base;
  ExprPtr Start;
  ExprPtr End;
};

struct FunctionCallOptionsExpr {
  ExprPtr Callee;
  std::vector<NamedArgument> Options;
};

struct UnaryExpr {
  std::string Operator;
  ExprPtr Operand;
  bool Prefix = true;
  unsigned Precedence = 30;
};

struct BinaryExpr {
  ExprPtr Left;
  std::string Operator;
  ExprPtr Right;
  unsigned Precedence = 0;
  bool ParenthesizeLeftOnEqual = false;
  bool ParenthesizeRightOnEqual = false;
};

struct ConditionalExpr {
  ExprPtr Condition;
  ExprPtr TrueValue;
  ExprPtr FalseValue;
};

struct AssignmentExpr {
  ExprPtr Left;
  std::string Operator = "=";
  ExprPtr Right;
};

struct CallExpr {
  ExprPtr Callee;
  std::vector<ExprPtr> Arguments;
  std::vector<NamedArgument> NamedArguments;
};

struct NewExpr {
  TypeRef Type;
};

struct TupleExpr {
  std::vector<ExprPtr> Elements;
};

struct InlineArrayExpr {
  std::vector<ExprPtr> Elements;
};

struct MetaTypeExpr {
  TypeRef Type;
};

// The expression nodes mirror SolidityParser.g4's expression alternatives:
// primary expressions, member/index access, calls and call options, unary and
// binary operators, conditional/assignment expressions, new, tuples, arrays,
// and meta-type expressions.
using ExpressionNode =
    std::variant<IdentifierExpr, TypeNameExpr, LiteralExpr, UnknownExpr,
                 TodoConditionExpr, MemberAccessExpr, IndexAccessExpr,
                 IndexRangeAccessExpr, FunctionCallOptionsExpr, UnaryExpr,
                 BinaryExpr, ConditionalExpr, AssignmentExpr, CallExpr, NewExpr,
                 TupleExpr, InlineArrayExpr, MetaTypeExpr>;

struct Expression {
  ExpressionNode Node;
};

struct VariableDeclaration {
  TypeRef Type;
  std::string DataLocation;
  std::string Name;
};

struct CommentStatement {
  std::string Text;
};

struct ReturnStatement {
  ExprPtr Value;
};

struct ExpressionStatement {
  ExprPtr Value;
};

struct VariableDeclarationStatement {
  std::vector<std::optional<VariableDeclaration>> Variables;
  ExprPtr InitialValue;
};

struct RevertStatement {
  ExprPtr Error;
  std::vector<ExprPtr> Arguments;
  std::string Comment;
};

struct RequireStatement {
  ExprPtr Condition;
  std::vector<ExprPtr> Arguments;
};

struct EmitStatement {
  ExprPtr Event;
  std::vector<ExprPtr> Arguments;
  std::string Comment;
};

struct IfStatement {
  ExprPtr Condition;
  BlockPtr Then;
  BlockPtr Else;
};

struct ForStatement {
  StmtPtr Init;
  ExprPtr Condition;
  ExprPtr Loop;
  StmtPtr Body;
};

struct WhileStatement {
  ExprPtr Condition;
  BlockPtr Body;
};

struct DoWhileStatement {
  StmtPtr Body;
  ExprPtr Condition;
};

struct TryCatchClause {
  std::string Name;
  std::vector<Parameter> Parameters;
  BlockPtr Body;
};

struct TryStatement {
  ExprPtr ExternalCall;
  std::vector<Parameter> Returns;
  BlockPtr Body;
  std::vector<TryCatchClause> Catches;
};

struct UncheckedBlockStatement {
  BlockPtr Body;
};

struct AssemblyStatement {
  std::string Dialect;
  std::vector<std::string> Flags;
  std::string Body;
};

struct BlockStatement {
  BlockPtr Body;
};

struct BreakStatement {};

struct ContinueStatement {};

// Statement follows SolidityParser.g4's statement rule.  The backend still uses
// CommentStatement for decompiler-only block labels and TODO notes; actual
// Solidity constructs have their own nodes.
using StatementNode =
    std::variant<CommentStatement, ReturnStatement, ExpressionStatement,
                 VariableDeclarationStatement, RevertStatement,
                 RequireStatement, EmitStatement, IfStatement, ForStatement,
                 WhileStatement, DoWhileStatement, TryStatement,
                 UncheckedBlockStatement, AssemblyStatement, BlockStatement,
                 BreakStatement, ContinueStatement>;

struct Statement {
  StatementNode Node;
};

struct Block {
  std::vector<Statement> Statements;
};

struct OverrideSpecifier {
  std::vector<IdentifierPath> Paths;
};

struct ModifierInvocation {
  IdentifierPath Name;
  std::vector<ExprPtr> Arguments;
  std::vector<NamedArgument> NamedArguments;
};

struct StateVariable {
  TypeRef Type;
  std::string Name;
  std::string Visibility;
  bool Constant = false;
  bool Immutable = false;
  bool Transient = false;
  std::optional<OverrideSpecifier> Override;
  ExprPtr InitialValue;
};

struct EventDecl {
  std::string Name;
  std::vector<Parameter> Parameters;
  bool Anonymous = false;
};

struct ErrorDecl {
  std::string Name;
  std::vector<Parameter> Parameters;
};

struct StructMember {
  TypeRef Type;
  std::string Name;
};

struct StructDecl {
  std::string Name;
  std::vector<StructMember> Members;
};

struct EnumDecl {
  std::string Name;
  std::vector<std::string> Values;
};

struct UserDefinedValueTypeDecl {
  std::string Name;
  TypeRef UnderlyingType;
};

struct UsingAlias {
  IdentifierPath FunctionName;
  std::string Operator;
};

struct UsingDirective {
  IdentifierPath LibraryName;
  std::vector<UsingAlias> Aliases;
  std::optional<TypeRef> ForType;
  bool ForAny = false;
  bool Global = false;
};

enum class FunctionKind {
  Function,
  Constructor,
  Fallback,
  Receive,
  Modifier
};

struct Function {
  std::string Name;
  FunctionKind Kind = FunctionKind::Function;
  std::string Visibility = "public";
  std::string StateMutability;
  bool Virtual = false;
  std::optional<OverrideSpecifier> Override;
  std::vector<Parameter> Parameters;
  std::vector<ModifierInvocation> Modifiers;
  std::vector<Parameter> Returns;
  std::optional<Block> Body;
};

enum class ContractKind {
  Contract,
  Interface,
  Library
};

struct InheritanceSpecifier {
  IdentifierPath Name;
  std::vector<ExprPtr> Arguments;
};

struct Contract {
  std::string Name = "Decompiled";
  ContractKind Kind = ContractKind::Contract;
  bool Abstract = false;
  ExprPtr StorageLayoutBase;
  std::vector<InheritanceSpecifier> BaseContracts;
  std::vector<UsingDirective> UsingDirectives;
  std::vector<StructDecl> Structs;
  std::vector<EnumDecl> Enums;
  std::vector<UserDefinedValueTypeDecl> UserDefinedValueTypes;
  std::vector<StateVariable> StateVariables;
  std::vector<EventDecl> Events;
  std::vector<ErrorDecl> Errors;
  std::vector<Function> Functions;
};

struct PragmaDirective {
  std::vector<std::string> Tokens;
};

struct ImportDirective {
  std::string Text;
};

struct ConstantVariableDecl {
  TypeRef Type;
  std::string Name;
  ExprPtr InitialValue;
};

using SourceUnitItem =
    std::variant<PragmaDirective, ImportDirective, UsingDirective, Contract,
                 Function, ConstantVariableDecl, StructDecl, EnumDecl,
                 UserDefinedValueTypeDecl, ErrorDecl, EventDecl>;

struct SourceUnit {
  std::vector<SourceUnitItem> Items;
  std::vector<Contract> Contracts;
};

} // namespace notdec::backend::solidity

#endif
