#ifndef NOTDEC_BACKENDS_SOLIDITY_PRINTER_H
#define NOTDEC_BACKENDS_SOLIDITY_PRINTER_H

#include <llvm/Support/raw_ostream.h>

#include "notdec-backends/Solidity/Ast.h"

namespace notdec::backend::solidity {

class Printer {
public:
  explicit Printer(llvm::raw_ostream &OS) : OS(OS) {}

  void print(const SourceUnit &Unit);

private:
  llvm::raw_ostream &OS;
  unsigned Indent = 0;

  void printSourceUnitItem(const SourceUnitItem &Item);
  void printPragma(const PragmaDirective &Pragma);
  void printImport(const ImportDirective &Import);
  void printUsingDirective(const UsingDirective &Using);
  void printContract(const Contract &Contract);
  void printStruct(const StructDecl &Struct);
  void printEnum(const EnumDecl &Enum);
  void printUserDefinedValueType(const UserDefinedValueTypeDecl &Decl);
  void printError(const ErrorDecl &Error);
  void printEvent(const EventDecl &Event);
  void printConstantVariable(const ConstantVariableDecl &Var);
  void printStateVariable(const StateVariable &Var);
  void printFunction(const Function &Func);
  void printModifierInvocation(const ModifierInvocation &Modifier);
  void printOverrideSpecifier(const std::optional<OverrideSpecifier> &Override);
  void printBlock(const Block &Block);
  void printBlock(const Block &Block, unsigned ExtraIndent);
  void printStatement(const Statement &Stmt, unsigned ExtraIndent = 0);
  void printSimpleStatementInline(const Statement &Stmt);
  void printCommentStatement(const CommentStatement &Stmt,
                             unsigned ExtraIndent);
  void printReturnStatement(const ReturnStatement &Stmt, unsigned ExtraIndent);
  void printExpressionStatement(const ExpressionStatement &Stmt,
                                unsigned ExtraIndent);
  void printVariableDeclarationStatement(
      const VariableDeclarationStatement &Stmt, unsigned ExtraIndent);
  void printRevertStatement(const RevertStatement &Stmt, unsigned ExtraIndent);
  void printRequireStatement(const RequireStatement &Stmt,
                             unsigned ExtraIndent);
  void printEmitStatement(const EmitStatement &Stmt, unsigned ExtraIndent);
  void printIfStatement(const IfStatement &Stmt, unsigned ExtraIndent);
  void printForStatement(const ForStatement &Stmt, unsigned ExtraIndent);
  void printWhileStatement(const WhileStatement &Stmt, unsigned ExtraIndent);
  void printDoWhileStatement(const DoWhileStatement &Stmt,
                             unsigned ExtraIndent);
  void printTryStatement(const TryStatement &Stmt, unsigned ExtraIndent);
  void printUncheckedBlockStatement(const UncheckedBlockStatement &Stmt,
                                    unsigned ExtraIndent);
  void printAssemblyStatement(const AssemblyStatement &Stmt,
                              unsigned ExtraIndent);
  void printBlockStatement(const BlockStatement &Stmt, unsigned ExtraIndent);
  void printExpression(const Expression &Expr, unsigned ParentPrecedence = 0,
                       bool ParenthesizeSamePrecedenceOperand = false);
  void printArguments(const std::vector<ExprPtr> &Args);
  void printNamedArguments(const std::vector<NamedArgument> &Args);
  void printParameters(const std::vector<Parameter> &Params);
  void printParameter(const Parameter &Param);
  void printVariableDeclaration(const VariableDeclaration &Decl);
  void printType(const TypeRef &Type);
  void printTypeName(const TypeName &Type);
  void printIdentifierPath(const IdentifierPath &Path);
  void printIndent();
  void printStatementIndent(unsigned ExtraIndent);
};

} // namespace notdec::backend::solidity

#endif
