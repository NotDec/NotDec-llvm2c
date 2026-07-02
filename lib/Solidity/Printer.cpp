#include "notdec-backends/Solidity/Printer.h"

#include <type_traits>

namespace notdec::backend::solidity {

namespace {

unsigned expressionPrecedence(const Expression &Expr) {
  if (std::holds_alternative<AssignmentExpr>(Expr.Node)) {
    return 0;
  }
  if (std::holds_alternative<ConditionalExpr>(Expr.Node)) {
    return 1;
  }
  if (const auto *Unary = std::get_if<UnaryExpr>(&Expr.Node)) {
    return Unary->Precedence;
  }
  if (const auto *Binary = std::get_if<BinaryExpr>(&Expr.Node)) {
    return Binary->Precedence;
  }
  if (std::holds_alternative<MemberAccessExpr>(Expr.Node) ||
      std::holds_alternative<IndexAccessExpr>(Expr.Node) ||
      std::holds_alternative<IndexRangeAccessExpr>(Expr.Node) ||
      std::holds_alternative<FunctionCallOptionsExpr>(Expr.Node) ||
      std::holds_alternative<CallExpr>(Expr.Node)) {
    return 30;
  }
  return 40;
}

} // namespace

void Printer::print(const SourceUnit &Unit) {
  if (!Unit.Items.empty()) {
    for (std::size_t I = 0; I < Unit.Items.size(); ++I) {
      if (I != 0) {
        OS << "\n";
      }
      printSourceUnitItem(Unit.Items[I]);
    }
    return;
  }

  for (std::size_t I = 0; I < Unit.Contracts.size(); ++I) {
    if (I != 0) {
      OS << "\n";
    }
    printContract(Unit.Contracts[I]);
  }
}

void Printer::printSourceUnitItem(const SourceUnitItem &Item) {
  std::visit([this](const auto &Node) {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, PragmaDirective>) {
      printPragma(Node);
    } else if constexpr (std::is_same_v<T, ImportDirective>) {
      printImport(Node);
    } else if constexpr (std::is_same_v<T, UsingDirective>) {
      printUsingDirective(Node);
    } else if constexpr (std::is_same_v<T, Contract>) {
      printContract(Node);
    } else if constexpr (std::is_same_v<T, Function>) {
      printFunction(Node);
    } else if constexpr (std::is_same_v<T, ConstantVariableDecl>) {
      printConstantVariable(Node);
    } else if constexpr (std::is_same_v<T, StructDecl>) {
      printStruct(Node);
    } else if constexpr (std::is_same_v<T, EnumDecl>) {
      printEnum(Node);
    } else if constexpr (std::is_same_v<T, UserDefinedValueTypeDecl>) {
      printUserDefinedValueType(Node);
    } else if constexpr (std::is_same_v<T, ErrorDecl>) {
      printError(Node);
    } else if constexpr (std::is_same_v<T, EventDecl>) {
      printEvent(Node);
    }
  }, Item);
}

void Printer::printPragma(const PragmaDirective &Pragma) {
  OS << "pragma";
  for (const std::string &Token : Pragma.Tokens) {
    OS << " " << Token;
  }
  OS << ";\n";
}

void Printer::printImport(const ImportDirective &Import) {
  OS << "import " << Import.Text << ";\n";
}

void Printer::printUsingDirective(const UsingDirective &Using) {
  printIndent();
  OS << "using ";
  if (!Using.Aliases.empty()) {
    OS << "{";
    for (std::size_t I = 0; I < Using.Aliases.size(); ++I) {
      if (I != 0) {
        OS << ", ";
      }
      printIdentifierPath(Using.Aliases[I].FunctionName);
      if (!Using.Aliases[I].Operator.empty()) {
        OS << " as " << Using.Aliases[I].Operator;
      }
    }
    OS << "}";
  } else {
    printIdentifierPath(Using.LibraryName);
  }
  OS << " for ";
  if (Using.ForAny) {
    OS << "*";
  } else if (Using.ForType.has_value()) {
    printType(*Using.ForType);
  }
  if (Using.Global) {
    OS << " global";
  }
  OS << ";\n";
}

void Printer::printContract(const Contract &Contract) {
  if (Contract.Abstract) {
    OS << "abstract ";
  }
  switch (Contract.Kind) {
  case ContractKind::Contract:
    OS << "contract ";
    break;
  case ContractKind::Interface:
    OS << "interface ";
    break;
  case ContractKind::Library:
    OS << "library ";
    break;
  }
  OS << Contract.Name;
  if (Contract.StorageLayoutBase) {
    OS << " layout at ";
    printExpression(*Contract.StorageLayoutBase);
  }
  if (!Contract.BaseContracts.empty()) {
    OS << " is ";
    for (std::size_t I = 0; I < Contract.BaseContracts.size(); ++I) {
      if (I != 0) {
        OS << ", ";
      }
      printIdentifierPath(Contract.BaseContracts[I].Name);
      if (!Contract.BaseContracts[I].Arguments.empty()) {
        OS << "(";
        printArguments(Contract.BaseContracts[I].Arguments);
        OS << ")";
      }
    }
  }
  OS << " {\n";
  ++Indent;
  for (const auto &Using : Contract.UsingDirectives) {
    printUsingDirective(Using);
  }
  for (const auto &Struct : Contract.Structs) {
    printStruct(Struct);
  }
  for (const auto &Enum : Contract.Enums) {
    printEnum(Enum);
  }
  for (const auto &Decl : Contract.UserDefinedValueTypes) {
    printUserDefinedValueType(Decl);
  }
  for (const auto &Event : Contract.Events) {
    printEvent(Event);
  }
  for (const auto &Error : Contract.Errors) {
    printError(Error);
  }
  if ((!Contract.Events.empty() || !Contract.Errors.empty() ||
       !Contract.Structs.empty() || !Contract.Enums.empty() ||
       !Contract.UserDefinedValueTypes.empty() ||
       !Contract.UsingDirectives.empty()) &&
      (!Contract.StateVariables.empty() || !Contract.Functions.empty())) {
    OS << "\n";
  }
  for (const auto &Var : Contract.StateVariables) {
    printStateVariable(Var);
  }
  if (!Contract.StateVariables.empty() && !Contract.Functions.empty()) {
    OS << "\n";
  }
  for (std::size_t I = 0; I < Contract.Functions.size(); ++I) {
    if (I != 0) {
      OS << "\n";
    }
    printFunction(Contract.Functions[I]);
  }
  --Indent;
  OS << "}\n";
}

void Printer::printStruct(const StructDecl &Struct) {
  printIndent();
  OS << "struct " << Struct.Name << " {\n";
  ++Indent;
  for (const StructMember &Member : Struct.Members) {
    printIndent();
    printType(Member.Type);
    OS << " " << Member.Name << ";\n";
  }
  --Indent;
  printIndent();
  OS << "}\n";
}

void Printer::printEnum(const EnumDecl &Enum) {
  printIndent();
  OS << "enum " << Enum.Name << " {";
  for (std::size_t I = 0; I < Enum.Values.size(); ++I) {
    if (I != 0) {
      OS << ", ";
    }
    OS << Enum.Values[I];
  }
  OS << "}\n";
}

void Printer::printUserDefinedValueType(const UserDefinedValueTypeDecl &Decl) {
  printIndent();
  OS << "type " << Decl.Name << " is ";
  printType(Decl.UnderlyingType);
  OS << ";\n";
}

void Printer::printError(const ErrorDecl &Error) {
  printIndent();
  OS << "error " << Error.Name << "(";
  printParameters(Error.Parameters);
  OS << ");\n";
}

void Printer::printEvent(const EventDecl &Event) {
  printIndent();
  OS << "event " << Event.Name << "(";
  printParameters(Event.Parameters);
  OS << ")";
  if (Event.Anonymous) {
    OS << " anonymous";
  }
  OS << ";\n";
}

void Printer::printConstantVariable(const ConstantVariableDecl &Var) {
  printIndent();
  printType(Var.Type);
  OS << " constant " << Var.Name << " = ";
  if (Var.InitialValue) {
    printExpression(*Var.InitialValue);
  }
  OS << ";\n";
}

void Printer::printStateVariable(const StateVariable &Var) {
  printIndent();
  printType(Var.Type);
  if (!Var.Visibility.empty()) {
    OS << " " << Var.Visibility;
  }
  if (Var.Constant) {
    OS << " constant";
  }
  if (Var.Immutable) {
    OS << " immutable";
  }
  if (Var.Transient) {
    OS << " transient";
  }
  printOverrideSpecifier(Var.Override);
  OS << " " << Var.Name;
  if (Var.InitialValue) {
    OS << " = ";
    printExpression(*Var.InitialValue);
  }
  OS << ";\n";
}

void Printer::printFunction(const Function &Func) {
  printIndent();
  switch (Func.Kind) {
  case FunctionKind::Constructor:
    OS << "constructor";
    break;
  case FunctionKind::Fallback:
    OS << "fallback";
    break;
  case FunctionKind::Receive:
    OS << "receive";
    break;
  case FunctionKind::Modifier:
    OS << "modifier " << Func.Name;
    break;
  case FunctionKind::Function:
    OS << "function " << Func.Name;
    break;
  }
  OS << "(";
  printParameters(Func.Parameters);
  OS << ")";
  if (!Func.Visibility.empty()) {
    OS << " " << Func.Visibility;
  }
  if (!Func.StateMutability.empty()) {
    OS << " " << Func.StateMutability;
  }
  for (const ModifierInvocation &Modifier : Func.Modifiers) {
    OS << " ";
    printModifierInvocation(Modifier);
  }
  if (Func.Virtual) {
    OS << " virtual";
  }
  printOverrideSpecifier(Func.Override);
  if (!Func.Returns.empty()) {
    OS << " returns (";
    printParameters(Func.Returns);
    OS << ")";
  }
  if (!Func.Body.has_value()) {
    OS << ";\n";
    return;
  }
  OS << " {\n";
  ++Indent;
  printBlock(*Func.Body);
  --Indent;
  printIndent();
  OS << "}\n";
}

void Printer::printModifierInvocation(const ModifierInvocation &Modifier) {
  printIdentifierPath(Modifier.Name);
  if (!Modifier.Arguments.empty()) {
    OS << "(";
    printArguments(Modifier.Arguments);
    OS << ")";
  } else if (!Modifier.NamedArguments.empty()) {
    OS << "({";
    printNamedArguments(Modifier.NamedArguments);
    OS << "})";
  }
}

void Printer::printOverrideSpecifier(
    const std::optional<OverrideSpecifier> &Override) {
  if (!Override.has_value()) {
    return;
  }
  OS << " override";
  if (!Override->Paths.empty()) {
    OS << "(";
    for (std::size_t I = 0; I < Override->Paths.size(); ++I) {
      if (I != 0) {
        OS << ", ";
      }
      printIdentifierPath(Override->Paths[I]);
    }
    OS << ")";
  }
}

void Printer::printBlock(const Block &Block) { printBlock(Block, 0); }

void Printer::printBlock(const Block &Block, unsigned ExtraIndent) {
  for (const Statement &Stmt : Block.Statements) {
    printStatement(Stmt, ExtraIndent);
  }
}

void Printer::printStatement(const Statement &Stmt, unsigned ExtraIndent) {
  std::visit([this, ExtraIndent](const auto &Node) {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, CommentStatement>) {
      printCommentStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, ReturnStatement>) {
      printReturnStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, ExpressionStatement>) {
      printExpressionStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, VariableDeclarationStatement>) {
      printVariableDeclarationStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, RevertStatement>) {
      printRevertStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, RequireStatement>) {
      printRequireStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, EmitStatement>) {
      printEmitStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, IfStatement>) {
      printIfStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, ForStatement>) {
      printForStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, WhileStatement>) {
      printWhileStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, DoWhileStatement>) {
      printDoWhileStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, TryStatement>) {
      printTryStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, UncheckedBlockStatement>) {
      printUncheckedBlockStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, AssemblyStatement>) {
      printAssemblyStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, BlockStatement>) {
      printBlockStatement(Node, ExtraIndent);
    } else if constexpr (std::is_same_v<T, BreakStatement>) {
      printStatementIndent(ExtraIndent);
      OS << "break;\n";
    } else if constexpr (std::is_same_v<T, ContinueStatement>) {
      printStatementIndent(ExtraIndent);
      OS << "continue;\n";
    }
  }, Stmt.Node);
}

void Printer::printSimpleStatementInline(const Statement &Stmt) {
  std::visit([this](const auto &Node) {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, ExpressionStatement>) {
      if (Node.Value) {
        printExpression(*Node.Value);
      }
    } else if constexpr (std::is_same_v<T, VariableDeclarationStatement>) {
      for (std::size_t I = 0; I < Node.Variables.size(); ++I) {
        if (I != 0) {
          OS << ", ";
        }
        if (Node.Variables[I].has_value()) {
          printVariableDeclaration(*Node.Variables[I]);
        }
      }
      if (Node.InitialValue) {
        OS << " = ";
        printExpression(*Node.InitialValue);
      }
    }
  }, Stmt.Node);
}

void Printer::printCommentStatement(const CommentStatement &Stmt,
                                    unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << Stmt.Text << "\n";
}

void Printer::printReturnStatement(const ReturnStatement &Stmt,
                                   unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "return";
  if (Stmt.Value) {
    OS << " ";
    printExpression(*Stmt.Value);
  }
  OS << ";\n";
}

void Printer::printExpressionStatement(const ExpressionStatement &Stmt,
                                       unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  if (Stmt.Value) {
    printExpression(*Stmt.Value);
  }
  OS << ";\n";
}

void Printer::printVariableDeclarationStatement(
    const VariableDeclarationStatement &Stmt, unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  if (Stmt.Variables.size() == 1 && Stmt.Variables.front().has_value()) {
    printVariableDeclaration(*Stmt.Variables.front());
  } else {
    OS << "(";
    for (std::size_t I = 0; I < Stmt.Variables.size(); ++I) {
      if (I != 0) {
        OS << ", ";
      }
      if (Stmt.Variables[I].has_value()) {
        printVariableDeclaration(*Stmt.Variables[I]);
      }
    }
    OS << ")";
  }
  if (Stmt.InitialValue) {
    OS << " = ";
    printExpression(*Stmt.InitialValue);
  }
  OS << ";\n";
}

void Printer::printRevertStatement(const RevertStatement &Stmt,
                                   unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "revert";
  if (Stmt.Error) {
    OS << " ";
    printExpression(*Stmt.Error);
  }
  OS << "(";
  printArguments(Stmt.Arguments);
  OS << ");";
  if (!Stmt.Comment.empty()) {
    OS << " // " << Stmt.Comment;
  }
  OS << "\n";
}

void Printer::printRequireStatement(const RequireStatement &Stmt,
                                    unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "require(";
  if (Stmt.Condition) {
    printExpression(*Stmt.Condition);
  }
  for (const ExprPtr &Arg : Stmt.Arguments) {
    OS << ", ";
    if (Arg) {
      printExpression(*Arg);
    }
  }
  OS << ");\n";
}

void Printer::printEmitStatement(const EmitStatement &Stmt,
                                 unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "emit ";
  if (Stmt.Event) {
    printExpression(*Stmt.Event);
  }
  OS << "(";
  printArguments(Stmt.Arguments);
  OS << ");";
  if (!Stmt.Comment.empty()) {
    OS << " // " << Stmt.Comment;
  }
  OS << "\n";
}

void Printer::printIfStatement(const IfStatement &Stmt, unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "if (";
  if (Stmt.Condition) {
    printExpression(*Stmt.Condition);
  }
  OS << ") {\n";
  if (Stmt.Then) {
    printBlock(*Stmt.Then, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}";
  if (Stmt.Else) {
    OS << "\n";
    printStatementIndent(ExtraIndent);
    OS << "else {\n";
    printBlock(*Stmt.Else, ExtraIndent + 1);
    printStatementIndent(ExtraIndent);
    OS << "}";
  }
  OS << "\n";
}

void Printer::printForStatement(const ForStatement &Stmt,
                                unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "for (";
  if (Stmt.Init) {
    printSimpleStatementInline(*Stmt.Init);
  }
  OS << "; ";
  if (Stmt.Condition) {
    printExpression(*Stmt.Condition);
  }
  OS << "; ";
  if (Stmt.Loop) {
    printExpression(*Stmt.Loop);
  }
  OS << ") {\n";
  if (Stmt.Body) {
    printStatement(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}\n";
}

void Printer::printWhileStatement(const WhileStatement &Stmt,
                                  unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "while (";
  if (Stmt.Condition) {
    printExpression(*Stmt.Condition);
  }
  OS << ") {\n";
  if (Stmt.Body) {
    printBlock(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}\n";
}

void Printer::printDoWhileStatement(const DoWhileStatement &Stmt,
                                    unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "do {\n";
  if (Stmt.Body) {
    printStatement(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "} while (";
  if (Stmt.Condition) {
    printExpression(*Stmt.Condition);
  }
  OS << ");\n";
}

void Printer::printTryStatement(const TryStatement &Stmt, unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "try ";
  if (Stmt.ExternalCall) {
    printExpression(*Stmt.ExternalCall);
  }
  if (!Stmt.Returns.empty()) {
    OS << " returns (";
    printParameters(Stmt.Returns);
    OS << ")";
  }
  OS << " {\n";
  if (Stmt.Body) {
    printBlock(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}";
  for (const TryCatchClause &Catch : Stmt.Catches) {
    OS << " catch";
    if (!Catch.Name.empty() || !Catch.Parameters.empty()) {
      OS << " " << Catch.Name << "(";
      printParameters(Catch.Parameters);
      OS << ")";
    }
    OS << " {\n";
    if (Catch.Body) {
      printBlock(*Catch.Body, ExtraIndent + 1);
    }
    printStatementIndent(ExtraIndent);
    OS << "}";
  }
  OS << "\n";
}

void Printer::printUncheckedBlockStatement(const UncheckedBlockStatement &Stmt,
                                           unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "unchecked {\n";
  if (Stmt.Body) {
    printBlock(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}\n";
}

void Printer::printAssemblyStatement(const AssemblyStatement &Stmt,
                                     unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "assembly";
  if (!Stmt.Dialect.empty()) {
    OS << " " << Stmt.Dialect;
  }
  if (!Stmt.Flags.empty()) {
    OS << " (";
    for (std::size_t I = 0; I < Stmt.Flags.size(); ++I) {
      if (I != 0) {
        OS << ", ";
      }
      OS << Stmt.Flags[I];
    }
    OS << ")";
  }
  OS << " {";
  if (!Stmt.Body.empty()) {
    OS << " " << Stmt.Body << " ";
  }
  OS << "}\n";
}

void Printer::printBlockStatement(const BlockStatement &Stmt,
                                  unsigned ExtraIndent) {
  printStatementIndent(ExtraIndent);
  OS << "{\n";
  if (Stmt.Body) {
    printBlock(*Stmt.Body, ExtraIndent + 1);
  }
  printStatementIndent(ExtraIndent);
  OS << "}\n";
}

void Printer::printExpression(const Expression &Expr, unsigned ParentPrecedence,
                              bool ParenthesizeSamePrecedenceOperand) {
  unsigned Precedence = expressionPrecedence(Expr);
  bool NeedsParens = Precedence < ParentPrecedence ||
                     (ParenthesizeSamePrecedenceOperand &&
                      Precedence == ParentPrecedence);
  if (NeedsParens) {
    OS << "(";
  }

  std::visit([this](const auto &Node) {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, IdentifierExpr>) {
      OS << Node.Name;
    } else if constexpr (std::is_same_v<T, TypeNameExpr>) {
      printType(Node.Type);
    } else if constexpr (std::is_same_v<T, LiteralExpr>) {
      OS << Node.Text;
      if (!Node.SubDenomination.empty()) {
        OS << " " << Node.SubDenomination;
      }
    } else if constexpr (std::is_same_v<T, UnknownExpr>) {
      OS << Node.Text;
    } else if constexpr (std::is_same_v<T, TodoConditionExpr>) {
      OS << "false /* TODO: " << Node.Text << " */";
    } else if constexpr (std::is_same_v<T, MemberAccessExpr>) {
      if (Node.Base) {
        printExpression(*Node.Base, 30);
      }
      OS << "." << Node.Member;
    } else if constexpr (std::is_same_v<T, IndexAccessExpr>) {
      if (Node.Base) {
        printExpression(*Node.Base, 30);
      }
      OS << "[";
      if (Node.Index) {
        printExpression(*Node.Index);
      }
      OS << "]";
    } else if constexpr (std::is_same_v<T, IndexRangeAccessExpr>) {
      if (Node.Base) {
        printExpression(*Node.Base, 30);
      }
      OS << "[";
      if (Node.Start) {
        printExpression(*Node.Start);
      }
      OS << ":";
      if (Node.End) {
        printExpression(*Node.End);
      }
      OS << "]";
    } else if constexpr (std::is_same_v<T, FunctionCallOptionsExpr>) {
      if (Node.Callee) {
        printExpression(*Node.Callee, 30);
      }
      OS << "{";
      printNamedArguments(Node.Options);
      OS << "}";
    } else if constexpr (std::is_same_v<T, UnaryExpr>) {
      if (Node.Prefix) {
        OS << Node.Operator;
      }
      if (Node.Operand) {
        printExpression(*Node.Operand, Node.Precedence);
      }
      if (!Node.Prefix) {
        OS << Node.Operator;
      }
    } else if constexpr (std::is_same_v<T, BinaryExpr>) {
      if (Node.Left) {
        printExpression(*Node.Left, Node.Precedence,
                        Node.ParenthesizeLeftOnEqual);
      }
      OS << " " << Node.Operator << " ";
      if (Node.Right) {
        printExpression(*Node.Right, Node.Precedence,
                        Node.ParenthesizeRightOnEqual);
      }
    } else if constexpr (std::is_same_v<T, ConditionalExpr>) {
      if (Node.Condition) {
        printExpression(*Node.Condition, 1);
      }
      OS << " ? ";
      if (Node.TrueValue) {
        printExpression(*Node.TrueValue, 1);
      }
      OS << " : ";
      if (Node.FalseValue) {
        printExpression(*Node.FalseValue, 1);
      }
    } else if constexpr (std::is_same_v<T, AssignmentExpr>) {
      if (Node.Left) {
        printExpression(*Node.Left, 0);
      }
      OS << " " << Node.Operator << " ";
      if (Node.Right) {
        printExpression(*Node.Right, 0);
      }
    } else if constexpr (std::is_same_v<T, CallExpr>) {
      if (Node.Callee) {
        printExpression(*Node.Callee, 30);
      }
      OS << "(";
      if (!Node.NamedArguments.empty()) {
        OS << "{";
        printNamedArguments(Node.NamedArguments);
        OS << "}";
      } else {
        printArguments(Node.Arguments);
      }
      OS << ")";
    } else if constexpr (std::is_same_v<T, NewExpr>) {
      OS << "new ";
      printType(Node.Type);
    } else if constexpr (std::is_same_v<T, TupleExpr>) {
      OS << "(";
      printArguments(Node.Elements);
      OS << ")";
    } else if constexpr (std::is_same_v<T, InlineArrayExpr>) {
      OS << "[";
      printArguments(Node.Elements);
      OS << "]";
    } else if constexpr (std::is_same_v<T, MetaTypeExpr>) {
      OS << "type(";
      printType(Node.Type);
      OS << ")";
    }
  }, Expr.Node);

  if (NeedsParens) {
    OS << ")";
  }
}

void Printer::printArguments(const std::vector<ExprPtr> &Args) {
  for (std::size_t I = 0; I < Args.size(); ++I) {
    if (I != 0) {
      OS << ", ";
    }
    if (Args[I]) {
      printExpression(*Args[I]);
    }
  }
}

void Printer::printNamedArguments(const std::vector<NamedArgument> &Args) {
  for (std::size_t I = 0; I < Args.size(); ++I) {
    if (I != 0) {
      OS << ", ";
    }
    OS << Args[I].Name << ": ";
    if (Args[I].Value) {
      printExpression(*Args[I].Value);
    }
  }
}

void Printer::printParameters(const std::vector<Parameter> &Params) {
  for (std::size_t I = 0; I < Params.size(); ++I) {
    if (I != 0) {
      OS << ", ";
    }
    printParameter(Params[I]);
  }
}

void Printer::printParameter(const Parameter &Param) {
  printType(Param.Type);
  if (!Param.DataLocation.empty()) {
    OS << " " << Param.DataLocation;
  }
  if (Param.Indexed) {
    OS << " indexed";
  }
  if (!Param.Name.empty()) {
    OS << " " << Param.Name;
  }
}

void Printer::printVariableDeclaration(const VariableDeclaration &Decl) {
  printType(Decl.Type);
  if (!Decl.DataLocation.empty()) {
    OS << " " << Decl.DataLocation;
  }
  if (!Decl.Name.empty()) {
    OS << " " << Decl.Name;
  }
}

void Printer::printType(const TypeRef &Type) {
  if (Type.Syntax) {
    printTypeName(*Type.Syntax);
    return;
  }
  OS << Type.Name;
}

void Printer::printTypeName(const TypeName &Type) {
  std::visit([this](const auto &Node) {
    using T = std::decay_t<decltype(Node)>;
    if constexpr (std::is_same_v<T, ElementaryTypeName>) {
      OS << Node.Name;
    } else if constexpr (std::is_same_v<T, FunctionTypeName>) {
      OS << "function(";
      for (std::size_t I = 0; I < Node.Parameters.size(); ++I) {
        if (I != 0) {
          OS << ", ";
        }
        printType(Node.Parameters[I]);
      }
      OS << ")";
      if (!Node.Visibility.empty()) {
        OS << " " << Node.Visibility;
      }
      if (!Node.StateMutability.empty()) {
        OS << " " << Node.StateMutability;
      }
      if (!Node.Returns.empty()) {
        OS << " returns (";
        for (std::size_t I = 0; I < Node.Returns.size(); ++I) {
          if (I != 0) {
            OS << ", ";
          }
          printType(Node.Returns[I]);
        }
        OS << ")";
      }
    } else if constexpr (std::is_same_v<T, MappingTypeName>) {
      OS << "mapping(";
      printType(Node.Key);
      if (!Node.KeyName.empty()) {
        OS << " " << Node.KeyName;
      }
      OS << " => ";
      printType(Node.Value);
      if (!Node.ValueName.empty()) {
        OS << " " << Node.ValueName;
      }
      OS << ")";
    } else if constexpr (std::is_same_v<T, UserDefinedTypeName>) {
      printIdentifierPath(Node.Name);
    } else if constexpr (std::is_same_v<T, ArrayTypeName>) {
      printType(Node.Base);
      OS << "[";
      if (Node.Size) {
        printExpression(*Node.Size);
      }
      OS << "]";
    }
  }, Type.Node);
}

void Printer::printIdentifierPath(const IdentifierPath &Path) {
  for (std::size_t I = 0; I < Path.Parts.size(); ++I) {
    if (I != 0) {
      OS << ".";
    }
    OS << Path.Parts[I];
  }
}

void Printer::printIndent() {
  for (unsigned I = 0; I < Indent; ++I) {
    OS << "    ";
  }
}

void Printer::printStatementIndent(unsigned ExtraIndent) {
  printIndent();
  for (unsigned I = 0; I < ExtraIndent; ++I) {
    OS << "  ";
  }
}

} // namespace notdec::backend::solidity
