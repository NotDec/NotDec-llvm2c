#ifndef NOTDEC_BACKENDS_SOLIDITY_AST_NODES_H
#define NOTDEC_BACKENDS_SOLIDITY_AST_NODES_H

#include <string>
#include <variant>
#include <vector>

namespace notdec::backend::solidity {

// Solidity type text used by the decompiler output.  This is intentionally much
// smaller than solc's type system: the backend often knows only the printable
// ABI/storage type, so the first AST layer stores that stable spelling instead
// of pretending full semantic type checking has happened.
struct TypeRef {
  std::string Name;
};

struct Parameter {
  TypeRef Type;
  std::string Name;
};

struct StateVariable {
  TypeRef Type;
  std::string Name;
  std::string Visibility;
};

struct EventDecl {
  std::string Name;
  std::vector<Parameter> Parameters;
};

// RawStatement is the bridge from the current string-based body builder.  New
// statement kinds should be added next to it as each small Solidity case proves
// the recovery rule, rather than growing ad-hoc strings in the printer.
struct RawStatement {
  std::string Text;
};

using Statement = std::variant<RawStatement>;

struct Block {
  std::vector<Statement> Statements;
};

struct Function {
  std::string Name;
  std::string Visibility = "public";
  std::vector<Parameter> Parameters;
  std::vector<Parameter> Returns;
  Block Body;
};

struct Contract {
  std::string Name = "Decompiled";
  std::vector<EventDecl> Events;
  std::vector<StateVariable> StateVariables;
  std::vector<Function> Functions;
};

struct SourceUnit {
  std::vector<Contract> Contracts;
};

} // namespace notdec::backend::solidity

#endif
