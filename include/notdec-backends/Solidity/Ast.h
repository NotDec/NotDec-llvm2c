#ifndef NOTDEC_BACKENDS_SOLIDITY_AST_H
#define NOTDEC_BACKENDS_SOLIDITY_AST_H

#include <string>
#include <vector>

namespace notdec::backend::solidity {

// Minimal Solidity source model. Keep this independent from LLVM IR and HType
// so readers can fill it from different analysis results later.
struct Parameter {
  std::string Type;
  std::string Name;
};

struct StateVariable {
  std::string Type;
  std::string Name;
  std::string Visibility;
};

struct EventDecl {
  std::string Name;
  std::vector<Parameter> Parameters;
};

struct Function {
  std::string Name;
  std::string Visibility = "public";
  std::vector<Parameter> Parameters;
  std::vector<Parameter> Returns;
  std::vector<std::string> Body;
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
