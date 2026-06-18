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

  void printContract(const Contract &Contract);
  void printEvent(const EventDecl &Event);
  void printStateVariable(const StateVariable &Var);
  void printFunction(const Function &Func);
  void printParameters(const std::vector<Parameter> &Params);
  void printIndent();
};

} // namespace notdec::backend::solidity

#endif
