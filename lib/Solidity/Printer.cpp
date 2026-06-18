#include "notdec-backends/Solidity/Printer.h"

namespace notdec::backend::solidity {

void Printer::print(const SourceUnit &Unit) {
  for (std::size_t I = 0; I < Unit.Contracts.size(); ++I) {
    if (I != 0) {
      OS << "\n";
    }
    printContract(Unit.Contracts[I]);
  }
}

void Printer::printContract(const Contract &Contract) {
  OS << "contract " << Contract.Name << " {\n";
  ++Indent;
  for (const auto &Event : Contract.Events) {
    printEvent(Event);
  }
  if (!Contract.Events.empty() && !Contract.StateVariables.empty()) {
    OS << "\n";
  }
  for (const auto &Var : Contract.StateVariables) {
    printStateVariable(Var);
  }
  if ((!Contract.Events.empty() || !Contract.StateVariables.empty()) &&
      !Contract.Functions.empty()) {
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

void Printer::printStateVariable(const StateVariable &Var) {
  printIndent();
  OS << Var.Type;
  if (!Var.Visibility.empty()) {
    OS << " " << Var.Visibility;
  }
  OS << " " << Var.Name << ";\n";
}

void Printer::printEvent(const EventDecl &Event) {
  printIndent();
  OS << "event " << Event.Name << "(";
  printParameters(Event.Parameters);
  OS << ");\n";
}

void Printer::printFunction(const Function &Func) {
  printIndent();
  OS << "function " << Func.Name << "(";
  printParameters(Func.Parameters);
  OS << ")";
  if (!Func.Visibility.empty()) {
    OS << " " << Func.Visibility;
  }
  if (!Func.Returns.empty()) {
    OS << " returns (";
    printParameters(Func.Returns);
    OS << ")";
  }
  OS << " {\n";
  ++Indent;
  for (const auto &Stmt : Func.Body) {
    printIndent();
    OS << Stmt << "\n";
  }
  --Indent;
  printIndent();
  OS << "}\n";
}

void Printer::printParameters(const std::vector<Parameter> &Params) {
  for (std::size_t I = 0; I < Params.size(); ++I) {
    if (I != 0) {
      OS << ", ";
    }
    OS << Params[I].Type;
    if (!Params[I].Name.empty()) {
      OS << " " << Params[I].Name;
    }
  }
}

void Printer::printIndent() {
  for (unsigned I = 0; I < Indent; ++I) {
    OS << "    ";
  }
}

} // namespace notdec::backend::solidity
