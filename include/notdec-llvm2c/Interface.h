/// The main interface headers
/// Do not include any clang headers, so that users do not need to install
/// clang.

#ifndef _NOTDEC_BACKEND_INTERFACE_H_
#define _NOTDEC_BACKEND_INTERFACE_H_

#include <clang/AST/Type.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  StructuralAlgorithms algo;
};

struct HighTypes {
  std::map<llvm::Value *, clang::QualType> ValueTypes;
  std::map<llvm::Function *, clang::QualType> FuncRetTypes;
  std::unique_ptr<clang::ASTUnit> ASTUnit;

  HighTypes() = default;
  HighTypes(HighTypes &&Other) = default;
  HighTypes &operator=(HighTypes &&Other) = default;
  void dump() const {
    llvm::errs() << "HighTypes.ValueTypes:\n";
    for (auto &VT : ValueTypes) {
      llvm::errs() << "  ";
      if (auto Arg = llvm::dyn_cast<llvm::Argument>(VT.first)) {
        llvm::errs() << Arg->getParent()->getName() << ": ";
      } else if (auto Inst = llvm::dyn_cast<llvm::Instruction>(VT.first)) {
        llvm::errs() << Inst->getParent()->getParent()->getName() << ": ";
      }
      llvm::errs() << *VT.first << " -> " << VT.second.getAsString() << "\n";
    }
    llvm::errs() << "HighTypes.FuncRetTypes:\n";
    for (auto &FT : FuncRetTypes) {
      llvm::errs() << "  " << FT.first->getName() << " -> "
                   << FT.second.getAsString() << "\n";
    }
  }
};

// main interface
void decompileModule(llvm::Module &M, llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HighTypes> HT = nullptr);

void demoteSSA(llvm::Module &M);

} // namespace notdec::llvm2c

#endif
