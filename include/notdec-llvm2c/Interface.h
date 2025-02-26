/// The main interface headers
/// Do not include any clang headers, so that users do not need to install
/// clang.

#ifndef _NOTDEC_BACKEND_INTERFACE_H_
#define _NOTDEC_BACKEND_INTERFACE_H_

#include <clang/AST/DeclBase.h>
#include <clang/AST/Type.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <variant>
#include <vector>

#include "Interface/StructManager.h"
#include "Interface/ExtValuePtr.h"

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  StructuralAlgorithms algo;
};

struct HighTypes {
  std::map<ExtValuePtr, clang::QualType> ValueTypes;
  std::map<ExtValuePtr, clang::QualType> ValueTypesLowerBound;
  std::map<clang::Decl *, std::string> DeclComments;
  std::map<clang::Decl *, StructInfo> StructInfos;
  std::set<clang::Decl*> AllDecls;

  std::unique_ptr<clang::ASTUnit> ASTUnit;
  clang::QualType MemoryType;

  HighTypes() = default;
  HighTypes(HighTypes &&Other) = default;
  HighTypes &operator=(HighTypes &&Other) = default;
  void dump() const {
    auto PrintMap = [](const auto &Map) -> void {
      for (auto &VT : Map) {
        llvm::errs() << "  ";
        if (auto Val = std::get_if<llvm::Value *>(&VT.first)) {
          if (auto Arg = llvm::dyn_cast<llvm::Argument>(*Val)) {
            llvm::errs() << Arg->getParent()->getName() << ": ";
          } else if (auto Inst = llvm::dyn_cast<llvm::Instruction>(*Val)) {
            llvm::errs() << Inst->getParent()->getParent()->getName() << ": ";
          }
          llvm::errs() << **Val << ": ";
        } else if (auto Ret = std::get_if<ReturnValue>(&VT.first)) {
          llvm::errs() << Ret->Func->getName() << " ReturnValue: ";
        } else if (auto IC = std::get_if<UConstant>(&VT.first)) {
          llvm::errs() << *IC->Val << " -> ";
        }
        llvm::errs() << VT.second.getAsString() << "\n";
      }
    };

    llvm::errs() << "HighTypes.ValueTypes:\n";
    PrintMap(ValueTypes);
    llvm::errs() << "HighTypes.ValueTypesUpperBound:\n";
    PrintMap(ValueTypesLowerBound);
  }
};

// main interface
void decompileModule(llvm::Module &M, llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HighTypes> HT = nullptr);

void demoteSSA(llvm::Module &M);

} // namespace notdec::llvm2c

#endif
