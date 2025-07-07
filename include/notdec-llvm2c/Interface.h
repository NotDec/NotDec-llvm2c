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
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <variant>
#include <vector>

#include "Interface/ExtValuePtr.h"
#include "Interface/StructManager.h"
#include "notdec-llvm2c/Interface/HType.h"

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  StructuralAlgorithms algo;
};

using notdec::ast::HType;
// using notdec::ast::HTypeContext;

struct HTypeResult {
  std::shared_ptr<ast::HTypeContext> HTCtx;
  std::map<ExtValuePtr, HType *> ValueTypes;
  std::map<ExtValuePtr, HType *> ValueTypesLowerBound;
  // std::map<clang::Decl *, std::string> DeclComments;
  // std::map<clang::Decl *, StructInfo> StructInfos;
  // std::set<clang::Decl*> AllDecls;

  HType *MemoryType;
  ast::RecordDecl *MemoryDecl;

  HTypeResult() = default;
  HTypeResult(HTypeResult &&Other) = default;
  HTypeResult &operator=(HTypeResult &&Other) = default;
  void dump() const {
    llvm::errs() << "Current Type definitions:\n";
    HTCtx->printDecls(llvm::errs());
    llvm::errs() << "\n";
    auto PrintMap = [](const std::map<ExtValuePtr, HType *> &Map) -> void {
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
        llvm::errs() << VT.second->getAsString() << "\n";
      }
    };

    llvm::errs() << "HighTypes.ValueTypes:\n";
    PrintMap(ValueTypes);
    llvm::errs() << "HighTypes.ValueTypesUpperBound:\n";
    PrintMap(ValueTypesLowerBound);
  }
};

// main interface
void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HTypeResult> HT = nullptr);

void demoteSSA(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

} // namespace notdec::llvm2c

#endif
