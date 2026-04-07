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
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <tuple>
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
  bool filterUnusedDefinitions = false;
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
  void print(llvm::raw_ostream &OS) const {
    OS << "# HTypeResult\n\n";
    printDeclSection(OS);
    printValueSection(OS, "upper", ValueTypes);
    printValueSection(OS, "lower", ValueTypesLowerBound);
    printMemorySection(OS);
  }
  void dump() const { print(llvm::errs()); }

private:
  static std::string formatDecl(const ast::TypedDecl &Decl) {
    std::string Ret;
    llvm::raw_string_ostream SS(Ret);
    Decl.print(SS);
    SS.flush();
    return Ret;
  }

  static std::string formatValueKey(const ExtValuePtr &Value) {
    return toString(Value, true);
  }

  static std::string formatType(const HType *Type) {
    return Type == nullptr ? "<null>" : Type->getAsString();
  }

  void printDeclSection(llvm::raw_ostream &OS) const {
    OS << "[decls]\n";
    if (HTCtx != nullptr) {
      std::vector<const ast::TypedDecl *> Decls;
      Decls.reserve(HTCtx->getDecls().size());
      for (const auto &Ent : HTCtx->getDecls()) {
        Decls.push_back(Ent.second.get());
      }
      std::sort(Decls.begin(), Decls.end(),
                [](const ast::TypedDecl *LHS, const ast::TypedDecl *RHS) {
                  if (LHS->getName() != RHS->getName()) {
                    return LHS->getName() < RHS->getName();
                  }
                  return formatDecl(*LHS) < formatDecl(*RHS);
                });
      for (const auto *Decl : Decls) {
        OS << formatDecl(*Decl) << "\n";
      }
    }
    OS << "\n";
  }

  void printValueSection(llvm::raw_ostream &OS, llvm::StringRef Name,
                         const std::map<ExtValuePtr, HType *> &Map) const {
    OS << "[" << Name << "]\n";
    std::vector<std::pair<std::string, std::string>> Entries;
    Entries.reserve(Map.size());
    for (const auto &Ent : Map) {
      Entries.emplace_back(formatValueKey(Ent.first), formatType(Ent.second));
    }
    std::sort(Entries.begin(), Entries.end());
    for (const auto &Ent : Entries) {
      OS << Ent.first << " => " << Ent.second << "\n";
    }
    OS << "\n";
  }

  void printMemorySection(llvm::raw_ostream &OS) const {
    OS << "[memory]\n";
    if (MemoryDecl != nullptr) {
      OS << "decl => " << MemoryDecl->getName() << "\n";
    }
    if (MemoryType != nullptr) {
      OS << "type => " << formatType(MemoryType) << "\n";
    }
    OS << "\n";
  }
};

// main interface
void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HTypeResult> HT = nullptr);

void demoteSSA(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

} // namespace notdec::llvm2c

#endif
