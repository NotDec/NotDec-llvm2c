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
#include <set>
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
  std::string workDir;
};

using notdec::ast::HType;
// using notdec::ast::HTypeContext;

struct HTypeResult {
  std::shared_ptr<ast::HTypeContext> HTCtx;
  std::map<ExtValuePtr, HType *> ValueTypes;
  std::set<ExtValuePtr> ContraVariantValues;
  // std::map<clang::Decl *, std::string> DeclComments;
  // std::map<clang::Decl *, StructInfo> StructInfos;
  // std::set<clang::Decl*> AllDecls;

  HType *MemoryType;
  ast::RecordDecl *MemoryDecl;

  HTypeResult() = default;
  HTypeResult(HTypeResult &&Other) = default;
  HTypeResult &operator=(HTypeResult &&Other) = default;
  void print(llvm::raw_ostream &OS) const {
    // snapshot 导出需要跨多个 section 共享同一套 canonical 名。
    // 因此这里先创建并预热一个 formatter，再统一打印 decl/type/memory，
    // 避免同一个声明在不同 section 中拿到不同名字。
    ast::HTypeSnapshotFormatter Formatter(HTCtx.get());
    primeFormatter(Formatter);
    OS << "# HTypeResult\n\n";
    printDeclSection(OS, Formatter);
    printValueSection(OS, "types", ValueTypes, Formatter);
    printMemorySection(OS, Formatter);
  }
  void dump() const { print(llvm::errs()); }

private:
  static std::string formatValueKey(const ExtValuePtr &Value) {
    return toStableString(Value);
  }

  std::string formatValuePosition(const ExtValuePtr &Value) const {
    return ContraVariantValues.count(Value) != 0 ? "[-]" : "[+]";
  }

  void primeFormatter(ast::HTypeSnapshotFormatter &Formatter) const {
    auto CollectMap = [&](const std::map<ExtValuePtr, HType *> &Map) {
      std::vector<std::pair<std::string, const HType *>> Entries;
      Entries.reserve(Map.size());
      for (const auto &Ent : Map) {
        Entries.emplace_back(formatValueKey(Ent.first), Ent.second);
      }
      std::sort(Entries.begin(), Entries.end());
      for (const auto &Ent : Entries) {
        Formatter.collectType(Ent.second);
      }
    };

    CollectMap(ValueTypes);
    if (MemoryType != nullptr) {
      Formatter.collectType(MemoryType);
    }
    if (MemoryDecl != nullptr) {
      Formatter.collectDecl(*MemoryDecl);
    }
  }

  void printDeclSection(llvm::raw_ostream &OS,
                        ast::HTypeSnapshotFormatter &Formatter) const {
    OS << "[decls]\n";
    if (HTCtx != nullptr) {
      auto Decls = Formatter.getOrderedDecls();
      for (const auto *Decl : Decls) {
        OS << Formatter.formatDecl(*Decl) << "\n";
      }
    }
    OS << "\n";
  }

  void printValueSection(llvm::raw_ostream &OS, llvm::StringRef Name,
                         const std::map<ExtValuePtr, HType *> &Map,
                         ast::HTypeSnapshotFormatter &Formatter) const {
    OS << "[" << Name << "]\n";
    std::vector<std::tuple<std::string, std::string, std::string>> Entries;
    Entries.reserve(Map.size());
    for (const auto &Ent : Map) {
      Entries.emplace_back(formatValueKey(Ent.first),
                           formatValuePosition(Ent.first),
                           Formatter.formatType(Ent.second));
    }
    std::sort(Entries.begin(), Entries.end());
    for (const auto &Ent : Entries) {
      OS << std::get<1>(Ent) << " " << std::get<0>(Ent) << " => "
         << std::get<2>(Ent) << "\n";
    }
    OS << "\n";
  }

  void printMemorySection(llvm::raw_ostream &OS,
                          ast::HTypeSnapshotFormatter &Formatter) const {
    OS << "[memory]\n";
    if (MemoryDecl != nullptr) {
      OS << "decl => " << Formatter.formatDeclName(*MemoryDecl) << "\n";
    }
    if (MemoryType != nullptr) {
      OS << "type => " << Formatter.formatType(MemoryType) << "\n";
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
