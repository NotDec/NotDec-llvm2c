#ifndef NOTDEC_BACKENDS_CORE_HTYPERESULT_H
#define NOTDEC_BACKENDS_CORE_HTYPERESULT_H

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/raw_ostream.h>

#include "notdec-backends/Core/ExtValuePtr.h"
#include "notdec-backends/Core/HType.h"

namespace notdec::llvm2c {

using notdec::ast::HType;

// Shared recovered high-level type result passed from type recovery to
// backends. It still lives in the legacy llvm2c namespace for compatibility;
// the header location is now backend-core so non-C backends can include it
// without pulling in the full C backend interface.
struct HTypeResult {
  std::shared_ptr<ast::HTypeContext> HTCtx;
  // `pos=true` follows lower bounds and computes their least common upper
  // bound. In the user-facing lattice naming this is the lower-side result.
  std::map<ExtValuePtr, HType *> ValueTypesLower;
  // `pos=false` follows upper bounds and computes their greatest common lower
  // bound. In the user-facing lattice naming this is the upper-side result.
  std::map<ExtValuePtr, HType *> ValueTypesUpper;
  std::set<ExtValuePtr> ContraVariantValues;

  HType *MemoryType = nullptr;
  ast::RecordDecl *MemoryDecl = nullptr;
  HType *StorageType = nullptr;
  ast::RecordDecl *StorageDecl = nullptr;

  HTypeResult() = default;
  HTypeResult(HTypeResult &&Other) = default;
  HTypeResult &operator=(HTypeResult &&Other) = default;

  bool prefersUpperValueType(const ExtValuePtr &Value) const {
    return ContraVariantValues.count(Value) != 0;
  }
  bool hasValueType(const ExtValuePtr &Value, bool Lower) const {
    const auto &Map = Lower ? ValueTypesLower : ValueTypesUpper;
    return Map.count(Value) != 0;
  }
  HType *getValueType(const ExtValuePtr &Value, bool Lower) const {
    const auto &Map = Lower ? ValueTypesLower : ValueTypesUpper;
    auto It = Map.find(Value);
    return It == Map.end() ? nullptr : It->second;
  }
  bool hasDefaultValueType(const ExtValuePtr &Value) const {
    return hasValueType(Value, !prefersUpperValueType(Value));
  }
  HType *getDefaultValueType(const ExtValuePtr &Value) const {
    auto *Ty = getValueType(Value, !prefersUpperValueType(Value));
    // Bottom carries little information for C emission. If the default choice
    // falls to bottom, try the upper-side result before giving up.
    if (Ty != nullptr && Ty->isBottomType()) {
      if (auto *UpperTy = getValueType(Value, false)) {
        return UpperTy;
      }
    }
    return Ty;
  }
  template <class Pred>
  void eraseValueTypesIf(Pred &&ShouldErase) {
    for (auto It = ValueTypesLower.begin(); It != ValueTypesLower.end();) {
      if (ShouldErase(It->first)) {
        It = ValueTypesLower.erase(It);
      } else {
        ++It;
      }
    }
    for (auto It = ValueTypesUpper.begin(); It != ValueTypesUpper.end();) {
      if (ShouldErase(It->first)) {
        It = ValueTypesUpper.erase(It);
      } else {
        ++It;
      }
    }
  }
  template <class Pred>
  void eraseContraVariantValuesIf(Pred &&ShouldErase) {
    for (auto It = ContraVariantValues.begin(); It != ContraVariantValues.end();) {
      if (ShouldErase(*It)) {
        It = ContraVariantValues.erase(It);
      } else {
        ++It;
      }
    }
  }
  void eraseTypesForDemotedValues(const std::set<const llvm::Value *> &Values) {
    auto References = [&Values](const ExtValuePtr &Value) {
      if (auto *V = std::get_if<llvm::Value *>(&Value)) {
        return *V != nullptr && Values.count(*V) != 0;
      }
      if (auto *C = std::get_if<UConstant>(&Value)) {
        return (C->Val != nullptr && Values.count(C->Val) != 0) ||
               (C->User != nullptr && Values.count(C->User) != 0);
      }
      if (auto *S = std::get_if<StackObject>(&Value)) {
        return S->Allocator != nullptr && Values.count(S->Allocator) != 0;
      }
      if (auto *H = std::get_if<HeapObject>(&Value)) {
        return H->Allocator != nullptr && Values.count(H->Allocator) != 0;
      }
      return false;
    };
    eraseValueTypesIf(References);
    eraseContraVariantValuesIf(References);
  }
  void print(llvm::raw_ostream &OS) const {
    // snapshot 导出需要跨多个 section 共享同一套 canonical 名。
    // 因此这里先创建并预热一个 formatter，再统一打印 decl/type/memory，
    // 避免同一个声明在不同 section 中拿到不同名字。
    ast::HTypeSnapshotFormatter Formatter(HTCtx.get());
    primeFormatter(Formatter);
    OS << "# HTypeResult\n\n";
    printDeclSection(OS, Formatter);
    printValueSection(OS, "types", Formatter);
    printMemorySection(OS, Formatter);
    printStorageSection(OS, Formatter);
  }
  void dump() const { print(llvm::errs()); }

private:
  static std::string formatValueKey(const ExtValuePtr &Value) {
    return toStableString(Value);
  }

  std::string formatValuePosition(const ExtValuePtr &Value) const {
    return ContraVariantValues.count(Value) != 0 ? "[-]" : "[+]";
  }

  static std::string formatMaybeType(const HType *Ty,
                                     ast::HTypeSnapshotFormatter &Formatter) {
    if (Ty == nullptr) {
      return "<null>";
    }
    return Formatter.formatType(Ty);
  }

  void primeFormatter(ast::HTypeSnapshotFormatter &Formatter) const {
    auto CollectMap = [&](const std::map<ExtValuePtr, HType *> &Map) {
      std::vector<std::pair<std::string, const HType *>> Entries;
      Entries.reserve(Map.size());
      for (const auto &Ent : Map) {
        if (Ent.second == nullptr) {
          continue;
        }
        Entries.emplace_back(formatValueKey(Ent.first), Ent.second);
      }
      std::sort(Entries.begin(), Entries.end());
      for (const auto &Ent : Entries) {
        Formatter.collectType(Ent.second);
      }
    };

    CollectMap(ValueTypesLower);
    CollectMap(ValueTypesUpper);
    if (MemoryType != nullptr) {
      Formatter.collectType(MemoryType);
    }
    if (MemoryDecl != nullptr) {
      Formatter.collectDecl(*MemoryDecl);
    }
    if (StorageType != nullptr) {
      Formatter.collectType(StorageType);
    }
    if (StorageDecl != nullptr) {
      Formatter.collectDecl(*StorageDecl);
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
                         ast::HTypeSnapshotFormatter &Formatter) const {
    OS << "[" << Name << "]\n";
    std::set<ExtValuePtr> Values;
    for (const auto &Ent : ValueTypesLower) {
      Values.insert(Ent.first);
    }
    for (const auto &Ent : ValueTypesUpper) {
      Values.insert(Ent.first);
    }
    std::vector<std::tuple<std::string, std::string, std::string, std::string>>
        Entries;
    Entries.reserve(Values.size());
    for (const auto &Value : Values) {
      Entries.emplace_back(formatValueKey(Value), formatValuePosition(Value),
                           formatMaybeType(getValueType(Value, true),
                                           Formatter),
                           formatMaybeType(getValueType(Value, false),
                                           Formatter));
    }
    std::sort(Entries.begin(), Entries.end());
    for (const auto &Ent : Entries) {
      OS << std::get<1>(Ent) << " " << std::get<0>(Ent)
         << " => lower=" << std::get<2>(Ent)
         << " ; upper=" << std::get<3>(Ent) << "\n";
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

  void printStorageSection(llvm::raw_ostream &OS,
                           ast::HTypeSnapshotFormatter &Formatter) const {
    OS << "[storage]\n";
    if (StorageDecl != nullptr) {
      OS << "decl => " << Formatter.formatDeclName(*StorageDecl) << "\n";
    }
    if (StorageType != nullptr) {
      OS << "type => " << Formatter.formatType(StorageType) << "\n";
    }
    OS << "\n";
  }
};

} // namespace notdec::llvm2c

#endif
