#pragma once

#include <map>
#include <set>

#include "clang/AST/RecursiveASTVisitor.h"

namespace notdec::llvm2c {

/// Counts declarations referenced in global initializers while allowing
/// specific declarations to be ignored in subsequent visits.
class DeclarationCounterVisitor
    : public clang::RecursiveASTVisitor<DeclarationCounterVisitor> {
public:
  using CounterMap = std::map<const clang::Decl *, int>;
  using IgnoreSet = std::set<const clang::Decl *>;

  /// Resets both the counter map and the ignore set.
  void reset() {
    Counts.clear();
    Ignored.clear();
  }

  /// Removes all accumulated counts but keeps the ignore set intact.
  void clearCounts() { Counts.clear(); }

  /// Marks a declaration as ignored for future visits.
  void ignoreDecl(const clang::Decl *D) {
    if (D)
      Ignored.insert(D);
  }

  /// Stops ignoring a declaration.
  void unignoreDecl(const clang::Decl *D) {
    if (D)
      Ignored.erase(D);
  }

  const CounterMap &getCounts() const { return Counts; }

  int getCount(const clang::Decl *D) const {
    const auto It = Counts.find(D);
    return It == Counts.end() ? 0 : It->second;
  }

  bool VisitDeclRefExpr(clang::DeclRefExpr *Ref) {
    clang::RecursiveASTVisitor<DeclarationCounterVisitor>::VisitDeclRefExpr(Ref);
    const clang::ValueDecl *Target = Ref->getDecl();
    if (!Target)
      return true;
    if (Ignored.count(Target) == 0)
      ++Counts[Target];
    return true;
  }

private:
  CounterMap Counts;
  IgnoreSet Ignored;
};

/// Tracks struct/union type references inside other record definitions to find
/// which types must be forward declared.
class TypeCounterVisitor
    : public clang::RecursiveASTVisitor<TypeCounterVisitor> {
public:
  using CounterMap = std::map<const clang::TagDecl *, int>;
  using IgnoreSet = std::set<const clang::TagDecl *>;

  void reset() {
    Counts.clear();
    Ignored.clear();
  }

  void clearCounts() { Counts.clear(); }

  void ignoreDecl(const clang::TagDecl *D) {
    if (D)
      Ignored.insert(D);
  }

  void unignoreDecl(const clang::TagDecl *D) {
    if (D)
      Ignored.erase(D);
  }

  const CounterMap &getCounts() const { return Counts; }

  int getCount(const clang::TagDecl *D) const {
    const auto It = Counts.find(D);
    return It == Counts.end() ? 0 : It->second;
  }

  bool VisitRecordType(clang::RecordType *Ty) {
    const clang::TagDecl *Tag = Ty->getDecl();
    if (!Tag)
      return true;
    if (Ignored.count(Tag) == 0)
      ++Counts[Tag];
    return true;
  }

private:
  CounterMap Counts;
  IgnoreSet Ignored;
};

} // namespace notdec::llvm2c
