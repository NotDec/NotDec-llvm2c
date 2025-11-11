#ifndef NOTDEC_LLVM2C_LOOP_GOTO_REWRITER_H
#define NOTDEC_LLVM2C_LOOP_GOTO_REWRITER_H

#include <vector>

#include "clang/AST/ASTContext.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"

namespace notdec::llvm2c {

/// Traverses statement lists that model the body of the loop currently being
/// translated and replaces matching `goto` statements with structured control
/// flow (break or continue).
class LoopGotoRewriter {
public:
  LoopGotoRewriter(clang::ASTContext &context, clang::LabelDecl *continueLabel,
                   clang::LabelDecl *breakLabel)
      : Context(context), ContinueLabel(continueLabel), BreakLabel(breakLabel) {
  }

  /// Recursively inspects the provided statement list. The method descends into
  /// nested `CompoundStmt` blocks reached from conditionals (if, switch, case,
  /// default). When it encounters a `goto` whose target label matches one of
  /// the stored loop labels, the statement is replaced in-place by either a
  /// `break` or `continue`. Loops nested inside the current loop are left
  /// untouched to preserve their own control flow.
  void Rewrite(std::vector<clang::Stmt *> &statements) const {
    Rewrite(llvm::MutableArrayRef<clang::Stmt *>(statements.data(),
                                                 statements.size()));
  }

  void Rewrite(llvm::MutableArrayRef<clang::Stmt *> statements) const {
    for (clang::Stmt *&stmt : statements) {
      if (clang::Stmt *rewritten = RewriteSingle(stmt))
        stmt = rewritten;
    }
  }

  void RewriteCompound(clang::CompoundStmt *compound) const {
    if (!compound)
      return;

    const unsigned count = compound->size();
    llvm::MutableArrayRef<clang::Stmt *> body(compound->body_begin(), count);
    Rewrite(body);
  }

private:
  clang::ASTContext &Context;
  clang::LabelDecl *ContinueLabel = nullptr;
  clang::LabelDecl *BreakLabel = nullptr;

  clang::Stmt *GetReplacementIfGoto(clang::Stmt *stmt) const {
    auto *gotoStmt = llvm::dyn_cast_or_null<clang::GotoStmt>(stmt);
    if (!gotoStmt)
      return nullptr;

    clang::LabelDecl *target = gotoStmt->getLabel();

    if (ContinueLabel != nullptr && target == ContinueLabel)
      return new (Context) clang::ContinueStmt(clang::SourceLocation());

    if (BreakLabel != nullptr && target == BreakLabel)
      return new (Context) clang::BreakStmt(clang::SourceLocation());

    return nullptr;
  }

  bool IsLoop(const clang::Stmt *stmt) const {
    if (!stmt)
      return false;
    switch (stmt->getStmtClass()) {
    case clang::Stmt::WhileStmtClass:
    case clang::Stmt::DoStmtClass:
    case clang::Stmt::ForStmtClass:
    case clang::Stmt::CXXForRangeStmtClass:
    case clang::Stmt::SwitchStmtClass:
      return true;
    default:
      return false;
    }
  }

  void RewriteIf(clang::IfStmt *ifStmt) const {
    if (!ifStmt)
      return;

    if (auto *thenCompound =
            llvm::dyn_cast_or_null<clang::CompoundStmt>(ifStmt->getThen()))
      RewriteCompound(thenCompound);
    else if (!IsLoop(ifStmt->getThen()))
      if (clang::Stmt *rewritten = RewriteSingle(ifStmt->getThen()))
        ifStmt->setThen(rewritten);

    if (auto *elseStmt = ifStmt->getElse()) {
      if (auto *elseCompound = llvm::dyn_cast<clang::CompoundStmt>(elseStmt))
        RewriteCompound(elseCompound);
      else if (!IsLoop(elseStmt))
        if (clang::Stmt *rewritten = RewriteSingle(elseStmt))
          ifStmt->setElse(rewritten);
    }
  }

  void RewriteSwitchCase(clang::SwitchCase *switchCase) const {
    if (!switchCase)
      return;

    if (auto *subStmt = switchCase->getSubStmt()) {
      if (auto *compound = llvm::dyn_cast<clang::CompoundStmt>(subStmt)) {
        RewriteCompound(compound);
      } else if (!IsLoop(subStmt)) {
        if (clang::Stmt *rewritten = RewriteSingle(subStmt))
          SetSwitchCaseSubStmt(switchCase, rewritten);
      }
    }
  }

public:
  clang::Stmt *RewriteSingle(clang::Stmt *stmt) const {
    if (!stmt)
      return nullptr;

    if (clang::Stmt *replacement = GetReplacementIfGoto(stmt))
      return replacement;

    if (IsLoop(stmt))
      return nullptr;

    if (auto *compound = llvm::dyn_cast<clang::CompoundStmt>(stmt)) {
      RewriteCompound(compound);
      return nullptr;
    }

    switch (stmt->getStmtClass()) {
    case clang::Stmt::IfStmtClass:
      RewriteIf(llvm::cast<clang::IfStmt>(stmt));
      break;
    case clang::Stmt::CaseStmtClass:
    case clang::Stmt::DefaultStmtClass:
      RewriteSwitchCase(llvm::cast<clang::SwitchCase>(stmt));
      break;
    default:
      break;
    }

    return nullptr;
  }

  static void SetSwitchCaseSubStmt(clang::SwitchCase *switchCase,
                                   clang::Stmt *stmt) {
    if (auto *caseStmt = llvm::dyn_cast<clang::CaseStmt>(switchCase)) {
      caseStmt->setSubStmt(stmt);
      return;
    }
    if (auto *defaultStmt = llvm::dyn_cast<clang::DefaultStmt>(switchCase)) {
      defaultStmt->setSubStmt(stmt);
      return;
    }
    llvm_unreachable("SwitchCase is neither CaseStmt nor DefaultStmt");
  }
};

} // namespace notdec::llvm2c

#endif // NOTDEC_LLVM2C_LOOP_GOTO_REWRITER_H
