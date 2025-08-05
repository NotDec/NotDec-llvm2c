
#ifndef _NOTDEC_BACKEND_CCODETRANSFORM_H_
#define _NOTDEC_BACKEND_CCODETRANSFORM_H_

#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/AST/ExprObjC.h"
#include "clang/AST/ExprOpenMP.h"
#include "clang/AST/OpenMPClause.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtObjC.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/Basic/DiagnosticParse.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/Sema/Designator.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Ownership.h"
#include "clang/Sema/ParsedTemplate.h"
#include "clang/Sema/ScopeInfo.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "clang/Sema/SemaInternal.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/ErrorHandling.h"
#include <clang/AST/ASTContext.h>
#include <llvm/Support/raw_ostream.h>

namespace notdec::llvm2c {

using namespace clang;

// A stub base class that raise unimplemented error for all TransformXX
// functions.
template <typename Derived> class StmtTransformBase {
protected:
  ASTContext &Context;

public:
  /// Initializes a new tree transformer.
  StmtTransformBase(ASTContext &Context) : Context(Context) {}

  /// Retrieves a reference to the derived class.
  Derived &getDerived() { return static_cast<Derived &>(*this); }
  /// Retrieves a reference to the derived class.
  const Derived &getDerived() const {
    return static_cast<const Derived &>(*this);
  }

  QualType TransformType(QualType T) { return T; }

  /// The reason why the value of a statement is not discarded, if any.
  enum StmtDiscardKind {
    SDK_Discarded,
    SDK_NotDiscarded,
    SDK_StmtExprResult,
  };

  /// Transform the given statement.
  ///
  /// By default, this routine transforms a statement by delegating to the
  /// appropriate TransformXXXStmt function to transform a specific kind of
  /// statement or the TransformExpr() function to transform an expression.
  /// Subclasses may override this function to transform statements using some
  /// other mechanism.
  ///
  /// \returns the transformed statement.
  StmtResult TransformStmt(Stmt *S, StmtDiscardKind SDK = SDK_Discarded);

  /// Transform the given expression.
  ///
  /// By default, this routine transforms an expression by delegating to the
  /// appropriate TransformXXXExpr function to build a new expression.
  /// Subclasses may override this function to transform expressions using some
  /// other mechanism.
  ///
  /// \returns the transformed expression.
  ExprResult TransformExpr(Expr *E);

#define STMT(Node, Parent)                                                     \
  StmtResult Transform##Node(Node *S) {                                        \
    llvm::errs() << "Unimplemented: Transform##Node\n";                        \
    assert(false && "Unimplemented StmtTransform!");                           \
  }
#define VALUESTMT(Node, Parent)                                                \
  StmtResult Transform##Node(Node *S, StmtDiscardKind SDK) {                   \
    llvm::errs() << "Unimplemented: Transform##Node\n";                        \
    assert(false && "Unimplemented StmtTransform!");                           \
  }
#define ABSTRACT_STMT(Stmt)
#define EXPR(Node, Parent)                                                     \
  ExprResult Transform##Node(Node *E) {                                        \
    llvm::errs() << "Unimplemented: Transform##Node\n";                        \
    assert(false && "Unimplemented StmtTransform!");                           \
  }
#include "clang/AST/StmtNodes.inc"
};

template <typename Derived>
StmtResult StmtTransformBase<Derived>::TransformStmt(Stmt *S,
                                                     StmtDiscardKind SDK) {
  using namespace clang;
  if (!S)
    return S;

  switch (S->getStmtClass()) {
  case Stmt::NoStmtClass:
    break;

    // Transform individual statement nodes
    // Pass SDK into statements that can produce a value
#define STMT(Node, Parent)                                                     \
  case Stmt::Node##Class:                                                      \
    return getDerived().Transform##Node(cast<Node>(S));
#define VALUESTMT(Node, Parent)                                                \
  case Stmt::Node##Class:                                                      \
    return getDerived().Transform##Node(cast<Node>(S), SDK);
#define ABSTRACT_STMT(Node)
#define EXPR(Node, Parent)
#include "clang/AST/StmtNodes.inc"

    // Transform expressions by calling TransformExpr.
#define STMT(Node, Parent)
#define ABSTRACT_STMT(Stmt)
#define EXPR(Node, Parent) case Stmt::Node##Class:
#include "clang/AST/StmtNodes.inc"
    {
      ExprResult E = getDerived().TransformExpr(cast<Expr>(S));
      return E.get();
    }
  }

  return S;
}

template <typename Derived>
ExprResult StmtTransformBase<Derived>::TransformExpr(Expr *E) {
  using namespace clang;
  if (!E)
    return E;

  switch (E->getStmtClass()) {
  case Stmt::NoStmtClass:
    break;
#define STMT(Node, Parent)                                                     \
  case Stmt::Node##Class:                                                      \
    break;
#define ABSTRACT_STMT(Stmt)
#define EXPR(Node, Parent)                                                     \
  case Stmt::Node##Class:                                                      \
    return getDerived().Transform##Node(cast<Node>(E));
#include "clang/AST/StmtNodes.inc"
  }

  return E;
}

// For all stmts, recursive transform its sub-stmt or sub-expr, check if is old
// value(pointer equal), if not, we reconstruct the node using
// getDerived().TransformStmt.
// !! Use TransformStmt instead of TransformExpr
template <typename Derived>
class StmtTransform : public StmtTransformBase<Derived> {
  bool IsAlwaysRebuild = false;

public:
  StmtTransform(ASTContext &Context, bool IsAlwaysRebuild = false)
      : StmtTransformBase<Derived>(Context), IsAlwaysRebuild(IsAlwaysRebuild) {}
  bool AlwaysRebuild() { return IsAlwaysRebuild; }

  StmtResult TransformReturnStmt(clang::ReturnStmt *S) {
    auto OldVal = S->getRetValue();
    ActionResult<clang::Expr *> ValR = this->getDerived().TransformExpr(OldVal);
    if (ValR.isInvalid())
      return StmtError();
    auto Val = ValR.get();
    if (OldVal == Val) {
      return S;
    } else {
      return clang::ReturnStmt::Create(this->Context, S->getReturnLoc(), Val,
                                       S->getNRVOCandidate());
    }
  }

  ExprResult TransformDeclRefExpr(clang::DeclRefExpr *E) { return E; }

  ExprResult TransformBinaryOperator(clang::BinaryOperator *E) {
    ExprResult LHS = this->getDerived().TransformExpr(E->getLHS());
    if (LHS.isInvalid())
      return ExprError();
    ExprResult RHS = this->getDerived().TransformExpr(E->getRHS());
    if (RHS.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && LHS.get() == E->getLHS() &&
        RHS.get() == E->getRHS())
      return E;

    return BinaryOperator::Create(
        this->Context, LHS.get(), RHS.get(), E->getOpcode(), E->getType(),
        E->getValueKind(), E->getObjectKind(), E->getOperatorLoc(),
        E->getStoredFPFeatures());
  }

  ExprResult TransformIntegerLiteral(clang::IntegerLiteral *E) { return E; }
};

// a simple class that always rewrite the stmt;
class SimpleRewriter : public StmtTransform<SimpleRewriter> {
public:
  SimpleRewriter(ASTContext &Context)
      : StmtTransform<SimpleRewriter>(Context, true) {}
};

} // namespace notdec::llvm2c

#endif
