
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
#include <clang/AST/OperationKinds.h>
#include <cstdint>
#include <functional>
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
    llvm::errs() << "Unimplemented: Transform" << #Node << "\n";               \
    assert(false && "Unimplemented StmtTransform!");                           \
  }
#define VALUESTMT(Node, Parent)                                                \
  StmtResult Transform##Node(Node *S, StmtDiscardKind SDK) {                   \
    llvm::errs() << "Unimplemented: Transform" << #Node << "\n";               \
    assert(false && "Unimplemented StmtTransform!");                           \
  }
#define ABSTRACT_STMT(Stmt)
#define EXPR(Node, Parent)                                                     \
  ExprResult Transform##Node(Node *E) {                                        \
    llvm::errs() << "Unimplemented: Transform" << #Node << "\n";               \
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

  ExprResult TransformDeclRefExpr(clang::DeclRefExpr *E) {
    if (!this->getDerived().AlwaysRebuild())
      return E;
    clang::ValueDecl *D = E->getDecl();
    clang::QualType Type = D->getType().getNonReferenceType();
    clang::DeclRefExpr *NewE = clang::DeclRefExpr::Create(
        this->Context, E->getQualifierLoc(), E->getTemplateKeywordLoc(), D,
        E->refersToEnclosingVariableOrCapture(), E->getLocation(), Type,
        E->getValueKind());
    return NewE;
  }

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
        E->getFPFeatures(this->Context.getLangOpts()));
  }

  ExprResult TransformMemberExpr(clang::MemberExpr *E) {
    ExprResult B = this->getDerived().TransformExpr(E->getBase());
    if (B.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && B.get() == E->getBase())
      return E;

    return MemberExpr::Create(
        this->Context, B.get(), E->isArrow(), E->getOperatorLoc(),
        E->getQualifierLoc(), E->getTemplateKeywordLoc(), E->getMemberDecl(),
        E->getFoundDecl(), E->getMemberNameInfo(), nullptr, E->getType(),
        E->getValueKind(), E->getObjectKind(), E->isNonOdrUse());
  }

  ExprResult TransformUnaryOperator(clang::UnaryOperator *E) {
    ExprResult SE = this->getDerived().TransformExpr(E->getSubExpr());
    if (SE.isInvalid())
      return ExprError();
    if (!this->getDerived().AlwaysRebuild() && SE.get() == E->getSubExpr())
      return E;
    return UnaryOperator::Create(
        this->Context, SE.get(), E->getOpcode(), E->getType(),
        E->getValueKind(), E->getObjectKind(), E->getSourceRange().getBegin(),
        E->canOverflow(), E->getFPOptionsOverride());
  }

  ExprResult TransformCallExpr(clang::CallExpr *E) {
    SmallVector<Expr *, 8> Args;
    bool AllArgsEqual = true;
    for (auto *Arg : E->arguments()) {
      ExprResult R = this->getDerived().TransformExpr(Arg);
      if (R.isInvalid())
        return ExprError();
      if (R.get() != Arg) {
        AllArgsEqual = false;
      }
      Args.push_back(R.get());
    }

    ExprResult C = this->getDerived().TransformExpr(E->getCallee());
    if (C.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && C.get() == E->getCallee() &&
        AllArgsEqual)
      return E;

    return CallExpr::Create(this->Context, C.get(), Args, E->getType(),
                            E->getValueKind(), E->getRParenLoc(),
                            E->getFPFeatures(), 0, E->getADLCallKind());
  }

  ExprResult TransformCStyleCastExpr(clang::CStyleCastExpr *E) {
    ExprResult Sub = this->getDerived().TransformExpr(E->getSubExpr());
    if (Sub.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && Sub.get() == E->getSubExpr())
      return E;

    return CStyleCastExpr::Create(
        this->Context, E->getType(), E->getValueKind(), E->getCastKind(),
        Sub.get(), nullptr, E->getFPFeatures(), E->getTypeInfoAsWritten(),
        E->getLParenLoc(), E->getRParenLoc());
  }

  ExprResult TransformImplicitCastExpr(clang::ImplicitCastExpr *E) {
    ExprResult Sub = this->getDerived().TransformExpr(E->getSubExpr());
    if (Sub.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && Sub.get() == E->getSubExpr())
      return E;

    return ImplicitCastExpr::Create(this->Context, E->getType(),
                                    E->getCastKind(), Sub.get(), nullptr,
                                    E->getValueKind(), E->getFPFeatures());
  }

  ExprResult TransformArraySubscriptExpr(clang::ArraySubscriptExpr *E) {
    ExprResult Base = this->getDerived().TransformExpr(E->getBase());
    if (Base.isInvalid())
      return ExprError();
    ExprResult Idx = this->getDerived().TransformExpr(E->getIdx());
    if (Idx.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && Base.get() == E->getBase() &&
        Idx.get() == E->getIdx())
      return E;

    return new (this->Context) ArraySubscriptExpr(
        Base.get(), Idx.get(), E->getType(), E->getValueKind(),
        E->getObjectKind(), E->getRBracketLoc());
  }

  ExprResult TransformParenExpr(clang::ParenExpr *E) {
    ExprResult Sub = this->getDerived().TransformExpr(E->getSubExpr());
    if (Sub.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && Sub.get() == E->getSubExpr())
      return E;

    return new (this->Context)
        ParenExpr(E->getLParen(), E->getRParen(), Sub.get());
  }

  ExprResult TransformIntegerLiteral(clang::IntegerLiteral *E) { return E; }

  ExprResult TransformCXXBoolLiteralExpr(clang::CXXBoolLiteralExpr *E) {
    return E;
  }

  ExprResult TransformFloatingLiteral(clang::FloatingLiteral *E) { return E; }

  ExprResult TransformConditionalOperator(clang::ConditionalOperator *E) {
    ExprResult Cond = this->getDerived().TransformExpr(E->getCond());
    if (Cond.isInvalid())
      return ExprError();
    ExprResult LHS = this->getDerived().TransformExpr(E->getTrueExpr());
    if (LHS.isInvalid())
      return ExprError();
    ExprResult RHS = this->getDerived().TransformExpr(E->getFalseExpr());
    if (RHS.isInvalid())
      return ExprError();

    if (!this->getDerived().AlwaysRebuild() && Cond.get() == E->getCond() &&
        LHS.get() == E->getTrueExpr() && RHS.get() == E->getFalseExpr())
      return E;

    return new (this->Context) ConditionalOperator(
        Cond.get(), E->getQuestionLoc(), LHS.get(), E->getColonLoc(), RHS.get(),
        E->getType(), E->getValueKind(), E->getObjectKind());
  }
};

// a simple class that always rewrite the stmt;
class SimpleRewriter : public StmtTransform<SimpleRewriter> {
public:
  SimpleRewriter(ASTContext &Context)
      : StmtTransform<SimpleRewriter>(Context, true) {}
};

// transform index expr to subscript by divide expected size. Used by
// tryDivideBySize.
class DivideTransform : public StmtTransformBase<DivideTransform> {
  int Size = 0;

public:
  DivideTransform(ASTContext &Context, int Size)
      : StmtTransformBase<DivideTransform>(Context), Size(Size) {
    assert(Size != 0 && Size != 1);
  }

  ExprResult TransformIntegerLiteral(clang::IntegerLiteral *E) {
    auto IntVal = E->getValue().getSExtValue();
    if (IntVal % Size == 0) {
      return clang::IntegerLiteral::Create(
          Context, llvm::APInt(64, IntVal / Size, false), Context.IntTy,
          clang::SourceLocation());
    } else {
      return ExprError();
    }
  }

  // static std::set<> UnsupportedOps;

  ExprResult TransformBinaryOperator(clang::BinaryOperator *E) {
    if (E->getOpcode() == clang::BO_Mul) {
      // either op can be divided is ok.
      Expr *LHS = E->getLHS();
      Expr *RHS = E->getRHS();
      // try divide left
      ExprResult LHSR = this->getDerived().TransformExpr(LHS);
      if (!LHSR.isInvalid()) {
        LHS = LHSR.get();
        return BinaryOperator::Create(
            this->Context, LHS, RHS, E->getOpcode(), E->getType(),
            E->getValueKind(), E->getObjectKind(), E->getOperatorLoc(),
            E->getFPFeatures(this->Context.getLangOpts()));
      }

      ExprResult RHSR = this->getDerived().TransformExpr(RHS);
      if (!RHSR.isInvalid()) {
        RHS = RHSR.get();
        return BinaryOperator::Create(
            this->Context, LHS, RHS, E->getOpcode(), E->getType(),
            E->getValueKind(), E->getObjectKind(), E->getOperatorLoc(),
            E->getFPFeatures(this->Context.getLangOpts()));
      }
    } else if (E->getOpcode() == clang::BO_Shl) {
      clang::Expr *RHS = E->getRHS();
      while (auto Cast = llvm::dyn_cast<clang::CastExpr>(RHS)) {
        RHS = Cast->getSubExpr();
      }
      if (auto IL = llvm::dyn_cast<clang::IntegerLiteral>(RHS)) {
        auto Val = IL->getValue().getZExtValue();
        assert(Val < 64);
        std::function<uint64_t(uint64_t, uint64_t)> intPow =
            [&](uint64_t x, uint64_t p) -> uint64_t {
          if (p == 0)
            return 1;
          if (p == 1)
            return x;

          int tmp = intPow(x, p / 2);
          if (p % 2 == 0)
            return tmp * tmp;
          else
            return x * tmp * tmp;
        };
        uint64_t Mul = intPow(2, Val);
        if (Mul == Size) {
          return E->getLHS();
        } else if (Mul % Size == 0) {
          Mul = Mul / Size;
          auto MulC = clang::IntegerLiteral::Create(
              Context, llvm::APInt(64, Mul, false), RHS->getType(),
              clang::SourceLocation());
          // convert to multiply
          return BinaryOperator::Create(
              this->Context, E->getLHS(), MulC, BO_Mul, E->getType(),
              E->getValueKind(), E->getObjectKind(), E->getOperatorLoc(),
              E->getFPFeatures(this->Context.getLangOpts()));
        }
      }
      // cannot divide RHS, try divide LHS
      ExprResult LHSR = this->getDerived().TransformExpr(E->getLHS());
      if (!LHSR.isInvalid()) {
        return BinaryOperator::Create(
            this->Context, LHSR.get(), E->getRHS(), E->getOpcode(),
            E->getType(), E->getValueKind(), E->getObjectKind(),
            E->getOperatorLoc(), E->getFPFeatures(this->Context.getLangOpts()));
      }
    } else if (E->getOpcode() == clang::BO_Add ||
               E->getOpcode() == clang::BO_Sub) {
      // both need to be divided.
      ExprResult LHSR = this->getDerived().TransformExpr(E->getLHS());
      if (!LHSR.isInvalid()) {
        ExprResult RHSR = this->getDerived().TransformExpr(E->getRHS());
        if (!RHSR.isInvalid()) {
          return BinaryOperator::Create(
              this->Context, LHSR.get(), RHSR.get(), E->getOpcode(),
              E->getType(), E->getValueKind(), E->getObjectKind(),
              E->getOperatorLoc(),
              E->getFPFeatures(this->Context.getLangOpts()));
        }
      }
    } else if (E->getOpcode() == clang::BO_Div ||
               E->getOpcode() == clang::BO_Shr) {
      // only LHS need to be divided.
      ExprResult LHSR = this->getDerived().TransformExpr(E->getLHS());
      if (!LHSR.isInvalid()) {
        return BinaryOperator::Create(
            this->Context, LHSR.get(), E->getRHS(), E->getOpcode(),
            E->getType(), E->getValueKind(), E->getObjectKind(),
            E->getOperatorLoc(), E->getFPFeatures(this->Context.getLangOpts()));
      }
    } else if (E->getOpcode() == clang::BO_And ||
               E->getOpcode() == clang::BO_Or ||
               E->getOpcode() == clang::BO_Xor) {
      bool powerOfTwo = !(Size == 0) && !(Size & (Size - 1));
      if (powerOfTwo) {
        // try divide both.
        ExprResult LHSR = this->getDerived().TransformExpr(E->getLHS());
        if (!LHSR.isInvalid()) {
          ExprResult RHSR = this->getDerived().TransformExpr(E->getRHS());
          if (!RHSR.isInvalid()) {
            return BinaryOperator::Create(
                this->Context, LHSR.get(), RHSR.get(), E->getOpcode(),
                E->getType(), E->getValueKind(), E->getObjectKind(),
                E->getOperatorLoc(),
                E->getFPFeatures(this->Context.getLangOpts()));
          }
        }
      }
    }
    return ExprError();
  }

  ExprResult TransformUnaryOperator(clang::UnaryOperator *E) {
    if (E->getOpcode() == clang::UO_Plus || E->getOpcode() == clang::UO_Minus) {
      ExprResult Op = this->getDerived().TransformExpr(E->getSubExpr());
      if (Op.isInvalid()) {
        return ExprError();
      }
      return UnaryOperator::Create(
          this->Context, Op.get(), E->getOpcode(), E->getType(),
          E->getValueKind(), E->getObjectKind(), E->getSourceRange().getBegin(),
          E->canOverflow(), E->getFPOptionsOverride());
    }
    return ExprError();
  }
  ExprResult TransformCStyleCastExpr(clang::CStyleCastExpr *E) {
    ExprResult Op = this->getDerived().TransformExpr(E->getSubExpr());
    if (Op.isInvalid()) {
      return ExprError();
    }
    return CStyleCastExpr::Create(
        this->Context, E->getType(), E->getValueKind(), E->getCastKind(),
        Op.get(), nullptr, E->getFPFeatures(), E->getTypeInfoAsWritten(),
        E->getLParenLoc(), E->getRParenLoc());
  }
  ExprResult TransformImplicitCastExpr(clang::ImplicitCastExpr *E) {
    ExprResult Op = this->getDerived().TransformExpr(E->getSubExpr());
    if (Op.isInvalid()) {
      return ExprError();
    }
    return ImplicitCastExpr::Create(this->Context, E->getType(),
                                    E->getCastKind(), Op.get(), nullptr,
                                    E->getValueKind(), E->getFPFeatures());
  }
  ExprResult TransformParenExpr(clang::ParenExpr *E) {
    ExprResult Op = this->getDerived().TransformExpr(E->getSubExpr());
    if (Op.isInvalid()) {
      return ExprError();
    }
    return new (this->Context)
        ParenExpr(E->getLParen(), E->getRParen(), Op.get());
  }
  ExprResult TransformConditionalOperator(clang::ConditionalOperator *E) {
    // both op should be divided.
    ExprResult LHS = this->getDerived().TransformExpr(E->getTrueExpr());
    if (LHS.isInvalid()) {
      return ExprError();
    }

    ExprResult RHS = this->getDerived().TransformExpr(E->getFalseExpr());
    if (RHS.isInvalid()) {
      return ExprError();
    }

    return new (this->Context) ConditionalOperator(
        E->getCond(), E->getQuestionLoc(), LHS.get(), E->getColonLoc(),
        RHS.get(), E->getType(), E->getValueKind(), E->getObjectKind());
  }

  // for all unsupported expr, return error.
  ExprResult TransformDeclRefExpr(clang::DeclRefExpr *E) { return ExprError(); }
  ExprResult TransformMemberExpr(clang::MemberExpr *E) { return ExprError(); }
  ExprResult TransformCallExpr(clang::CallExpr *E) { return ExprError(); }
  ExprResult TransformArraySubscriptExpr(clang::ArraySubscriptExpr *E) {
    return ExprError();
  }
  ExprResult TransformCXXBoolLiteralExpr(clang::CXXBoolLiteralExpr *E) {
    return ExprError();
  }
  ExprResult TransformFloatingLiteral(clang::FloatingLiteral *E) {
    return ExprError();
  }
};

} // namespace notdec::llvm2c

#endif
