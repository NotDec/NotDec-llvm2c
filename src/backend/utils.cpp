
#include "backend/utils.h"
#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <clang/AST/OperationKinds.h>
#include <iostream>
#include <type_traits>

#define DEBUG_TYPE "notdec-backend-utils"

namespace notdec::backend {

// ===============
// Precedence
// ===============

PrecedenceLevel getPrecedence(clang::Expr *E) {
  // Unary
  if (auto U = llvm::dyn_cast<clang::UnaryOperator>(E)) {
    return getOperatorPrecedence(U->getOpcode());
  } else if (llvm::isa<clang::CallExpr>(E)) {
    return SuffixUnary;
  } else if (llvm::isa<clang::ArraySubscriptExpr>(E)) {
    return SuffixUnary;
  } else if (llvm::isa<clang::MemberExpr>(E)) {
    return SuffixUnary;
  } else if (llvm::isa<clang::CompoundLiteralExpr>(E)) {
    return SuffixUnary;
  } else if (llvm::isa<clang::CastExpr>(E)) {
    return PrefixUnary;
  } else if (llvm::isa<clang::UnaryExprOrTypeTraitExpr>(E)) {
    return PrefixUnary;
  }
  // binary operator
  if (auto B = llvm::dyn_cast<clang::BinaryOperator>(E)) {
    return getOperatorPrecedence(B->getOpcode());
  }
  // ternary operator
  if (llvm::isa<clang::ConditionalOperator>(E) ||
      llvm::isa<clang::BinaryConditionalOperator>(E)) {
    return Conditional;
  }
  return Unknown;
}

bool isRightAssociative(PrecedenceLevel PL) {
  return PL == Conditional || PL == Assignment || PL == PrefixUnary;
}

bool isLeftAssociative(PrecedenceLevel PL) { return !isRightAssociative(PL); }

bool isUnary(PrecedenceLevel PL) {
  return PL == PrefixUnary || PL == SuffixUnary;
}

bool needParen(PrecedenceLevel PParent, PrecedenceLevel PChild, bool isLeft) {
  if (PParent == Unknown || PChild == Unknown) {
    // LLVM_DEBUG(llvm::dbgs() << "Unknown PrecedenceLevel found!\n");
    return false;
  }
  // if parent is higher precedence, then need paren
  if (PParent > PChild) {
    return true;
  }
  // if parent is same precedence, then need paren if associative equals
  // current location
  if (!isUnary(PParent) && PParent == PChild) {
    return isLeft ? isRightAssociative(PParent) : isLeftAssociative(PParent);
  }
  return false;
}

} // namespace notdec::backend