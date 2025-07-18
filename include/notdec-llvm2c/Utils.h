
#ifndef _NOTDEC_BACKEND_UTILS_H_
#define _NOTDEC_BACKEND_UTILS_H_

#include <clang/AST/Type.h>
#include <iostream>
#include <type_traits>

#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/Debug.h>

namespace notdec::llvm2c {
extern const char *KIND_STACK_POINTER;
bool isSPByMetadata(llvm::GlobalVariable *GV);

unsigned getLLVMTypeSize(llvm::Type *Ty, unsigned PointerSizeInBits = 0);
std::string join(std::string path, std::string elem);
void printModule(llvm::Module &M, const char *path);
std::unique_ptr<clang::ASTUnit> buildAST(llvm::StringRef FileName);
clang::QualType getBoolTy(clang::ASTContext& Ctx);

// ===============
// Pass
// ===============

// merge into single predecessor
// eliminate empty block with only one successor.
struct AdjustCFGPass : llvm::PassInfoMixin<AdjustCFGPass> {
  // Main entry point, takes IR unit to run the pass on (&F) and the
  // corresponding  pass manager (to be queried if need be)
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &);

  // Without isRequired returning true, this pass will be skipped for functions
  // decorated with the optnone LLVM attribute. Note that clang -O0 decorates
  // all functions with optnone.
  static bool isRequired() { return true; }
};

// match phi ret pattern and duplicate return block.
struct RetDupPass : llvm::PassInfoMixin<RetDupPass> {
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &);

  static bool isRequired() { return true; }
};

// Demote phi to stack like Reg2mem pass. The difference is not demoting values
// that escape the basic block, but only demote phis.
struct DemotePhiPass : llvm::PassInfoMixin<DemotePhiPass> {
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &);

  static bool isRequired() { return true; }
};

// ===============
// Precedence
// ===============

// Based on:
// https://github.com/llvm/llvm-project/blob/b4682816bc6e35fb4e207227f73b88b2603d4363/clang/include/clang/Basic/OperatorPrecedence.h#L26
enum PrecedenceLevel {
  Unknown = 0,          // Not binary operator.
  Comma = 1,            // ,
  Assignment = 2,       // =, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=
  Conditional = 3,      // ?  Ternary conditional
  LogicalOr = 4,        // ||
  LogicalAnd = 5,       // &&
  InclusiveOr = 6,      // |
  ExclusiveOr = 7,      // ^
  And = 8,              // &
  Equality = 9,         // ==, !=
  Relational = 10,      //  >=, <=, >, <
  Spaceship = 11,       // <=>
  Shift = 12,           // <<, >>
  Additive = 13,        // -, +
  Multiplicative = 14,  // *, /, %
  PointerToMember = 15, // .*, ->*
  PrefixUnary = 16,
  SuffixUnary = 17,
  ScopeResolution = 18, // ::
};

bool needParen(PrecedenceLevel PParent, PrecedenceLevel PChild, bool isLeft);
PrecedenceLevel getPrecedence(clang::Expr *E);

constexpr PrecedenceLevel getOperatorPrecedence(clang::BinaryOperatorKind OK) {
  switch (OK) {
  case clang::BO_Comma:
    return Comma;
  case clang::BO_Assign:
  case clang::BO_MulAssign:
  case clang::BO_DivAssign:
  case clang::BO_RemAssign:
  case clang::BO_AddAssign:
  case clang::BO_SubAssign:
  case clang::BO_ShlAssign:
  case clang::BO_ShrAssign:
  case clang::BO_AndAssign:
  case clang::BO_XorAssign:
  case clang::BO_OrAssign:
    return Assignment;
  case clang::BO_LAnd:
  case clang::BO_LOr:
    return LogicalOr;
  case clang::BO_Or:
    return InclusiveOr;
  case clang::BO_Xor:
    return ExclusiveOr;
  case clang::BO_And:
    return And;
  case clang::BO_EQ:
  case clang::BO_NE:
    return Equality;
  case clang::BO_GE:
  case clang::BO_LE:
  case clang::BO_GT:
  case clang::BO_LT:
    return Relational;
  case clang::BO_Shl:
  case clang::BO_Shr:
    return Shift;
  case clang::BO_Add:
  case clang::BO_Sub:
    return Additive;
  case clang::BO_Mul:
  case clang::BO_Div:
  case clang::BO_Rem:
    return Multiplicative;
  case clang::BO_PtrMemD:
  case clang::BO_PtrMemI:
    return PointerToMember;
  case clang::BO_Cmp:
    return Spaceship;
  }
}

constexpr PrecedenceLevel getOperatorPrecedence(clang::UnaryOperatorKind OK) {
  switch (OK) {
  case clang::UO_PostInc:
  case clang::UO_PostDec:
    return SuffixUnary;
  case clang::UO_PreInc:
  case clang::UO_PreDec:
  case clang::UO_AddrOf:
  case clang::UO_Deref:
  case clang::UO_Plus:
  case clang::UO_Minus:
  case clang::UO_Not:
  case clang::UO_LNot:
  case clang::UO_Coawait:
  case clang::UO_Real:
  case clang::UO_Imag:
  case clang::UO_Extension:
    return PrefixUnary;
  }
}

template <typename T> constexpr PrecedenceLevel getPrecedence(int OpCode = -1) {
  if (std::is_base_of<clang::CallExpr, T>()) {
    return SuffixUnary;
  } else if (std::is_base_of<clang::ArraySubscriptExpr, T>()) {
    return SuffixUnary;
  } else if (std::is_base_of<clang::MemberExpr, T>()) {
    return SuffixUnary;
  } else if (std::is_base_of<clang::CompoundLiteralExpr, T>()) {
    return SuffixUnary;
  } else if (std::is_base_of<clang::CastExpr, T>()) {
    return PrefixUnary;
  } else if (std::is_base_of<clang::UnaryExprOrTypeTraitExpr, T>()) {
    return PrefixUnary;
  } else if (std::is_base_of<clang::UnaryOperator, T>()) {
    return getOperatorPrecedence((clang::UnaryOperatorKind)OpCode);
  } else if (std::is_base_of<clang::BinaryOperator, T>()) {
    return getOperatorPrecedence((clang::BinaryOperatorKind)OpCode);
  } else if (std::is_base_of<clang::ConditionalOperator, T>() ||
             std::is_base_of<clang::BinaryConditionalOperator, T>()) {
    return Conditional;
  }
  return Unknown;
}

template <typename T>
bool needParen(int OpCode, clang::Expr *Child, bool isLeft) {
  static_assert(std::is_same<T, clang::BinaryOperator>() ||
                    std::is_same<T, clang::UnaryOperator>(),
                "Invalid type");
  return needParen(getPrecedence<T>(OpCode), getPrecedence(Child), isLeft);
}

template <typename T> bool needParen(clang::Expr *Child, bool isLeft) {
  static_assert(!(std::is_same<T, clang::BinaryOperator>() ||
                  std::is_same<T, clang::UnaryOperator>()),
                "Invalid type");
  return needParen(getPrecedence<T>(), getPrecedence(Child), isLeft);
}

/// Main interface function to check if a parenthesis is needed.
/// Pass the ast type of parent node as the template argument.
/// This version is for binary or unary operator.
/// cast the operator opcode to int as the first argument.
template <typename T>
clang::Expr *addParenthesis(clang::ASTContext &Ctx, int OpCode,
                            clang::Expr *Child, bool isLeft) {
  if (needParen<T>(OpCode, Child, isLeft)) {
    return new (Ctx)
        clang::ParenExpr(Child->getBeginLoc(), Child->getEndLoc(), Child);
  }
  return Child;
}

/// Main interface function to check if a parenthesis is needed.
/// Pass the ast type of parent node as the template argument.
/// This version is for other types except binary/unary operator.
template <typename T>
clang::Expr *addParenthesis(clang::ASTContext &Ctx, clang::Expr *Child,
                            bool isLeft) {
  if (needParen<T>(Child, isLeft)) {
    return new (Ctx)
        clang::ParenExpr(Child->getBeginLoc(), Child->getEndLoc(), Child);
  }
  return Child;
}

inline clang::BinaryOperator *
createBinaryOperator(clang::ASTContext &Ctx, clang::Expr *LHS, clang::Expr *RHS,
                     clang::BinaryOperatorKind BOK, clang::QualType QT,
                     clang::ExprValueKind VK) {
  return clang::BinaryOperator::Create(
      Ctx, addParenthesis<clang::BinaryOperator>(Ctx, (int)BOK, LHS, true),
      addParenthesis<clang::BinaryOperator>(Ctx, (int)BOK, RHS, false), BOK, QT,
      VK, clang::OK_Ordinary, clang::SourceLocation(),
      clang::FPOptionsOverride());
}

inline clang::ConditionalOperator *
createConditionalOperator(clang::ASTContext &Ctx, clang::Expr *cond,
                          clang::Expr *LHS, clang::Expr *RHS,
                          clang::QualType QT, clang::ExprValueKind VK) {
  return new (Ctx) clang::ConditionalOperator(
      addParenthesis<clang::ConditionalOperator>(Ctx, cond, true),
      clang::SourceLocation(), LHS, clang::SourceLocation(),
      addParenthesis<clang::ConditionalOperator>(Ctx, RHS, false), QT, VK,
      clang::OK_Ordinary);
}

inline clang::UnaryOperator *createUnaryOperator(clang::ASTContext &Ctx,
                                                 clang::Expr *E,
                                                 clang::UnaryOperatorKind OK,
                                                 clang::QualType QT,
                                                 clang::ExprValueKind VK) {
  return clang::UnaryOperator::Create(
      Ctx, addParenthesis<clang::UnaryOperator>(Ctx, (int)OK, E, true), OK, QT,
      VK, clang::OK_Ordinary, clang::SourceLocation(), false,
      clang::FPOptionsOverride());
}

// create cstylecast
inline clang::CStyleCastExpr *createCStyleCastExpr(clang::ASTContext &Ctx,
                                                   clang::QualType QT,
                                                   clang::ExprValueKind VK,
                                                   clang::CastKind CK,
                                                   clang::Expr *E) {
  assert(!QT.isNull());
  if (auto CE = llvm::dyn_cast<clang::CStyleCastExpr>(E)) {
    E = CE->getSubExpr();
  }
  return clang::CStyleCastExpr::Create(
      Ctx, QT, VK, CK, addParenthesis<clang::CStyleCastExpr>(Ctx, E, true),
      nullptr, clang::FPOptionsOverride(), Ctx.getTrivialTypeSourceInfo(QT),
      clang::SourceLocation(), clang::SourceLocation());
}

clang::Expr *getNoCast(clang::Expr *E);
clang::Expr *makeNonArrow(clang::MemberExpr *M);
clang::Expr *makeArrow(clang::MemberExpr *M);
clang::Expr *createMemberExpr(clang::ASTContext &Ctx, clang::Expr *Base,
                              clang::FieldDecl *Field);
clang::Expr *getDerefInner(clang::Expr *E);
clang::Expr *getAddrofInner(clang::Expr *E);

clang::Expr *addrOf(clang::ASTContext &Ctx, clang::Expr *E, bool NoElimMember = false);
clang::Expr *deref(clang::ASTContext &Ctx, clang::Expr *E);

// dump LLVM object to string
template <typename T> std::string llvmObjToString(const T *t) {
  std::string str;
  llvm::raw_string_ostream ss(str);
  if (t)
    t->print(ss);
  else
    ss << "nullptr";
  return ss.str();
}

std::string llvmObjToString(const llvm::Module *t);

// dump Clang object to string
template <typename T> std::string clangObjToString(const T *stmt) {
  clang::LangOptions lo;
  std::string out_str;
  llvm::raw_string_ostream outstream(out_str);
  stmt->printPretty(outstream, NULL, clang::PrintingPolicy(lo));
  return out_str;
}

inline std::string clangObjToString(clang::QualType Ty) {
  clang::LangOptions lo;
  std::string out_str;
  llvm::raw_string_ostream outstream(out_str);
  Ty.print(outstream, clang::PrintingPolicy(lo));
  return out_str;
}

} // namespace notdec::llvm2c
#endif
