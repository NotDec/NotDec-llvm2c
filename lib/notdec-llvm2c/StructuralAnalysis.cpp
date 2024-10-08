#include <cassert>
#include <llvm/IR/InstrTypes.h>
#include <utility>

#include "llvm/ADT/PostOrderIterator.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalObject.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <clang/AST/ASTContext.h>
#include <clang/AST/Comment.h>
#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/OperationKinds.h>
#include <clang/AST/PrettyPrinter.h>
#include <clang/AST/RawCommentList.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Type.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/Analysis/CFG.h>
#include <clang/Basic/LLVM.h>
#include <clang/Basic/LangOptions.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/Specifiers.h>
#include <clang/Basic/TokenKinds.h>
#include <clang/Tooling/Transformer/RewriteRule.h>
#include <variant>
#include <vector>

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/CompoundConditionBuilder.h"
#include "notdec-llvm2c/Goto.h"
#include "notdec-llvm2c/Interface.h"
#include "notdec-llvm2c/Phoenix.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include "notdec-llvm2c/Utils.h"

#define DEBUG_TYPE "structural-analysis"

namespace notdec::llvm2c {

clang::Stmt *getStmt(CFGElement e) {
  if (auto stmt = e.getAs<CFGStmt>()) {
    return const_cast<clang::Stmt *>(stmt->getStmt());
  }
  return nullptr;
}

void CFGBuilder::visitInstruction(llvm::Instruction &I) {
  llvm::errs() << "Warning: CFGBuilder: Cannot handle: " << I << "\n";
  LLVM_DEBUG(std::abort());
}

/// Check if the block is the entry block of the function, or a must via block
/// that follows the entry.
bool isMustViaBlock(llvm::BasicBlock &bb) {
  llvm::Function *F = bb.getParent();
  llvm::BasicBlock *entry = &F->getEntryBlock();
  // from entry, continue if unique successor and predecessor.
  do {
    if (entry == &bb) {
      return true;
    }
    entry = entry->getUniqueSuccessor();
  } while (entry != nullptr && entry->getUniquePredecessor() != nullptr);
  return false;
}

clang::Expr *addrOf(clang::ASTContext &Ctx, clang::Expr *E) {
  // eliminate addrOf + deref
  if (llvm::isa<clang::UnaryOperator>(E) &&
      llvm::cast<clang::UnaryOperator>(E)->getOpcode() == clang::UO_Deref) {
    return llvm::cast<clang::UnaryOperator>(E)->getSubExpr();
  }
  // eliminate addrOf + array subscript 0.
  if (auto ArraySub = llvm::dyn_cast<clang::ArraySubscriptExpr>(E)) {
    if (auto Index =
            llvm::dyn_cast<clang::IntegerLiteral>(ArraySub->getIdx())) {
      if (Index->getValue() == 0) {
        return llvm::cast<clang::ArraySubscriptExpr>(E)->getBase();
      }
    }
  }
  return createUnaryOperator(Ctx, E, clang::UO_AddrOf,
                             Ctx.getPointerType(E->getType()),
                             clang::VK_LValue);
}

clang::Expr *deref(clang::ASTContext &Ctx, clang::Expr *E) {
  // eliminate deref + addrOf
  if (llvm::isa<clang::UnaryOperator>(E) &&
      llvm::cast<clang::UnaryOperator>(E)->getOpcode() == clang::UO_AddrOf) {
    return llvm::cast<clang::UnaryOperator>(E)->getSubExpr();
  }
  clang::QualType Ty = E->getType();
  if (Ty->isPointerType()) {
    Ty = Ty->getPointeeType();
  } else if (Ty->isArrayType()) {
    Ty = Ty->castAsArrayTypeUnsafe()->getElementType();
  } else {
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "CFGBuilder.deref: unexpected type: ";
    Ty.dump();
    llvm::errs() << "\n";
    std::abort();
  }
  return createUnaryOperator(Ctx, E, clang::UO_Deref, Ty, clang::VK_LValue);
}

void CFGBuilder::visitAllocaInst(llvm::AllocaInst &I) {
  // check if the instruction is in the entry block.
  if (isMustViaBlock(*I.getParent())) {
    // create a local variable
    auto II = FCtx.getIdentifierInfo(FCtx.getValueNamer().getTempName(I));
    clang::VarDecl *VD = clang::VarDecl::Create(
        Ctx, FCtx.getFunctionDecl(), clang::SourceLocation(),
        clang::SourceLocation(), II,
        FCtx.getTypeBuilder().getType(I)->getPointeeType(), nullptr,
        clang::SC_None);

    // Create a decl statement.
    clang::DeclStmt *DS = new (Ctx)
        clang::DeclStmt(clang::DeclGroupRef(VD), clang::SourceLocation(),
                        clang::SourceLocation());
    Blk->appendStmt(DS);
    FCtx.getFunctionDecl()->addDecl(VD);
    // When alloca is referenced, it refers to the address of the DeclRefExpr
    auto addr = addrOf(Ctx, makeDeclRefExpr(VD));
    addExprOrStmt(I, *addr);
  } else {
    // TODO create a alloca call
    llvm::errs() << "Warning: Dynamic stack allocation in "
                 << I.getParent()->getParent()->getName() << ": " << I << "\n";
    std::abort();
  }
}

clang::Expr *handleGEP(clang::ASTContext &Ctx, ExprBuilder &EB,
                       llvm::GEPOperator &I) {
  clang::Expr *Val = EB.visitValue(I.getPointerOperand());
  const clang::Type *Ty = Val->getType().getTypePtr();
  for (unsigned i = 0; i < I.getNumIndices(); i++) {
    llvm::Value *LIndex = *(I.idx_begin() + i);
    // TODO Assert llvm type to ensure that each step is correct.
    // llvm::Type *lty = I.getTypeAtIndex(I.getSourceElementType(), i);
    if (auto PointerTy = Ty->getAs<clang::PointerType>()) {
      // 1. pointer arithmetic + deref
      Ty = PointerTy->getPointeeType().getTypePtr();
      clang::Expr *Index;
      if (llvm::isa<llvm::ConstantInt>(LIndex) &&
          llvm::cast<llvm::ConstantInt>(LIndex)->getZExtValue() == 0) {
        // skip the add
      } else {
        Index = EB.visitValue(LIndex);
        // Create pointer arithmetic
        Val = createBinaryOperator(Ctx, Val, Index, clang::BO_Add,
                                   Val->getType(), clang::VK_LValue);
      }
      Val = deref(Ctx, Val);
    } else if (auto ArrayTy = Ty->getAsArrayTypeUnsafe()) {
      // 2. array indexing
      clang::Expr *Index = EB.visitValue(LIndex);
      Ty = ArrayTy->getElementType().getTypePtr();
      Val = new (Ctx) clang::ArraySubscriptExpr(
          Val, Index, ArrayTy->getElementType(), clang::VK_LValue,
          clang::OK_Ordinary, clang::SourceLocation());
    } else if (auto RecordTy = Ty->getAs<clang::RecordType>()) {
      // 3. field reference
      auto Decl = RecordTy->getDecl();
      auto Field = Decl->field_begin();
      auto IndexNum = llvm::cast<llvm::ConstantInt>(LIndex)->getZExtValue();
      std::advance(Field, IndexNum);
      // check if the val is deref, if so, then remove it and use arrow expr.
      bool useArrow = false;
      if (llvm::isa<clang::UnaryOperator>(Val) &&
          llvm::cast<clang::UnaryOperator>(Val)->getOpcode() ==
              clang::UO_Deref) {
        Val = llvm::cast<clang::UnaryOperator>(Val)->getSubExpr();
        useArrow = true;
      }
      Ty = Field->getType().getTypePtr();
      Val = clang::MemberExpr::Create(
          Ctx, Val, useArrow, clang::SourceLocation(),
          clang::NestedNameSpecifierLoc(), clang::SourceLocation(), *Field,
          clang::DeclAccessPair::make(*Field, Field->getAccess()),
          clang::DeclarationNameInfo(), nullptr, Field->getType(),
          clang::VK_LValue, clang::OK_Ordinary, clang::NOUR_None);
    } else {
      llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                   << "UnImplemented: CFGBuilder.visitGetElementPtrInst cannot "
                      "handle type: ";
      Ty->dump();
      llvm::errs() << "\n";
      std::abort();
    }
  }
  // implicit addrOf at the end of GEP
  return addrOf(Ctx, Val);
}

void CFGBuilder::visitGetElementPtrInst(llvm::GetElementPtrInst &I) {
  addExprOrStmt(I, *handleGEP(Ctx, EB, llvm::cast<llvm::GEPOperator>(I)));
}

void CFGBuilder::visitStoreInst(llvm::StoreInst &I) {
  // store = assign + deref left.
  clang::Expr *lhs = EB.visitValue(I.getPointerOperand());
  clang::Expr *rhs = EB.visitValue(I.getValueOperand());
  lhs = deref(Ctx, lhs);
  clang::Expr *assign = createBinaryOperator(Ctx, lhs, rhs, clang::BO_Assign,
                                             lhs->getType(), clang::VK_LValue);
  addExprOrStmt(I, *assign);
}

void CFGBuilder::visitLoadInst(llvm::LoadInst &I) {
  clang::Expr *E = EB.visitValue(I.getPointerOperand());
  E = deref(Ctx, E);
  addExprOrStmt(I, *E);
}

clang::Expr *handleCmp(clang::ASTContext &Ctx, TypeBuilder &TB, ExprBuilder &EB,
                       llvm::CmpInst::Predicate OpCode, llvm::Value *Result,
                       llvm::Value *Op0, llvm::Value *Op1) {
  clang::Expr *cmp;
  clang::Expr *lhs = EB.visitValue(Op0);
  clang::Expr *rhs = nullptr;
  // handle FCMP_FALSE and FCMP_TRUE case
  // TODO: return typedef TRUE and FALSE
  if (OpCode == llvm::CmpInst::FCMP_FALSE) {
    // create int 0
    return clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, 0, false),
                                         Ctx.IntTy, clang::SourceLocation());
  } else if (OpCode == llvm::CmpInst::FCMP_TRUE) {
    return clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, 1, false),
                                         Ctx.IntTy, clang::SourceLocation());
  } else if (OpCode == llvm::CmpInst::FCMP_ORD ||
             OpCode == llvm::CmpInst::FCMP_UNO) {
    // ord: yields true if both operands are not a QNAN.
    // convert to op1 == op1 && op2 == op2.
    // uno: yields true if either operand is a QNAN.
    // convert to op1 != op1 || op2 != op2.
    // https://stackoverflow.com/a/570694/13798540
    // op1 != op1 can check if one number is NAN.
    // Specialize for case where op2 is Constant.
    assert(!llvm::isa<llvm::Constant>(Op0)); // because of inst combine.
    lhs = createBinaryOperator(
        Ctx, lhs, lhs, llvm::CmpInst::FCMP_ORD ? clang::BO_EQ : clang::BO_NE,
        TB.getType(*Result), clang::VK_PRValue);
    if (llvm::isa<llvm::Constant>(Op1)) {
      return lhs;
    } else {
      rhs = EB.visitValue(Op1);
      rhs = createBinaryOperator(Ctx, rhs, rhs, clang::BO_NE,
                                 TB.getType(*Result), clang::VK_PRValue);
      cmp = createBinaryOperator(
          Ctx, lhs, rhs,
          OpCode == llvm::CmpInst::FCMP_ORD ? clang::BO_LAnd : clang::BO_LOr,
          TB.getType(*Result), clang::VK_PRValue);
      return cmp;
    }
    // unreachable.
    llvm_unreachable(
        "CFGBuilder.visitCmpInst FCMP_ORD / FCMP_UNO: unreachable");
  }

  clang::Optional<clang::BinaryOperatorKind> op = convertOp(OpCode);
  assert(op.hasValue() && "CFGBuilder.visitCmpInst: unexpected predicate");
  Conversion cv = getSignedness(OpCode);
  if (cv == Signed) {
    lhs = castSigned(Ctx, TB, lhs);
  } else if (cv == Unsigned) {
    lhs = castUnsigned(Ctx, TB, lhs);
  }
  rhs = EB.visitValue(Op1);
  if (cv == Signed) {
    lhs = castSigned(Ctx, TB, lhs);
  } else if (cv == Unsigned) {
    lhs = castUnsigned(Ctx, TB, lhs);
  }
  cmp = createBinaryOperator(Ctx, lhs, rhs, *op, TB.getType(*Result),
                             clang::VK_PRValue);
  return cmp;
}

void CFGBuilder::visitCmpInst(llvm::CmpInst &I) {
  auto cmp = handleCmp(Ctx, FCtx.getTypeBuilder(), EB, I.getPredicate(), &I,
                       I.getOperand(0), I.getOperand(1));
  addExprOrStmt(I, *cmp);
  return;
}

/// Get the signedness of the binary operator.
Conversion getSignedness(llvm::Instruction::BinaryOps op) {
  switch (op) {
  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
  case llvm::Instruction::Mul:
  case llvm::Instruction::And:
  case llvm::Instruction::Or:
  case llvm::Instruction::Xor:
  case llvm::Instruction::Shl:
    return None;
  case llvm::Instruction::LShr:
    return Logical;
  case llvm::Instruction::AShr:
    return Arithmetic;
  case llvm::Instruction::UDiv:
  case llvm::Instruction::URem:
    return Unsigned;
  case llvm::Instruction::SDiv:
  case llvm::Instruction::SRem:
    return Signed;
  case llvm::Instruction::FAdd:
  case llvm::Instruction::FSub:
  case llvm::Instruction::FMul:
  case llvm::Instruction::FDiv:
  case llvm::Instruction::FRem:
    return None;
  default:
    return None;
  }
}

Conversion getSignedness(llvm::CmpInst::Predicate op) {
  switch (op) {
  case llvm::CmpInst::ICMP_UGT:
  case llvm::CmpInst::ICMP_UGE:
  case llvm::CmpInst::ICMP_ULT:
  case llvm::CmpInst::ICMP_ULE:
    return Unsigned;
  case llvm::CmpInst::ICMP_SGT:
  case llvm::CmpInst::ICMP_SGE:
  case llvm::CmpInst::ICMP_SLT:
  case llvm::CmpInst::ICMP_SLE:
    return Signed;
  case llvm::CmpInst::ICMP_EQ:
  case llvm::CmpInst::ICMP_NE:
    return None;
  default:
    return None;
  }
}

/// Convert the LLVM compare operator to Clang binary operator op.
clang::Optional<clang::BinaryOperatorKind>
convertOp(llvm::CmpInst::Predicate op) {
  switch (op) {
  case llvm::CmpInst::Predicate::ICMP_EQ:
    return clang::BO_EQ;
  case llvm::CmpInst::Predicate::ICMP_NE:
    return clang::BO_NE;
  case llvm::CmpInst::Predicate::ICMP_UGT:
    return clang::BO_GT;
  case llvm::CmpInst::Predicate::ICMP_UGE:
    return clang::BO_GE;
  case llvm::CmpInst::Predicate::ICMP_ULT:
    return clang::BO_LT;
  case llvm::CmpInst::Predicate::ICMP_ULE:
    return clang::BO_LE;
  case llvm::CmpInst::Predicate::ICMP_SGT:
    return clang::BO_GT;
  case llvm::CmpInst::Predicate::ICMP_SGE:
    return clang::BO_GE;
  case llvm::CmpInst::Predicate::ICMP_SLT:
    return clang::BO_LT;
  case llvm::CmpInst::Predicate::ICMP_SLE:
    return clang::BO_LE;
  case llvm::CmpInst::FCMP_FALSE:
  case llvm::CmpInst::FCMP_TRUE:
    llvm_unreachable("CFGBuilder.convertOp: FCMP_FALSE or FCMP_TRUE should "
                     "be considered ahead of time.");
  // TODO handle ordered and unordered comparison
  // probably by converting unordered comparison to ordered comparison plus
  // negation, or a function call.
  case llvm::CmpInst::FCMP_OEQ:
  case llvm::CmpInst::FCMP_UEQ:
    return clang::BO_EQ;
  case llvm::CmpInst::FCMP_OGT:
  case llvm::CmpInst::FCMP_UGT:
    return clang::BO_GT;
  case llvm::CmpInst::FCMP_OGE:
  case llvm::CmpInst::FCMP_UGE:
    return clang::BO_GE;
  case llvm::CmpInst::FCMP_OLT:
  case llvm::CmpInst::FCMP_ULT:
    return clang::BO_LT;
  case llvm::CmpInst::FCMP_OLE:
  case llvm::CmpInst::FCMP_ULE:
    return clang::BO_LE;
  case llvm::CmpInst::FCMP_ONE:
  case llvm::CmpInst::FCMP_UNE:
    return clang::BO_NE;
  case llvm::CmpInst::FCMP_ORD:
  case llvm::CmpInst::FCMP_UNO:
    llvm_unreachable(
        "CFGBuilder.convertOp: FCMP_ORD or FCMP_UNO should already be handled");
  case llvm::CmpInst::BAD_FCMP_PREDICATE:
  case llvm::CmpInst::BAD_ICMP_PREDICATE:
    return clang::None;
  }
}

/// Convert the LLVM binary operator to a Clang binary operator op.
clang::Optional<clang::BinaryOperatorKind>
convertOp(llvm::Instruction::BinaryOps op) {
  switch (op) {
  case llvm::Instruction::Add:
  case llvm::Instruction::FAdd:
    return clang::BO_Add;
  case llvm::Instruction::Sub:
  case llvm::Instruction::FSub:
    return clang::BO_Sub;
  case llvm::Instruction::Mul:
  case llvm::Instruction::FMul:
    return clang::BO_Mul;
  case llvm::Instruction::UDiv:
  case llvm::Instruction::SDiv:
  case llvm::Instruction::FDiv:
    return clang::BO_Div;
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
  case llvm::Instruction::FRem:
    return clang::BO_Rem;

  case llvm::Instruction::Shl:
    return clang::BO_Shl;
  case llvm::Instruction::LShr:
    return clang::BO_Shr;
  case llvm::Instruction::AShr:
    return clang::BO_Shl;

  case llvm::Instruction::And:
    return clang::BO_And;
  case llvm::Instruction::Or:
    return clang::BO_Or;
  case llvm::Instruction::Xor:
    return clang::BO_Xor;
  default:
    return clang::None;
  }
}

void CFGBuilder::visitCallInst(llvm::CallInst &I) {
  // See also:
  // https://github.com/llvm/llvm-project/blob/d8e5a0c42bd8796cce9caa53aacab88c7cb2a3eb/clang/lib/Analysis/BodyFarm.cpp#L245
  llvm::SmallVector<clang::Expr *, 16> Args(I.arg_size());
  for (unsigned i = 0; i < I.arg_size(); i++) {
    Args[i] = EB.visitValue(I.getArgOperand(i));
    assert(Args[i] != nullptr && "CFGBuilder.visitCallInst: Args[i] is null?");
  }
  llvm::Function *Callee = I.getCalledFunction();
  clang::QualType ret;
  clang::Expr *FRef;
  if (Callee != nullptr) {
    auto FD = FCtx.getSAContext().getFunctionDecl(*Callee);
    clang::QualType Ty = FD->getType();
    FRef = makeDeclRefExpr(FD);
    if (Ty->isLValueReferenceType() && FRef->getType()->isFunctionType()) {
      Ty = Ctx.getPointerType(Ty.getNonReferenceType());
      FRef = makeImplicitCast(FRef, Ty, clang::CK_FunctionToPointerDecay);
    }
    ret = FD->getReturnType();
  } else {
    // Function pointer call
    // TODO: double check
    auto CalleeExpr = EB.visitValue(I.getCalledOperand());
    assert(CalleeExpr != nullptr &&
           "CFGBuilder.visitCallInst: CalleeExpr is null?");
    assert(CalleeExpr->getType()->isPointerType() &&
           CalleeExpr->getType()->getPointeeType()->isFunctionType() &&
           "CallInst operand is not a function pointer?");
    FRef = CalleeExpr;
    ret =
        llvm::cast<clang::FunctionType>(CalleeExpr->getType()->getPointeeType())
            ->getReturnType();
  }
  // TODO? CallExpr type is function return type or not?
  auto Call = clang::CallExpr::Create(Ctx, FRef, Args, ret, clang::VK_PRValue,
                                      clang::SourceLocation(),
                                      clang::FPOptionsOverride());

  addExprOrStmt(I, *Call);
}

ValueNamer &SAFuncContext::getValueNamer() {
  return getSAContext().getValueNamer();
}

void SAFuncContext::addExprOrStmt(llvm::Value &v, clang::Stmt &Stmt,
                                  CFGBlock &block) {
  // non-instruction or expr-like instruction.
  if (!llvm::isa<llvm::Instruction>(&v) || llvm::isa<llvm::AllocaInst>(&v) ||
      llvm::isa<llvm::Argument>(&v)) {
    ExprMap[&v] = &llvm::cast<clang::Expr>(Stmt);
    return;
  }
  if (v.getNumUses() == 0) {
    // Treat as stmt
    block.appendStmt(&Stmt);
    return;
  }
  assert(llvm::isa<clang::Expr>(&Stmt) &&
         "SAFuncContext.addExprOrStmt: Instruction has uses but is not Expr?");
  auto &Expr = llvm::cast<clang::Expr>(Stmt);
  auto &inst = *llvm::cast<llvm::Instruction>(&v);
  if (onlyUsedInBlock(inst)) {
    // Has one use and is in the same block
    ExprMap[&v] = &Expr; // wait to be folded
  } else {
    // Create a local variable for it, in order to be faithful to the IR.
    auto Name = getValueNamer().getTempName(inst);

    llvm::SmallString<128> Buf;
    clang::IdentifierInfo *II2 = getIdentifierInfo(Name);
    clang::VarDecl *decl = clang::VarDecl::Create(
        getASTContext(), FD, clang::SourceLocation(), clang::SourceLocation(),
        II2, TB.getType(inst), nullptr, clang::SC_None);
    decl->setInit(&Expr);
    clang::DeclStmt *DS = new (getASTContext())
        clang::DeclStmt(clang::DeclGroupRef(decl), clang::SourceLocation(),
                        clang::SourceLocation());

    // Use Assign stmt
    // clang::DeclRefExpr *ref = clang::DeclRefExpr::Create(
    //     getASTContext(), clang::NestedNameSpecifierLoc(),
    //     clang::SourceLocation(), decl, false,
    //     clang::DeclarationNameInfo(II2, clang::SourceLocation()),
    //     expr->getType(), clang::VK_LValue);
    // // assign stmt
    // clang::Stmt *DS = createBinaryOperator(
    //     getASTContext(), ref, expr, clang::BO_Assign,
    //     expr->getType(), clang::VK_PRValue);
    block.appendStmt(DS);
    ExprMap[&v] = makeDeclRefExpr(decl);
  }
}

clang::DeclRefExpr *makeDeclRefExpr(clang::ValueDecl *D,
                                    bool RefersToEnclosingVariableOrCapture) {
  clang::ASTContext &Ctx = D->getASTContext();
  clang::QualType Type = D->getType().getNonReferenceType();

  clang::DeclRefExpr *DR = clang::DeclRefExpr::Create(
      Ctx, clang::NestedNameSpecifierLoc(), clang::SourceLocation(), D,
      RefersToEnclosingVariableOrCapture, clang::SourceLocation(), Type,
      clang::VK_LValue);
  return DR;
}

clang::Expr *castSigned(clang::ASTContext &Ctx, TypeBuilder &TB,
                        clang::Expr *E) {
  if (E->getType()->isUnsignedIntegerType()) {
    auto ty = TB.makeSigned(Ctx, E->getType());
    return clang::CStyleCastExpr::Create(
        Ctx, ty, clang::VK_PRValue, clang::CK_IntegralCast, E, nullptr,
        clang::FPOptionsOverride(), Ctx.CreateTypeSourceInfo(ty),
        clang::SourceLocation(), clang::SourceLocation());
  }
  return E;
}

clang::Expr *castUnsigned(clang::ASTContext &Ctx, TypeBuilder &TB,
                          clang::Expr *E) {
  if (E->getType()->isSignedIntegerType()) {
    auto ty = TB.makeUnsigned(Ctx, E->getType());
    return clang::CStyleCastExpr::Create(
        Ctx, ty, clang::VK_PRValue, clang::CK_IntegralCast, E, nullptr,
        clang::FPOptionsOverride(), Ctx.CreateTypeSourceInfo(ty),
        clang::SourceLocation(), clang::SourceLocation());
  }
  return E;
}

void CFGBuilder::visitUnaryOperator(llvm::UnaryOperator &I) {
  clang::UnaryOperatorKind op;
  switch (I.getOpcode()) {
  case llvm::Instruction::FNeg:
    op = clang::UO_Minus;
    break;
  default:
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "CFGBuilder.visitUnaryOperator: unexpected opcode: "
                 << I.getOpcode() << "\n";
    std::abort();
  }
  clang::Expr *expr = createUnaryOperator(Ctx, EB.visitValue(I.getOperand(0)),
                                          op, getType(I), clang::VK_PRValue);
  addExprOrStmt(I, *expr);
}

clang::CastKind convertCastKind(llvm::Instruction::CastOps OpCode,
                                bool isBool) {
  clang::CastKind ck;
  switch (OpCode) {
  case llvm::Instruction::Trunc:
    ck = clang::CK_IntegralCast;
    break;
  case llvm::Instruction::ZExt:
    ck = clang::CK_IntegralCast;
    break;
  case llvm::Instruction::SExt:
    ck = clang::CK_IntegralCast;
    break;
  case llvm::Instruction::FPTrunc:
    ck = clang::CK_FloatingCast;
    break;
  case llvm::Instruction::FPExt:
    ck = clang::CK_FloatingCast;
    break;
  case llvm::Instruction::UIToFP:
    ck = clang::CK_IntegralToFloating;
    break;
  case llvm::Instruction::SIToFP:
    ck = clang::CK_IntegralToFloating;
    break;
  case llvm::Instruction::FPToUI:
    if (isBool) {
      ck = clang::CK_FloatingToBoolean;
    } else {
      ck = clang::CK_FloatingToIntegral;
    }
    break;
  case llvm::Instruction::FPToSI:
    ck = clang::CK_FloatingToIntegral;
    break;
  case llvm::Instruction::PtrToInt:
    if (isBool) {
      ck = clang::CK_PointerToBoolean;
    } else {
      ck = clang::CK_PointerToIntegral;
    }
    break;
  case llvm::Instruction::IntToPtr:
    ck = clang::CK_IntegralToPointer;
    break;
  case llvm::Instruction::BitCast:
    ck = clang::CK_BitCast;
    break;
  case llvm::Instruction::AddrSpaceCast:
    ck = clang::CK_AddressSpaceConversion;
    break;
  default:
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "CFGBuilder.visitCastInst: unexpected opcode: " << OpCode
                 << "\n";
    std::abort();
  }
  return ck;
}

clang::Expr *handleLLVMCast(clang::ASTContext &Ctx, llvm::LLVMContext &LCtx,
                            ExprBuilder &EB, TypeBuilder &TB,
                            llvm::Instruction::CastOps OpCode,
                            llvm::Type *srcTy, llvm::Type *destTy,
                            llvm::Value *Operand) {
  auto Val = EB.visitValue(Operand);
  // If zext i1 to integer, then ignore.
  if (srcTy->isIntegerTy(1)) {
    srcTy = llvm::Type::getInt32Ty(LCtx);
  }
  if (destTy->isIntegerTy(1)) {
    destTy = llvm::Type::getInt32Ty(LCtx);
  }
  if (srcTy == destTy) {
    // no need to cast
    return Val;
  }
  // TODO handle signness and ZExt SExt
  clang::CastKind ck = convertCastKind(OpCode, destTy->isIntegerTy(1));

  clang::Expr *expr = createCStyleCastExpr(Ctx, TB.visitType(*destTy),
                                           clang::VK_PRValue, ck, Val);
  return expr;
}

/// Handle the cast instruction. Shared by instruction visitor and expr
/// builder (for ConstantExpr operators).
clang::Expr *handleCast(clang::ASTContext &Ctx, llvm::LLVMContext &LCtx,
                        ExprBuilder &EB, TypeBuilder &TB,
                        llvm::Instruction::CastOps OpCode,
                        clang::QualType destTy, llvm::Value *Operand) {
  auto Val = EB.visitValue(Operand);
  auto srcTy = Val->getType();
  if (srcTy == destTy) {
    // no need to cast
    return Val;
  }
  bool isBool = destTy->isBooleanType();

  clang::CastKind ck = convertCastKind(OpCode, isBool);
  // Fixup CastKind. Actually we should not use OpCode to infer ck here.
  if (srcTy->isPointerType() && destTy->isPointerType()) {
    ck = clang::CK_BitCast;
  }

  clang::Expr *expr =
      createCStyleCastExpr(Ctx, destTy, clang::VK_PRValue, ck, Val);
  return expr;
}

void CFGBuilder::visitCastInst(llvm::CastInst &I) {
  // Ignore reg2mem alloca point inserted by reg2mem.
  if (I.getNumUses() == 0 && I.getName().startswith("reg2mem alloca point")) {
    return;
  }
  auto destTy = getType(I);
  auto expr = handleCast(Ctx, I.getContext(), EB, FCtx.getTypeBuilder(),
                         I.getOpcode(), destTy, I.getOperand(0));
  if (expr == nullptr) {
    // no need to cast
    FCtx.addMapping(I, *EB.visitValue(I.getOperand(0)));
    return;
  }
  addExprOrStmt(I, *expr);
}

clang::Expr *handleBinary(clang::ASTContext &Ctx, ExprBuilder &EB,
                          TypeBuilder &TB, llvm::Instruction::BinaryOps OpCode,
                          llvm::Value &Result, llvm::Value *L, llvm::Value *R) {
  clang::Optional<clang::BinaryOperatorKind> op = convertOp(OpCode);
  assert(op.hasValue() && "CFGBuilder.visitBinaryOperator: unexpected op type");
  Conversion cv = getSignedness(OpCode);
  // insert conversion if needed
  clang::Expr *lhs = EB.visitValue(L);
  if (cv == Signed || cv == Arithmetic) {
    lhs = castSigned(Ctx, TB, lhs);
  } else if (cv == Unsigned || cv == Logical) {
    lhs = castUnsigned(Ctx, TB, lhs);
  }
  clang::Expr *rhs = EB.visitValue(R);
  if (cv == Signed) {
    rhs = castSigned(Ctx, TB, rhs);
  } else if (cv == Unsigned) {
    rhs = castUnsigned(Ctx, TB, rhs);
  }

  // handle logical and/or
  if (*op == clang::BO_And && Result.getType()->isIntegerTy(1)) {
    op = clang::BO_LAnd;
  }
  if (*op == clang::BO_Or && Result.getType()->isIntegerTy(1)) {
    op = clang::BO_LOr;
  }

  clang::Expr *binop = createBinaryOperator(
      Ctx, lhs, rhs, op.getValue(), TB.getType(Result), clang::VK_PRValue);
  return binop;
}

void CFGBuilder::visitBinaryOperator(llvm::BinaryOperator &I) {
  auto binop = handleBinary(Ctx, EB, FCtx.getTypeBuilder(), I.getOpcode(), I,
                            I.getOperand(0), I.getOperand(1));
  addExprOrStmt(I, *binop);
  return;
}

void CFGBuilder::visitReturnInst(llvm::ReturnInst &I) {
  clang::Stmt *ret;
  ret = clang::ReturnStmt::Create(Ctx, clang::SourceLocation(),
                                  EB.visitValue(I.getReturnValue()), nullptr);
  addExprOrStmt(I, *ret);
  // not add to terminator!
  // Blk->setTerminator(CFGTerminator(ret));
}

/// Convert SelectInst to Ternary operator `?:`.
/// If the usage matches the and/or logical operator, then convert to && or ||
void CFGBuilder::visitSelectInst(llvm::SelectInst &I) {
  auto &TB = FCtx.getTypeBuilder();
  auto RTy = TB.getType(I);
  clang::Expr *cond = EB.visitValue(I.getCondition());
  if (I.getType()->isIntegerTy(1)) {
    // select i1 expr1, i1 true, i1 expr2 -> expr1 || expr2
    if (auto i = llvm::dyn_cast<llvm::ConstantInt>(I.getTrueValue())) {
      if (i->isOne()) {
        clang::Expr *fl = EB.visitValue(I.getFalseValue());
        auto exp = createBinaryOperator(Ctx, cond, fl, clang::BO_LOr, RTy,
                                        clang::VK_PRValue);
        addExprOrStmt(I, *exp);
        return;
      }
    }
    // select i1 expr1, i1 expr2, i1 false -> expr1 && expr2
    if (auto i = llvm::dyn_cast<llvm::ConstantInt>(I.getFalseValue())) {
      if (i->isZero()) {
        clang::Expr *tr = EB.visitValue(I.getTrueValue());
        auto exp = createBinaryOperator(Ctx, cond, tr, clang::BO_LAnd, RTy,
                                        clang::VK_PRValue);
        addExprOrStmt(I, *exp);
        return;
      }
    }
  }
  clang::Expr *tr = EB.visitValue(I.getTrueValue());
  clang::Expr *fl = EB.visitValue(I.getFalseValue());
  auto exp =
      createConditionalOperator(Ctx, cond, tr, fl, RTy, clang::VK_PRValue);
  addExprOrStmt(I, *exp);
}

void CFGBuilder::visitSwitchInst(llvm::SwitchInst &I) {
  assert(I.getNumSuccessors() > 2 &&
         "CFGBuilder.visitSwitchInst: SwitchInst with less than 2 successors?");
  Blk->setTerminator(SwitchTerminator(EB.visitValue(I.getCondition())));
  // Add case expressions
  auto cases = std::get_if<SwitchTerminator>(&Blk->getTerminator());
  for (auto &expr : I.cases()) {
    cases->cases().push_back(EB.visitValue(expr.getCaseValue()));
  }
  // there is not default value for switch.
  // cases->cases().push_back(EB.visitValue(I.case_default()->getCaseValue()));
}

const llvm::StringSet<> SAContext::Keywords = {
#define KEYWORD(NAME, FLAGS) #NAME,
#include "clang/Basic/TokenKinds.def"
#undef KEYWORD
};

bool SAContext::isKeyword(llvm::StringRef Name) {
  return Keywords.find(Name) != Keywords.end();
}

template <class T>
clang::IdentifierInfo *getNewIdentifierInfo(T &Names,
                                            clang::IdentifierTable &Idents,
                                            llvm::StringRef Name) {
  llvm::SmallString<128> Buf;
  auto Iter = Names.find(Name);
  unsigned ID = 0;
  while (Iter != Names.end()) {
    // already exist, add suffix to the name
    Iter = Names.find((Name + llvm::Twine(ID)).toStringRef(Buf));
    ++ID;
  }
  return &Idents.get(Name);
}

// clang::IdentifierInfo *getGlobalIdentifierInfo(clang::ASTContext &Ctx,
//                                                llvm::StringRef Name) {
//   auto &Idents = Ctx.Idents;
//   return getNewIdentifierInfo(Idents, Idents, Name);
// }

ExprBuilder::ExprBuilder(SAFuncContext &FCtx)
    : FCtx(&FCtx), SCtx(FCtx.getSAContext()), Ctx(FCtx.getASTContext()),
      TB(FCtx.getTypeBuilder()) {}

clang::ASTContext &SAFuncContext::getASTContext() {
  return ctx.getASTContext();
}

/// Decompile the module to c and print to a file.
void decompileModule(llvm::Module &M, llvm::raw_fd_ostream &OS, Options opts,
                     std::unique_ptr<HighTypes> HT) {
  if (!opts.noDemoteSSA) {
    // demote SSA using reg2mem
    notdec::llvm2c::demoteSSA(M);
  }
  LLVM_DEBUG(
      llvm::dbgs() << "\n========= IR before structural analysis =========\n");
  LLVM_DEBUG(llvm::dbgs() << M);
  LLVM_DEBUG(llvm::dbgs()
             << "\n========= End IR before structural analysis =========\n");

  // TODO remove:
  printModule(M, "current.ll");
  if (HT != nullptr) {
    HT->dump();
  }

  SAContext Ctx(const_cast<llvm::Module &>(M), opts, HT);
  Ctx.createDecls();
  for (const llvm::Function &F : M) {
    if (F.isDeclaration()) {
      continue;
    }
    SAFuncContext &FuncCtx =
        Ctx.getFuncContext(const_cast<llvm::Function &>(F));
    FuncCtx.run();
    LLVM_DEBUG(llvm::dbgs() << "Function: " << F.getName() << "\n");
    LLVM_DEBUG(FuncCtx.getFunctionDecl()->dump());
  }
  Ctx.getASTContext().getTranslationUnitDecl()->print(OS);
}

bool usedInBlock(llvm::Instruction &inst, llvm::BasicBlock &bb) {
  for (llvm::User *U : inst.users()) {
    if (llvm::Instruction *UI = llvm::dyn_cast<llvm::Instruction>(U)) {
      if (UI->getParent() == &bb) {
        return true;
      }
    }
  }
  return false;
}

bool onlyUsedInBlock(llvm::Instruction &inst) {
  llvm::BasicBlock *BB = inst.getParent();
  if (inst.hasOneUse()) {
    for (llvm::User *U : inst.users()) {
      if (llvm::Instruction *UI = llvm::dyn_cast<llvm::Instruction>(U)) {
        if (UI->getParent() == BB) {
          return true;
        }
      }
    }
  }
  return false;
}

clang::StorageClass SAContext::getStorageClass(llvm::GlobalValue &GV) {
  return GV.isDeclaration() ? clang::SC_Extern
         : GV.getLinkage() == llvm::GlobalValue::LinkageTypes::InternalLinkage
             ? clang::SC_Static
             : clang::SC_None;
}

void SAContext::createDecls() {
  // visit all record definitions
  for (llvm::StructType *Ty : M.getIdentifiedStructTypes()) {
    llvm::errs() << "Creating struct: " << *Ty << "\n";
    // create RecordDecls
    TB.visitStructType(*Ty);
  }

  // TODO: add comments
  // getASTContext().addComment(
  //     clang::RawComment(getASTContext().getSourceManager(),
  //                       clang::SourceRange(), clang::CommentOptions(),
  //                       false));

  // create function decls
  for (llvm::Function &F : M) {
    getValueNamer().clearFuncCount();
    llvm::SmallVector<clang::ParmVarDecl *, 16> Params;
    // llvm::errs() << "Function: " << F.getName() << "\n";
    // create function decl
    clang::IdentifierInfo *II =
        getIdentifierInfo(getValueNamer().getFuncName(F));
    clang::FunctionProtoType::ExtProtoInfo EPI;
    EPI.Variadic = F.isVarArg();
    clang::FunctionDecl *FD = clang::FunctionDecl::Create(
        getASTContext(), getASTContext().getTranslationUnitDecl(),
        clang::SourceLocation(), clang::SourceLocation(), II,
        TB.getFunctionType(F, EPI), nullptr, getStorageClass(F));

    // create function parameters
    for (llvm::Argument &Arg : F.args()) {
      clang::IdentifierInfo *ArgII =
          getIdentifierInfo(getValueNamer().getArgName(Arg));
      clang::ParmVarDecl *PVD = clang::ParmVarDecl::Create(
          getASTContext(), FD, clang::SourceLocation(), clang::SourceLocation(),
          ArgII, TB.getType(Arg), nullptr, clang::SC_None, nullptr);
      Params.push_back(PVD);
    }
    FD->setParams(Params);

    getASTContext().getTranslationUnitDecl()->addDecl(FD);
    globalDecls.insert(std::make_pair(&F, FD));
  }

  // create global variable decls
  for (llvm::GlobalVariable &GV : M.globals()) {
    clang::IdentifierInfo *II =
        getIdentifierInfo(getValueNamer().getGlobName(GV));
    auto Ty = TB.getType(GV)->getPointeeType();
    if (GV.isConstant()) {
      Ty = Ty.withConst();
    }
    clang::VarDecl *VD = clang::VarDecl::Create(
        getASTContext(), getASTContext().getTranslationUnitDecl(),
        clang::SourceLocation(), clang::SourceLocation(), II, Ty, nullptr,
        getStorageClass(GV));

    getASTContext().getTranslationUnitDecl()->addDecl(VD);
    globalDecls.insert(std::make_pair(&GV, VD));
  }

  // create global variable initializers
  for (llvm::GlobalVariable &GV : M.globals()) {
    auto VD = getGlobalVarDecl(GV);
    if (GV.hasInitializer()) {
      VD->setInit(EB.visitInitializer(GV.getInitializer(), VD->getType()));
    }
  }
}

void SAFuncContext::run() {
  // will not run over declaration
  assert(!Func.isDeclaration());
  ctx.getValueNamer().clearFuncCount();
  auto PrevFD = ctx.getFunctionDecl(Func);
  assert(PrevFD != nullptr && "SAFuncContext::run: FunctionDecl is null, not "
                              "created by `SAContext::createDecls`?");
  // 1. build the CFGBlocks
  CFGBuilder Builder(*this);

  // create function decl again, and set the previous declaration.
  clang::IdentifierInfo *II =
      ctx.getIdentifierInfo(getSAContext().getValueNamer().getFuncName(Func));
  clang::FunctionProtoType::ExtProtoInfo EPI;
  EPI.Variadic = Func.isVarArg();
  FD = clang::FunctionDecl::Create(
      getASTContext(), getASTContext().getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), II, PrevFD->getType(),
      nullptr, SAContext::getStorageClass(Func));
  FD->setPreviousDeclaration(PrevFD);

  std::deque<llvm::BasicBlock *> TopoSort;
  for (llvm::po_iterator<llvm::BasicBlock *>
           I = po_begin(&Func.getEntryBlock()),
           IE = po_end(&Func.getEntryBlock());
       I != IE; ++I) {
    TopoSort.push_front(*I);
  }
  for (llvm::BasicBlock *bb : TopoSort) {
    // create initial CFGBlocks by converting each instructions (except for
    // Terminators) to Clang AST
    Builder.run(*bb);
  }

  // connect the edges
  // for if block, the true edge comes first in the successor list.
  // for switch block, first successor is the default branch, then the edge
  // sequences matches the switch expr list.
  for (llvm::BasicBlock &bb : Func) {
    auto term = bb.getTerminator();
    for (auto succ : llvm::successors(term)) {
      auto src = getBlock(bb);
      auto dst = getBlock(*succ);
      LLVM_DEBUG(llvm::dbgs() << "Adding edge from " << src->getBlockID()
                              << " to " << dst->getBlockID() << "\n");
      addEdge(src, dst);
    }
  }

  // // Create the stub exit block.
  // // TODO the exit block is currently not used. Edges to exit block are not
  // // maintained.
  // CFG::iterator Exit = Cfg->createBlock();
  // Cfg->setExit(*Exit);

  LLVM_DEBUG(llvm::dbgs() << "========" << Func.getName() << ": "
                          << "Before CFGCleaner ========"
                          << "\n");
  LLVM_DEBUG(Cfg->dump(getASTContext().getLangOpts(), getOpts().enableColor));

  // clean up empty blocks
  CFGCleaner CC(*this);
  CC.execute();

  LLVM_DEBUG(llvm::dbgs() << "========" << Func.getName() << ": "
                          << "Before Structural Analysis ========"
                          << "\n");
  LLVM_DEBUG(Cfg->dump(getASTContext().getLangOpts(), getOpts().enableColor));

  // create logical and/or
  CompoundConditionBuilder CCB(*this);
  CCB.execute();

  // ============== structural analysis ==============
  // TODO: create structural analysis according to cmdline
  if (getOpts().algo == SA_Goto) {
    Goto SA(*this);
    SA.execute();
  } else if (getOpts().algo == SA_Phoenix) {
    Phoenix SA(*this);
    SA.execute();
  } else {
    llvm::errs() << "SAFuncContext::run: unknown algorithm: " << getOpts().algo
                 << "\n";
    std::abort();
  }

  LLVM_DEBUG(llvm::dbgs() << "========" << Func.getName() << ": "
                          << "After Structural Analysis ========"
                          << "\n");
  LLVM_DEBUG(Cfg->dump(getASTContext().getLangOpts(), getOpts().enableColor));

  // Finalize steps
  // After structural analysis, if things goes well, the CFG should have only
  // one linear block. But we still pick up other blocks if there are any.
  std::set<CFGBlock *> visited;
  std::vector<clang::Stmt *> Stmts;

  assert(&Cfg->front() == &Cfg->getEntry());
  for (auto BB : *Cfg) {
    // add labelStmt
    IStructuralAnalysis::addAllStmtTo(BB, Stmts, true);
  }

  // create a compound stmt as function body
  auto CS = clang::CompoundStmt::Create(
      getASTContext(), Stmts, clang::SourceLocation(), clang::SourceLocation());
  FD->setBody(CS);
  getASTContext().getTranslationUnitDecl()->addDecl(FD);
}

clang::Expr *ExprBuilder::createCompoundLiteralExpr(llvm::Value *Val) {
  auto ObjTy = TB.getType(*Val);
  clang::Expr *ret = new (Ctx) clang::CompoundLiteralExpr(
      clang::SourceLocation(), Ctx.getTrivialTypeSourceInfo(ObjTy), ObjTy,
      clang::VK_LValue, visitValue(Val), false);
  ret = clang::ImplicitCastExpr::Create(Ctx, ObjTy, clang::CK_LValueToRValue,
                                        ret, nullptr, clang::VK_PRValue,
                                        clang::FPOptionsOverride());
  return ret;
}

clang::Expr *ExprBuilder::visitValue(llvm::Value *Val, clang::QualType Ty) {
  if (Val == nullptr) {
    return nullptr;
  }
  // Check for ExprMap
  if (FCtx != nullptr && FCtx->isExpr(*Val)) {
    return FCtx->getExpr(*Val);
  }
  if (llvm::Instruction *Inst = llvm::dyn_cast<llvm::Instruction>(Val)) {
    return visit(*Inst);
  } else if (llvm::Constant *C = llvm::dyn_cast<llvm::Constant>(Val)) {
    return visitConstant(*C, Ty);
  } else {
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "UnImplemented: ExprBuilder.visitValue cannot handle: "
                 << *Val << "\n";
    std::abort();
  }
}

clang::QualType TypeBuilder::getFunctionType(
    llvm::Function &F, const clang::FunctionProtoType::ExtProtoInfo &EPI) {
  return visitFunctionType(*F.getFunctionType(), EPI, &F);
}

clang::QualType TypeBuilder::visitFunctionType(
    llvm::FunctionType &Ty, const clang::FunctionProtoType::ExtProtoInfo &EPI,
    llvm::Function *ActualFunc) {
  llvm::SmallVector<clang::QualType, 8> Args(Ty.getNumParams());
  for (unsigned i = 0; i < Ty.getNumParams(); i++) {
    if (ActualFunc != nullptr) {
      Args[i] = getType(*ActualFunc->getArg(i));
    } else {
      Args[i] = visitType(*Ty.getParamType(i));
    }
  }
  clang::QualType RetTy;
  if (ActualFunc != nullptr && FuncRetTypes.count(ActualFunc) > 0) {
    RetTy = FuncRetTypes[ActualFunc];
  } else {
    RetTy = visitType(*Ty.getReturnType());
  }
  return Ctx.getFunctionType(RetTy, Args, EPI);
}

clang::RecordDecl *TypeBuilder::createRecordDecl(llvm::StructType &Ty,
                                                 bool isDefinition,
                                                 bool isNotLiteral) {
  clang::IdentifierInfo *II = nullptr;
  if (Ty.hasName()) {
    II = getIdentifierInfo(VN->getStructName(Ty));
  }

  clang::RecordDecl *prev = nullptr;
  clang::RecordDecl *decl = clang::RecordDecl::Create(
      Ctx, clang::TagDecl::TagKind::TTK_Struct, Ctx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), II, prev);
  // Set free standing so that unnamed struct can be combined:
  // (`struct {int x;} a,b`)
  // This also requires that the var decl uses a ElaboratedType whose owned tag
  // decl is the previous RecordDecl
  // See also https://lists.llvm.org/pipermail/cfe-dev/2021-February/067631.html
  decl->setFreeStanding(isNotLiteral);
  // save to map early so that recursive pointers are allowed, for example:
  // `struct A { struct A *a; };`
  typeMap[&Ty] = decl;
  if (isDefinition) {
    decl->startDefinition();
    for (unsigned i = 0; i < Ty.getNumElements(); i++) {
      auto FieldTy = visitType(*Ty.getElementType(i));
      // TODO handle field name.
      clang::IdentifierInfo *FieldII = getIdentifierInfo(VN->getFieldName(i));
      clang::FieldDecl *FieldDecl = clang::FieldDecl::Create(
          Ctx, decl, clang::SourceLocation(), clang::SourceLocation(), FieldII,
          FieldTy, nullptr, nullptr, false, clang::ICIS_NoInit);
      decl->addDecl(FieldDecl);
    }
    decl->completeDefinition();
  }
  Ctx.getTranslationUnitDecl()->addDecl(decl);
  return decl;
}

clang::QualType TypeBuilder::visitStructType(llvm::StructType &Ty) {
  // return if is cached in type map
  if (typeMap.find(&Ty) != typeMap.end()) {
    clang::RecordDecl *decl = llvm::cast<clang::RecordDecl>(typeMap[&Ty]);
    return Ctx.getRecordType(decl);
  }
  // if type not visited yet, create it.
  if (Ty.isLiteral()) {
    // Handle unnamed struct definition: (`struct {int x;} a,b`)
    // See also
    // https://lists.llvm.org/pipermail/cfe-dev/2021-February/067631.html create
    // a non-freestanding RecordDecl in place.
    auto decl = createRecordDecl(Ty, true, false);
    // This also requires that the var decl uses a ElaboratedType whose owned
    // tag decl is the previous RecordDecl
    auto ElabTy = Ctx.getElaboratedType(clang::ETK_Struct, nullptr,
                                        Ctx.getRecordType(decl), decl);
    return ElabTy;
  } else {
    // insertion is done in createRecordDecl
    return Ctx.getRecordType(createRecordDecl(Ty, true, true));
  }
}

clang::QualType TypeBuilder::getType(llvm::Value &Val) {
  if (ValueTypes.count(&Val) > 0) {
    return ValueTypes[&Val];
  } else if (auto F = llvm::dyn_cast<llvm::Function>(&Val)) {
    llvm::errs()
        << "Warning: getType for function (call getFunctionType instead!): "
        << F->getName() << "\n";
    return getFunctionType(*F, clang::FunctionProtoType::ExtProtoInfo());
  }
  return visitType(*Val.getType());
}

clang::QualType TypeBuilder::visitType(llvm::Type &Ty) {
  if (Ty.isPointerTy()) {
    return Ctx.getPointerType(visitType(*Ty.getPointerElementType()));
  } else if (Ty.isFunctionTy()) {
    return visitFunctionType(llvm::cast<llvm::FunctionType>(Ty),
                             clang::FunctionProtoType::ExtProtoInfo());
  } else if (Ty.isArrayTy()) {
    return Ctx.getConstantArrayType(visitType(*Ty.getArrayElementType()),
                                    llvm::APInt(32, Ty.getArrayNumElements()),
                                    nullptr, clang::ArrayType::Normal, 0);
  } else if (Ty.isStructTy()) {
    auto StructTy = llvm::cast<llvm::StructType>(&Ty);
    return visitStructType(*StructTy);
  }
  // simple primitive types
  if (Ty.isFloatTy()) {
    return Ctx.FloatTy;
  } else if (Ty.isDoubleTy()) {
    return Ctx.DoubleTy;
  } else if (Ty.isVoidTy()) {
    return Ctx.VoidTy;
  } else if (Ty.isIntegerTy(1)) {
    return Ctx.IntTy;
    // return Ctx.BoolTy;
  }

  if (Ty.isIntegerTy()) {
    auto ret = Ctx.getIntTypeForBitwidth(Ty.getIntegerBitWidth(), false);
    if (ret.isNull()) {
      llvm::errs() << "Warning: cannot find exact type for: " << Ty << "\n";
      ret = Ctx.getBitIntType(true, Ty.getIntegerBitWidth());
    }
    return ret;
  } else {
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "UnImplemented: TypeBuilder.visitType cannot handle: " << Ty
                 << "\n";
    std::abort();
  }
}

// This is separated because zero initializer requires type information
clang::Expr *ExprBuilder::visitInitializer(llvm::Value *Val,
                                           clang::QualType Ty) {

  return visitValue(Val, Ty);
}

clang::Expr *ExprBuilder::visitConstant(llvm::Constant &C, clang::QualType Ty) {
  if (llvm::GlobalObject *GO = llvm::dyn_cast<llvm::GlobalObject>(&C)) {
    // global variables and functions
    return addrOf(Ctx, makeDeclRefExpr(SCtx.getGlobalDecl(*GO)));
  } else if (llvm::ConstantAggregate *CA =
                 llvm::dyn_cast<llvm::ConstantAggregate>(&C)) {
    // struct and array
    llvm::SmallVector<clang::Expr *> vec(CA->getNumOperands());
    for (unsigned i = 0; i < CA->getNumOperands(); i++) {
      vec[i] = visitValue(CA->getOperand(i), Ty);
    }
    return new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), vec,
                                         clang::SourceLocation());
  } else if (llvm::ConstantDataSequential *CS =
                 llvm::dyn_cast<llvm::ConstantDataSequential>(&C)) {
    if (CS->isCString()) {
      return clang::StringLiteral::Create(
          Ctx, CS->getAsCString(), clang::StringLiteral::Ascii, false,
          Ctx.getStringLiteralArrayType(Ctx.CharTy, CS->getNumElements()),
          clang::SourceLocation());
    }
    // struct and array
    llvm::SmallVector<clang::Expr *> vec(CS->getNumElements());
    for (unsigned i = 0; i < CS->getNumElements(); i++) {
      vec[i] = visitValue(CS->getElementAsConstant(i), Ty);
    }
    return new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), vec,
                                         clang::SourceLocation());
  } else if (llvm::ConstantAggregateZero *CAZ =
                 llvm::dyn_cast<llvm::ConstantAggregateZero>(&C)) {
    assert(!Ty.isNull() && "ExprBuilder.visitConstant: Ty is null?");
    auto ret = new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), {},
                                             clang::SourceLocation());
    ret->setArrayFiller(new (Ctx) clang::ImplicitValueInitExpr(Ty));
    return ret;
    // We create ImplicitValueInitExpr for zero initializer but it requires type
    // information, so use visitInitializer instead
    // llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
    //              << "Error: ExprBuilder.visitConstant cannot handle "
    //                 "ConstantAggregateZero, use visitInitializer instead.\n";
  } else if (llvm::ConstantPointerNull *CPN =
                 llvm::dyn_cast<llvm::ConstantPointerNull>(&C)) {
    // TODO insert a ImplicitCastExpr<NullToPointer> to corresponding type?
    return clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, 0, false),
                                         Ctx.IntTy, clang::SourceLocation());
  } else if (llvm::ConstantInt *CI = llvm::dyn_cast<llvm::ConstantInt>(&C)) {
    auto val = CI->getValue();
    if (val.getBitWidth() == 1) {
      val = val.zext(32);
    }
    // TODO: eliminate Ui8 Ui16 i8 i16 suffix?
    // https://stackoverflow.com/questions/33659846/microsoft-integer-literal-extensions-where-documented
    return clang::IntegerLiteral::Create(Ctx, val, getType(*CI),
                                         clang::SourceLocation());
  } else if (llvm::ConstantFP *CFP = llvm::dyn_cast<llvm::ConstantFP>(&C)) {
    return clang::FloatingLiteral::Create(
        Ctx, CFP->getValueAPF(), true, getType(*CFP), clang::SourceLocation());
  } else if (llvm::ConstantExpr *CE = llvm::dyn_cast<llvm::ConstantExpr>(&C)) {
    // https://llvm.org/docs/LangRef.html#constant-expressions
    // handle gep
    if (CE->getOpcode() == llvm::Instruction::GetElementPtr) {
      return handleGEP(Ctx, *this, *llvm::cast<llvm::GEPOperator>(CE));
    }
    // handle casts
    switch (CE->getOpcode()) {
    case llvm::Instruction::Trunc:
    case llvm::Instruction::ZExt:
    case llvm::Instruction::SExt:
    case llvm::Instruction::FPTrunc:
    case llvm::Instruction::FPExt:
    case llvm::Instruction::UIToFP:
    case llvm::Instruction::SIToFP:
    case llvm::Instruction::FPToUI:
    case llvm::Instruction::FPToSI:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::BitCast:
    case llvm::Instruction::AddrSpaceCast:
      return handleCast(Ctx, CE->getContext(), *this, TB,
                        (llvm::Instruction::CastOps)CE->getOpcode(),
                        TB.getType(*CE), CE->getOperand(0));

    case llvm::Instruction::ICmp:
    case llvm::Instruction::FCmp:
      return handleCmp(Ctx, TB, *this,
                       (llvm::CmpInst::Predicate)CE->getPredicate(), CE,
                       CE->getOperand(0), CE->getOperand(1));
    case llvm::Instruction::Add:
    case llvm::Instruction::FAdd:
    case llvm::Instruction::Sub:
    case llvm::Instruction::FSub:
    case llvm::Instruction::Mul:
    case llvm::Instruction::FMul:
    case llvm::Instruction::UDiv:
    case llvm::Instruction::SDiv:
    case llvm::Instruction::FDiv:
    case llvm::Instruction::URem:
    case llvm::Instruction::SRem:
    case llvm::Instruction::FRem:
    case llvm::Instruction::Shl:
    case llvm::Instruction::LShr:
    case llvm::Instruction::AShr:
    case llvm::Instruction::And:
    case llvm::Instruction::Or:
    case llvm::Instruction::Xor:
      return handleBinary(Ctx, *this, TB,
                          (llvm::Instruction::BinaryOps)CE->getOpcode(), *CE,
                          CE->getOperand(0), CE->getOperand(1));
    default:
      llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                   << "UnImplemented: ExprBuilder.visitConstant cannot handle "
                      "ConstantExpr: "
                   << *CE << "\n";
      std::abort();
    }
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "UnImplemented: ExprBuilder.visitConstant cannot handle: "
               << C << "\n";
  std::abort();
}

/// get the label for the block. Create if not exist.
clang::LabelDecl *IStructuralAnalysis::getBlockLabel(CFGBlock *Blk,
                                                     bool prepend) {
  if (auto label = Blk->getLabel()) {
    return llvm::cast<clang::LabelStmt>(label)->getDecl();
  } else {
    // if prepend and already has a label, return the label
    if (prepend) {
      if (Blk->size() > 0 && Blk->front().getAs<CFGStmt>().hasValue()) {
        if (auto label = llvm::dyn_cast<clang::LabelStmt>(
                Blk->front().getAs<CFGStmt>()->getStmt())) {
          return label->getDecl();
        }
      }
    }
    auto &astCtx = FCtx.getASTContext();
    auto bb = FCtx.getBlock(*Blk);
    clang::IdentifierInfo *II = FCtx.getIdentifierInfo(
        getValueNamer().getBlockName(*bb, Blk->getBlockID()));
    auto LabelDecl = clang::LabelDecl::Create(astCtx, FCtx.getFunctionDecl(),
                                              clang::SourceLocation(), II);
    // create LabelStmt
    clang::LabelStmt *LabelStmt = new (astCtx)
        clang::LabelStmt(clang::SourceLocation(), LabelDecl,
                         new (astCtx) clang::NullStmt(clang::SourceLocation()));
    if (prepend) {
      Blk->prependStmt(LabelStmt);
    } else {
      Blk->setLabel(LabelStmt);
    }
    return LabelDecl;
  }
}

SAFuncContext::SAFuncContext(SAContext &ctx, llvm::Function &func)
    : ctx(ctx), Func(func), TB(ctx.getTypeBuilder()) {
  Cfg = std::make_unique<CFG>();
  Names = std::make_unique<llvm::StringSet<>>();
}

void ValueNamer::escapeBuf() {
  for (auto &c : Buf) {
    if (!std::isalnum(c) && c != '_') {
      c = '_';
    }
  }
}

llvm::StringRef ValueNamer::getValueName(llvm::Value &Val, const char *prefix,
                                         unsigned int &id) {
  Buf.clear();
  llvm::raw_svector_ostream OS(Buf);
  if (Val.hasName() && !SAContext::isKeyword(Val.getName())) {
    OS << Val.getName();
    escapeBuf();
  } else {
    OS << prefix << id++;
  }
  return OS.str();
}

llvm::StringRef ValueNamer::getTypeName(llvm::StructType &Ty,
                                        const char *prefix, unsigned int &id) {
  Buf.clear();
  llvm::raw_svector_ostream OS(Buf);
  if (Ty.hasName() && !SAContext::isKeyword(Ty.getName())) {
    OS << Ty.getName();
    escapeBuf();
  } else {
    OS << prefix << id++;
  }
  return OS.str();
}

const Options &SAFuncContext::getOpts() const { return ctx.getOpts(); }

} // namespace notdec::llvm2c
