#include <cassert>
#include <cctype>
#include <clang/Sema/Ownership.h>
#include <cstddef>
#include <llvm/IR/Instruction.h>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <utility>

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/MemorySSA.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ArrayRef.h>
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
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Use.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar/GVN.h>

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
#include <clang/Frontend/ASTUnit.h>
#include <clang/Tooling/Transformer/RewriteRule.h>
#include <variant>
#include <vector>

#include "ASTManager.h"
#include "ASTPrinter/DeclPrinter.h"
#include "Interface/HType.h"
#include "TypeManager.h"
#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/CompoundConditionBuilder.h"
#include "notdec-llvm2c/Goto.h"
#include "notdec-llvm2c/Interface.h"
#include "notdec-llvm2c/Phoenix.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include "notdec-llvm2c/Utils.h"

#define DEBUG_TYPE "structural-analysis"

namespace notdec::llvm2c {

bool canIgnore(llvm::Instruction::CastOps OpCode) {
  switch (OpCode) {
  case llvm::Instruction::PtrToInt:
  case llvm::Instruction::IntToPtr:
  case llvm::Instruction::BitCast:
    return true;
  default:
    return false;
  }
  return false;
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

/// Handle the cast instruction. Shared by instruction visitor and expr
/// builder (for ConstantExpr operators).
clang::Expr *createCCast(clang::ASTContext &Ctx, ExprBuilder &EB,
                         TypeBuilder &TB, llvm::Instruction::CastOps OpCode,
                         llvm::User *User, clang::QualType destTy,
                         clang::Expr *Val) {
  auto srcTy = Val->getType();
  if (destTy->canDecayToPointerType()) {
    destTy = Ctx.getDecayedType(destTy);
  }
  if (TB.isTypeCompatible(srcTy, destTy)) {
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
bool isEntryLikeBlock(llvm::BasicBlock &bb) {
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

void CFGBuilder::visitAllocaInst(llvm::AllocaInst &I) {
  // if (I.getFunction()->getName() == "find_matches") {
  //   std::cerr << "here\n";
  // }

  // check if the instruction is in the entry block.
  if (isEntryLikeBlock(*I.getParent()) && !I.isArrayAllocation()) {
    // create a local variable
    auto II = FCtx.getIdentifierInfo(FCtx.getValueNamer().getTempName(I));
    auto PTy = FCtx.getTypeBuilder().getType(&I, nullptr, -1);

    // for array type, we do not need to get pointee type.
    auto VDTy = toLValueType(
        Ctx, PTy->getPointeeType().isNull() ? PTy : PTy->getPointeeType());

    if (VDTy->isVoidType()) {
      VDTy = FCtx.getTypeBuilder().visitType(*I.getType())->getPointeeType();
    }

    // TODO: ensure that type size is the same to ensure semantic.
    clang::VarDecl *VD = clang::VarDecl::Create(
        Ctx, FCtx.getFunctionDecl(), clang::SourceLocation(),
        clang::SourceLocation(), II, VDTy, nullptr, clang::SC_None);

    // Create a decl statement.
    clang::DeclStmt *DS = new (Ctx)
        clang::DeclStmt(clang::DeclGroupRef(VD), clang::SourceLocation(),
                        clang::SourceLocation());
    VarDecls.push_back(DS);
    FCtx.getFunctionDecl()->addDecl(VD);
    // When alloca is referenced, it refers to the address of the DeclRefExpr
    auto addr = addrOf(Ctx, makeDeclRefExpr(VD));
    FCtx.addMapping(&I, *addr);
  } else {
    auto *FD = FCtx.getSAContext().getIntrinsic("alloca");
    if (I.isArrayAllocation()) {
      // only allow alloca char, n;
      assert(I.getAllocatedType()->isIntegerTy(8));
      auto Arg = EB.visitValue(I.getOperand(0), &I, 0);
      Arg = getTypeBuilder().checkCast(Arg, FD->getParamDecl(0)->getType());
      auto Call = clang::CallExpr::Create(
          Ctx, makeDeclRefExpr(FD), {Arg}, FD->getReturnType(),
          clang::VK_PRValue, clang::SourceLocation(),
          clang::FPOptionsOverride());
      addExprOrStmt(I, *Call);
    } else {
      auto AT = I.getAllocatedType();
      auto &DL = I.getModule()->getDataLayout();
      assert(AT->isSized() && DL.getTypeAllocSize(AT).getFixedSize() != 0);

      auto PTy = FD->getParamDecl(0)->getType();
      auto Size = llvm::APInt(Ctx.getTypeSize(PTy),
                              DL.getTypeAllocSize(AT).getFixedSize());
      auto Arg = clang::IntegerLiteral::Create(Ctx, Size, PTy,
                                               clang::SourceLocation());
      auto Call = clang::CallExpr::Create(
          Ctx, makeDeclRefExpr(FD), {Arg}, FD->getReturnType(),
          clang::VK_PRValue, clang::SourceLocation(),
          clang::FPOptionsOverride());
      addExprOrStmt(I, *Call);
    }
  }
}

clang::Expr *handleGEP(clang::ASTContext &Ctx, clang::Expr *Val,
                       llvm::ArrayRef<clang::Expr *> Indexes) {
  const clang::Type *Ty = Val->getType().getTypePtr();
  for (unsigned i = 0; i < Indexes.size(); i++) {
    clang::Expr *Index = Indexes[i];
    if (auto PointerTy = Ty->getAs<clang::PointerType>()) {
      // 1. pointer arithmetic + deref
      Ty = PointerTy->getPointeeType().getTypePtr();
      if (Index == nullptr ||
          (llvm::isa<clang::IntegerLiteral>(Index) &&
           llvm::cast<clang::IntegerLiteral>(Index)->getValue().isZero())) {
        // we use nullptr as int 0 here.
        Val = deref(Ctx, Val);
        continue;
      }
      Val = createBinaryOperator(Ctx, Val, Index, clang::BO_Add, Val->getType(),
                                 clang::VK_LValue);
      Val = deref(Ctx, Val);
    } else if (auto ArrayTy = Ty->getAsArrayTypeUnsafe()) {
      // 2. array indexing
      Ty = ArrayTy->getElementType().getTypePtr();
      Val = new (Ctx) clang::ArraySubscriptExpr(
          Val, Index, ArrayTy->getElementType(), clang::VK_LValue,
          clang::OK_Ordinary, clang::SourceLocation());
    } else if (auto *RecordTy = Ty->getAs<clang::RecordType>()) {
      // 3. field reference
      auto Decl = RecordTy->getDecl();
      clang::FieldDecl *TargetField = nullptr;

      auto Field = Decl->field_begin();
      assert(llvm::isa<clang::IntegerLiteral>(Index) &&
             "Gep index is not constant int!");
      auto IndexNum =
          llvm::cast<clang::IntegerLiteral>(Index)->getValue().getZExtValue();
      std::advance(Field, IndexNum);
      TargetField = *Field;

      if (TargetField == nullptr) {
        // Check if the offset is contained within a field.
        // TODO
        llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                     << "Warning: handleGEP cannot "
                        "find field at offset: ";
        // assert(false);
        return nullptr;
      }
      Val = createMemberExpr(Ctx, Val, TargetField);
    } else {
      llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                   << "UnImplemented: handleGEP cannot "
                      "handle type: ";
      Ty->dump();
      llvm::errs() << "\n";
      std::abort();
    }
  }
  // implicit addrOf at the end of GEP
  return addrOf(Ctx, Val);
}

clang::Expr *TypeBuilder::ptrAdd(clang::Expr *Val, clang::Expr *Index) {
  return CT->handlePtrAdd(Val, Index);
}

std::vector<clang::Expr *> TypeBuilder::tryGepZero(clang::Expr *Val) {
  return CT->tryAddZero(Val);
}

clang::Expr *handleGEP(clang::ASTContext &Ctx, ExprBuilder &EB,
                       llvm::GEPOperator &I) {
  clang::Expr *Val = EB.visitValue(I.getPointerOperand(), &I, 0);
  llvm::SmallVector<clang::Expr *, 8> Indices;

  if (I.hasAllZeroIndices()) {
    return Val;
  }
  // convert the gep to ptr add, then solve using ptr add offset.

  for (unsigned i = 0; i < I.getNumIndices(); i++) {
    llvm::Value *LIndex = *(I.idx_begin() + i);
    clang::Expr *Index = EB.visitValue(LIndex, &I, i + 1);
    Indices.push_back(Index);
  }

  return handleGEP(Ctx, Val, Indices);
}

void CFGBuilder::visitGetElementPtrInst(llvm::GetElementPtrInst &I) {
  addExprOrStmt(I, *handleGEP(Ctx, EB, llvm::cast<llvm::GEPOperator>(I)));
}

void CFGBuilder::visitStoreInst(llvm::StoreInst &I) {
  // skip store undef value
  if (isa<llvm::UndefValue>(I.getValueOperand())) {
    llvm::errs() << "Skipping undef store! Probably uninitialized variable?: "
                 << I << "\n";
    return;
  }
  // store = assign + deref left.
  clang::Expr *Val = EB.visitValue(I.getValueOperand(), &I, 0);
  clang::Expr *Ptr = EB.visitValue(I.getPointerOperand(), &I, 1);
  Ptr = getNoCast(Ptr);
  // implicit inttoptr
  if (!Ptr->getType()->isPointerType()) {
    Ptr = createCStyleCastExpr(
        Ctx, getTypeBuilder().visitType(*I.getPointerOperandType()),
        clang::VK_PRValue, clang::CastKind::CK_BitCast, Ptr);
  }
  // 1. 确定assign的类型
  clang::Expr *Val1 = Val;
  clang::Expr *Ptr1 = Ptr;
  // assign最终的类型。
  QualType Ty;
  auto StoreSize = getLLVMTypeSize(I.getValueOperand()->getType(),
                                   getTypeBuilder().getPointerSizeInBits());

  if (StoreSize == 1) {
    Ty = getBoolTy(Ctx);
  }

  // 2. 优先左边的类型
  if (Ty.isNull()) {
    // 获得左边指针+0可能的值，依次判断类型
    auto Vals2 = getTypeBuilder().tryAddZero(Ptr);
    Vals2.insert(Vals2.begin(), Ptr);
    for (auto V : Vals2) {
      assert(V->getType()->isPointerType());
      auto VTy = toLValueType(Ctx, V->getType());
      auto Pte = VTy->getPointeeType();
      // 检查类型是否符合store的大小
      if (Pte->isArrayType() || Pte->isRecordType()) {
        continue;
      }
      auto TS = expectedToOptional(getTypeBuilder().getTypeSize(Pte));
      if (TS && *TS == StoreSize) {
        Ty = Pte;
        Ptr1 = V;
        break;
      }
    }
  }

  if (Ty.isNull()) {
    if (!Val1->getType()->isArrayType()) {
      auto TS =
          expectedToOptional(getTypeBuilder().getTypeSize(Val1->getType()));
      if (TS && *TS == StoreSize) {
        Ty = Val1->getType();
      }
    }
  }

  // use simple int if the type is not found.
  if (Ty.isNull()) {
    llvm::errs() << "Warning: Cannot find type for store inst: " << I << "\n";
    Ty = Ctx.getIntTypeForBitwidth(StoreSize, true);
  }

  Ty = toLValueType(Ctx, Ty);
  Val1 = getTypeBuilder().checkCast(Val1, Ty);
  Ptr1 = getTypeBuilder().checkCast(Ptr1, Ctx.getPointerType(Ty));
  auto PtrD = deref(Ctx, Ptr1);
  // // if there is addrof, cast may be eliminated, we need to cast again.
  // PtrD = getTypeBuilder().checkCast(PtrD, Ty);
  clang::Expr *assign = createBinaryOperator(Ctx, PtrD, Val1, clang::BO_Assign,
                                             Ptr->getType(), clang::VK_LValue);
  addExprOrStmt(I, *assign);
}

void CFGBuilder::visitLoadInst(llvm::LoadInst &I) {
  clang::Expr *Ptr = EB.visitValue(I.getPointerOperand(), &I, 0);
  Ptr = getNoCast(Ptr);
  // implicit inttoptr
  if (!Ptr->getType()->isPointerType()) {
    Ptr = createCStyleCastExpr(
        Ctx, getTypeBuilder().visitType(*I.getPointerOperandType()),
        clang::VK_PRValue, clang::CastKind::CK_BitCast, Ptr);
  }

  clang::Expr *Ptr1 = Ptr;
  // load type
  QualType Ty;
  auto Size =
      getLLVMTypeSize(I.getPointerOperandType()->getPointerElementType(),
                      getTypeBuilder().getPointerSizeInBits());

  if (Size == 1) {
    Ty = getBoolTy(Ctx);
  }

  if (Ty.isNull()) {
    auto Vals2 = getTypeBuilder().tryAddZero(Ptr);
    Vals2.insert(Vals2.begin(), Ptr);
    for (auto V : Vals2) {
      assert(V->getType()->isPointerType());
      auto VTy = V->getType();
      // VTy = toLValueType(Ctx, VTy);
      auto Pte = VTy->getPointeeType();
      // 检查类型是否符合store的大小
      if (Pte->isArrayType() || Pte->isRecordType()) {
        continue;
      }
      auto TS = expectedToOptional(getTypeBuilder().getTypeSize(Pte));
      if (TS && *TS == Size) {
        Ty = Pte;
        Ptr1 = V;
        break;
      }
    }
  }

  if (Ty.isNull()) {
    llvm::errs() << "Warning: Cannot find type for load inst: " << I << "\n";
    Ty = Ctx.getIntTypeForBitwidth(Size, true);
  }

  assert(!Ty->isVoidType());
  Ptr1 = getTypeBuilder().checkCast(Ptr1, Ctx.getPointerType(Ty));
  Ptr1 = deref(Ctx, Ptr1);

  // TODO: 尝试重写Ptr1，确保其指针唯一。
  SimpleRewriter SR(Ctx);
  auto R = SR.TransformExpr(Ptr1);
  assert(!R.isInvalid());
  Ptr1 = R.get();
  assert(Ptr1 != nullptr);

  // 获取一个槽位用来放临时表达式
  auto Ind = allocateSlot();

  // 调用专用函数，暂存到Map同时，存储相关Info
  LoadExprCreater LEC(Ind, getFCtx(), *getBlk(), I, Ptr1, Ty);
  FCtx.addLoadExpr(I, Ptr1, LEC);

  // TODO然后去改addExprOrStmt里面加入语句的地方，都wrap起来。
  // addExprOrStmt(I, *Ptr1);
}

LoadExprCreater::LoadExprCreater(size_t Ind, SAFuncContext &FCtx, CFGBlock &Blk,
                                 llvm::LoadInst &Load, clang::Expr *E,
                                 QualType Ty)
    : FCtx(FCtx), Blk(Blk), Ind(Ind), Load(Load), E(E), Ty(Ty) {
  assert(Ind < Blk.size());
}

// Checks if this->Load can be moved before InsertLoc
bool LoadExprCreater::canClone(llvm::Instruction *InsertLoc) {
  if (&this->Load == InsertLoc) {
    return true;
  }
  if (this->Load.isVolatile()) {
    return false;
  }
  // 获得MemorySSA，然后分析
  auto &MSSAR =
      FCtx.getFAM().getResult<llvm::MemorySSAAnalysis>(FCtx.getFunction());
  auto &MSSA = MSSAR.getMSSA();
  auto &DT =
      FCtx.getFAM().getResult<llvm::DominatorTreeAnalysis>(FCtx.getFunction());
  // MSSA.ensureOptimizedUses();
  auto Walker = MSSA.getWalker();

  llvm::MemoryLocation Loc = llvm::MemoryLocation::get(&Load);

  llvm::MemoryUse *LoadMU = cast<llvm::MemoryUse>(MSSA.getMemoryAccess(&Load));
  llvm::MemoryAccess *UserMA = nullptr;
  // Because we insert before, we do not need to consider it.
  // auto UserMA1 = MSSA.getMemoryAccess(InsertLoc);
  // if (UserMA1 && !isa<llvm::MemoryUse>(UserMA1)) {
  //   UserMA = UserMA1;
  // }
  // Walk backward to find the most recent memory access (non-MemoryUse) in the
  // block. Because getClobberingMemoryAccess assert arg is not MemoryUse.
  llvm::BasicBlock *BB = InsertLoc->getParent();
  auto It = InsertLoc->getIterator();
  while (UserMA == nullptr) {
    while (It != BB->begin()) {
      --It;
      // if we met the load inst without any memory def in the middle.
      if (&*It == &this->Load) {
        return true;
      }
      auto UserMA1 = MSSA.getMemoryAccess(&*It);
      if (UserMA1 && !isa<llvm::MemoryUse>(UserMA1)) {
        UserMA = UserMA1;
      }
      if (UserMA)
        break;
    }
    // If still nullptr, get for the block (if any)
    if (!UserMA) {
      UserMA = MSSA.getMemoryAccess(BB);
    }
    // If still nullptr, get from only dominator
    if (!UserMA) {
      if (auto Pred = BB->getSinglePredecessor()) {
        BB = Pred;
        It = Pred->end();
        continue;
      }
    }
    // If still nullptr, walk to direct dominator
    if (!UserMA) {
      auto *BBNode =DT.getNode(BB);
      if (BBNode) {
        auto *IDomNode = BBNode->getIDom();
        if (IDomNode) {
            auto *ImmediateDominator = IDomNode->getBlock();
            BB = ImmediateDominator;
            It = ImmediateDominator->end();
            continue;
        }
      }
    }
    if (UserMA) {
      break;
    }
    if (UserMA == nullptr) {
      llvm::errs() << "Cannot find memory SSA for this inst!\n";
      MemorySSAAnnotatedWriter MSSAW(&MSSA);
      BB->getParent()->print(llvm::errs(), &MSSAW);
      llvm::errs() << "We are checking if " << this->Load
                   << " can be moved before " << *InsertLoc << "!\n";
    }
    assert(UserMA != nullptr);
  }
  // auto *UserMU = dyn_cast<llvm::MemoryUse>(UserMA);
  llvm::MemoryAccess *DefAtLoad = Walker->getClobberingMemoryAccess(LoadMU);
  llvm::MemoryAccess *DefAtUser =
      Walker->getClobberingMemoryAccess(UserMA, Loc);
  if (DefAtLoad == DefAtUser) {
    return true; // Good to reuse
  } else {
    return false; // Value updated! Cannot reuse.
  }
}

clang::Expr *LoadExprCreater::getSafeExpr() {
  if (Cached) {
    return Cached;
  }
  auto DRef = FCtx.cacheExpr(Load, E, Blk, Ty, Ind);
  this->Cached = DRef;
  return DRef;
}

clang::Expr *handleCmp(clang::ASTContext &Ctx, TypeBuilder &TB, ExprBuilder &EB,
                       llvm::CmpInst::Predicate OpCode, llvm::User *Result,
                       llvm::Value *Op0, llvm::Value *Op1) {
  clang::Expr *cmp;
  clang::Expr *lhs = EB.visitValue(Op0, Result, 0);
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
        TB.getType(Result, nullptr, -1), clang::VK_PRValue);
    if (llvm::isa<llvm::Constant>(Op1)) {
      return lhs;
    } else {
      rhs = EB.visitValue(Op1, Result, 1);
      rhs = createBinaryOperator(Ctx, rhs, rhs, clang::BO_NE,
                                 TB.getType(Result, nullptr, -1),
                                 clang::VK_PRValue);
      cmp = createBinaryOperator(
          Ctx, lhs, rhs,
          OpCode == llvm::CmpInst::FCMP_ORD ? clang::BO_LAnd : clang::BO_LOr,
          TB.getType(Result, nullptr, -1), clang::VK_PRValue);
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
  rhs = EB.visitValue(Op1, Result, 1);
  if (cv == Signed) {
    lhs = castSigned(Ctx, TB, lhs);
  } else if (cv == Unsigned) {
    lhs = castUnsigned(Ctx, TB, lhs);
  }
  // ensure both pointer type or integer type, for eq and ne.
  if (op == clang::BO_EQ || op == clang::BO_NE) {
    if (lhs->getType()->isAnyPointerType() &&
        !rhs->getType()->isAnyPointerType()) {
      rhs = TB.checkCast(rhs, lhs->getType());
    }
    if (!lhs->getType()->isAnyPointerType() &&
        rhs->getType()->isAnyPointerType()) {
      lhs = TB.checkCast(lhs, rhs->getType());
    }
  }
  cmp = createBinaryOperator(
      Ctx, lhs, rhs, *op, TB.getType(Result, nullptr, -1), clang::VK_PRValue);
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
    llvm_unreachable("CFGBuilder.convertOp: FCMP_ORD or FCMP_UNO should "
                     "already be handled");
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
  if (auto Target = I.getCalledFunction()) {
    if (Target->getName().startswith("llvm.dbg") ||
        Target->getName().startswith("llvm.lifetime")) {
      return;
    }
  }
  // See also:
  // https://github.com/llvm/llvm-project/blob/d8e5a0c42bd8796cce9caa53aacab88c7cb2a3eb/clang/lib/Analysis/BodyFarm.cpp#L245
  llvm::SmallVector<clang::Expr *, 16> Args(I.arg_size());
  for (unsigned i = 0; i < I.arg_size(); i++) {
    Args[i] = EB.visitValue(I.getArgOperand(i), &I, i);
    assert(Args[i] != nullptr && "CFGBuilder.visitCallInst: Args[i] is null?");
  }
  llvm::Function *Callee = I.getCalledFunction();
  clang::QualType Ret;
  clang::Expr *FRef;
  clang::QualType FunctionType;
  if (Callee != nullptr) {
    auto FD = FCtx.getSAContext().getFunctionDecl(*Callee);
    FunctionType = FD->getType();
    clang::QualType Ty = FD->getType();
    FRef = makeDeclRefExpr(FD);
    if (Ty->isLValueReferenceType() && FRef->getType()->isFunctionType()) {
      Ty = Ctx.getPointerType(Ty.getNonReferenceType());
      FRef = makeImplicitCast(FRef, Ty, clang::CK_FunctionToPointerDecay);
    }
    Ret = FD->getReturnType();

    if (Callee->isIntrinsic()) {
      if (FD->param_size() < Args.size()) {
        Args.resize(FD->param_size());
      }
    }

    // Handle argument casts
    auto *FT = llvm::cast<clang::FunctionProtoType>(FunctionType);
    auto Params = FT->getParamTypes();
    for (unsigned i = 0; i < Params.size(); i++) {
      auto ParamTy = Params[i];
      Args[i] = FCtx.getTypeBuilder().checkCast(Args[i], ParamTy);
    }
  } else {
    // Function pointer call.
    // TODO: What is the return type here?
    auto CalleeExpr = EB.visitValue(I.getCalledOperand(), &I, I.arg_size());
    auto RetTy = getTypeBuilder().getType(&I, nullptr, -1);
    llvm::SmallVector<clang::QualType, 16> ArgTypes;
    for (auto Arg : Args) {
      ArgTypes.push_back(Arg->getType());
    }
    FunctionType =
        Ctx.getFunctionType(RetTy, ArgTypes, FunctionProtoType::ExtProtoInfo());

    // FunctionType = CalleeExpr->getType();
    assert(CalleeExpr != nullptr &&
           "CFGBuilder.visitCallInst: CalleeExpr is null?");
    auto Ty = CalleeExpr->getType();
    if (Ty->isPointerType()) {
      Ty = Ty->getPointeeType();
      CalleeExpr = getTypeBuilder().checkCast(CalleeExpr,
                                              Ctx.getPointerType(FunctionType));
    } else {
      CalleeExpr = getTypeBuilder().checkCast(CalleeExpr, FunctionType);
    }
    assert(Ty->isFunctionType() &&
           "CallInst operand is not a function pointer?");
    FRef = CalleeExpr;

    // create func ptr type cast
    Ret = llvm::cast<clang::FunctionType>(Ty)->getReturnType();

    llvm::errs() << "CFGBuilder.visitCallInst: Warning: Func ptr call does no "
                    "arg casting.\n";
  }

  FRef = addParenthesis<clang::CallExpr>(Ctx, FRef, false);
  // TODO? CallExpr type is function return type or not?
  auto Call = clang::CallExpr::Create(Ctx, FRef, Args, Ret, clang::VK_PRValue,
                                      clang::SourceLocation(),
                                      clang::FPOptionsOverride());

  addExprOrStmt(I, *Call);
}

clang::Expr *TypeBuilder::checkCast(clang::Expr *Val, clang::QualType To) {
  // remove any cast expr
  while (auto Cast = llvm::dyn_cast<clang::CastExpr>(Val)) {
    Val = Cast->getSubExpr();
  }

  if (isTypeCompatible(Val->getType(), To)) {
    return Val;
  }
  if (auto R = CT->gepCast(Val, To)) {
    return R;
  }
  // TODO should we use CK_Bitcast here?
  return createCStyleCastExpr(Ctx, To, clang::VK_PRValue, clang::CK_BitCast,
                              Val);
}

ValueNamer &SAFuncContext::getValueNamer() {
  return getSAContext().getValueNamer();
}

clang::Expr *SAFuncContext::cacheExpr(llvm::Instruction &Inst,
                                      clang::Expr *Expr, CFGBlock &block,
                                      QualType Ty, std::optional<size_t> Slot) {
  // Create a local variable for it, in order to be faithful to the IR.
  auto Name = getValueNamer().getTempName(Inst);

  llvm::SmallString<128> Buf;
  clang::IdentifierInfo *II2 = getIdentifierInfo(Name);
  if (Ty.isNull()) {
    Ty = toLValueType(getASTContext(), Expr->getType());
  }
  if (Ty->canDecayToPointerType()) {
    Ty = Ctx.getASTContext().getDecayedType(Ty);
  }
  clang::VarDecl *Decl = clang::VarDecl::Create(
      getASTContext(), FD, clang::SourceLocation(), clang::SourceLocation(),
      II2, Ty, nullptr, clang::SC_None);

  clang::DeclStmt *DS = new (getASTContext())
      clang::DeclStmt(clang::DeclGroupRef(Decl), clang::SourceLocation(),
                      clang::SourceLocation());
  VarDecls.push_back(DS);
  clang::Expr *assign = createBinaryOperator(
      getASTContext(), makeDeclRefExpr(Decl), TB.checkCast(Expr, Ty),
      clang::BO_Assign, Decl->getType(), clang::VK_LValue);

  addStmt(block, *assign, Inst, Slot);
  return makeDeclRefExpr(Decl);
}

void SAFuncContext::addStmt(CFGBlock &Block, clang::Stmt &Stmt,
                            llvm::Instruction &InsertLoc,
                            std::optional<size_t> Slot) {
  LoadCloneRewriter R(getASTContext(), LoadInfoMap, InsertLoc);
  auto NewStmt = R.TransformStmt(&Stmt);
  assert(NewStmt.get() != nullptr);

  if (Slot) {
    assert(isa<llvm::LoadInst>(&InsertLoc));
    Block.updateStmt(*Slot, NewStmt.get());
  } else {
    Block.appendStmt(NewStmt.get());
  }
}

void SAFuncContext::setTerminator(CFGBlock &block, CFGTerminator Term,
                                  llvm::Instruction &InsertLoc) {
  LoadCloneRewriter R(getASTContext(), LoadInfoMap, InsertLoc);
  StmtResult NewCond;
  if (auto *BT = std::get_if<BranchTerminator>(&Term)) {
    NewCond = R.TransformStmt(BT->getStmt());
    assert(NewCond.get() != nullptr);
    block.setTerminator(NewCond.get());
  } else if (auto ST = std::get_if<SwitchTerminator>(&Term)) {
    NewCond = R.TransformStmt(ST->getStmt());
    assert(NewCond.get() != nullptr);
    block.setTerminator(SwitchTerminator(NewCond.get()));
  } else {
    assert(false && "Unreachable");
  }
}

void SAFuncContext::addExprOrStmt(llvm::Value &V, clang::Stmt &Stmt,
                                  CFGBlock &Block, QualType Ty) {
  // non-instruction or expr-like instruction.
  if (!llvm::isa<llvm::Instruction>(&V) || llvm::isa<llvm::Argument>(&V)) {
    ExprMap[&V] = &llvm::cast<clang::Expr>(Stmt);
    return;
  }
  auto &I = llvm::cast<llvm::Instruction>(V);
  if (V.getNumUses() == 0) {
    // Treat as stmt
    addStmt(Block, Stmt, I);
    return;
  }
  assert(llvm::isa<clang::Expr>(&Stmt) &&
         "SAFuncContext.addExprOrStmt: Instruction has uses but is not Expr?");
  auto &Expr = llvm::cast<clang::Expr>(Stmt);
  auto &Inst = *llvm::cast<llvm::Instruction>(&V);

  bool canCache = false;

  using namespace llvm;
  if (auto LI = dyn_cast<LoadInst>(&Inst)) {
    canCache = false;

    // auto &MSSAR = FAM.getResult<llvm::MemorySSAAnalysis>(Func);
    // auto &MSSA = MSSAR.getMSSA();
    // auto Walker = MSSA.getWalker();
    // MemoryUse *LoadMU = cast<MemoryUse>(MSSA.getMemoryAccess(LI));

    // bool allUserGood = true;
    // for (User *U : LI->users()) {
    //   if (Instruction *UsrI = dyn_cast<Instruction>(U)) {
    //     // 只关心再次访问内存的指令（比如另一个 load/store/atomicrmw/...）
    //     MemoryAccess *UserMA = MSSA.getMemoryAccess(UsrI);
    //     if (!UserMA)
    //       continue;

    //     // 确定 UserMA 也是一个 MemoryUse（读）或 MemoryDef（写）
    //     // 对于“读后读”一致性检查，我们比较它们背后的“最近写入”是不是同一个。
    //     if (auto *UserMU = dyn_cast<MemoryUse>(UserMA)) {
    //       MemoryAccess *DefAtLoad =
    //       Walker->getClobberingMemoryAccess(LoadMU); MemoryAccess *DefAtUser
    //       = Walker->getClobberingMemoryAccess(UserMU);

    //       if (DefAtLoad == DefAtUser) {
    //         // ——在 LI 和 UsrI 之间，没有其他 Store 改写这块内存——
    //         // LI 加载的值和 UsrI 处读取的值是相同的
    //       } else {
    //         // 之间被某个 Store（或 MemoryDef）修改过
    //         allUserGood = false;
    //       }
    //     }
    //   }
    // }

    // if (allUserGood) {
    //   canCache = true;
    // }

  } else if (onlyUsedInCurrentBlock(Inst) || isAddrOf(&Expr)) {
    canCache = true;
  }

  if (canCache) {
    ExprMap[&V] = &Expr; // wait to be folded
  } else {
    auto Cache = cacheExpr(Inst, &Expr, Block, Ty);
    ExprMap[&V] = Cache;
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
    return createCStyleCastExpr(Ctx, ty, clang::VK_PRValue,
                                clang::CK_IntegralCast, E);
  }
  return E;
}

clang::Expr *castUnsigned(clang::ASTContext &Ctx, TypeBuilder &TB,
                          clang::Expr *E) {
  if (E->getType()->isSignedIntegerType()) {
    auto ty = TB.makeUnsigned(Ctx, E->getType());
    return createCStyleCastExpr(Ctx, ty, clang::VK_PRValue,
                                clang::CK_IntegralCast, E);
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
  clang::Expr *expr =
      createUnaryOperator(Ctx, EB.visitValue(I.getOperand(0), &I, 0), op,
                          getType(&I, nullptr, -1), clang::VK_PRValue);
  addExprOrStmt(I, *expr);
}

clang::Expr *handleLLVMCast(clang::ASTContext &Ctx, llvm::LLVMContext &LCtx,
                            ExprBuilder &EB, TypeBuilder &TB,
                            llvm::Instruction::CastOps OpCode,
                            llvm::Type *srcTy, llvm::Type *destTy,
                            llvm::Value *Operand, llvm::User *User,
                            long OpInd) {
  auto Val = EB.visitValue(Operand, User, OpInd);
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

void CFGBuilder::visitCastInst(llvm::CastInst &I) {
  // Ignore reg2mem alloca point inserted by reg2mem.
  if (I.getNumUses() == 0 && I.getName().startswith("reg2mem alloca point")) {
    return;
  }
  auto *Val = EB.visitValue(I.getOperand(0), &I, 0);
  auto destTy = getType(&I, nullptr, -1);
  // if there is HighTypes, ignore low level casts.
  if (FCtx.getSAContext().hasHighTypes() && canIgnore(I.getOpcode())) {
    // no need to cast
    FCtx.addMapping(&I, *Val);
    return;
  } else {
    auto expr = createCCast(Ctx, EB, FCtx.getTypeBuilder(), I.getOpcode(), &I,
                            destTy, Val);
    addExprOrStmt(I, *expr);
  }
}

clang::Expr *handleBinary(clang::ASTContext &Ctx, ExprBuilder &EB,
                          TypeBuilder &TB, llvm::Instruction::BinaryOps OpCode,
                          llvm::User &Result, llvm::Value *L, llvm::Value *R,
                          std::map<clang::Decl *, StructInfo> *StructInfos) {
  clang::Optional<clang::BinaryOperatorKind> op = convertOp(OpCode);
  assert(op.hasValue() && "CFGBuilder.visitBinaryOperator: unexpected op type");
  Conversion cv = getSignedness(OpCode);
  // insert conversion if needed
  clang::Expr *lhs = EB.visitValue(L, &Result, 0);
  clang::Expr *rhs = EB.visitValue(R, &Result, 1);

  // If it is add or sub with ptr, handle it like a gep
  if (OpCode == llvm::Instruction::Add || OpCode == llvm::Instruction::Sub) {
    // Check if it is ptr/array + number or ptr/array - number
    if (OpCode == llvm::Instruction::Add && lhs->getType()->isIntegerType() &&
        (rhs->getType()->isPointerType() || rhs->getType()->isArrayType())) {
      std::swap(L, R);
      std::swap(lhs, rhs);
    }
    if ((lhs->getType()->isPointerType() || lhs->getType()->isArrayType()) &&
        rhs->getType()->isIntegerType()) {
      return TB.ptrAdd(lhs, rhs);
    }
  }

  if (cv == Signed || cv == Arithmetic) {
    lhs = castSigned(Ctx, TB, lhs);
  } else if (cv == Unsigned || cv == Logical) {
    lhs = castUnsigned(Ctx, TB, lhs);
  }
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

  auto ExpectedTy = TB.getType(&Result, nullptr, -1);
  // for normal arithmetic addition, use lhs's type.
  clang::Expr *binop = createBinaryOperator(Ctx, lhs, rhs, op.getValue(),
                                            lhs->getType(), clang::VK_PRValue);
  binop = TB.checkCast(binop, ExpectedTy);
  return binop;
}

void CFGBuilder::visitBinaryOperator(llvm::BinaryOperator &I) {
  auto binop =
      handleBinary(Ctx, EB, FCtx.getTypeBuilder(), I.getOpcode(), I,
                   I.getOperand(0), I.getOperand(1),
                   nullptr // &FCtx.getSAContext().getHighTypes().StructInfos
      );
  assert(binop != nullptr && "CFGBuilder.visitBinaryOperator: binop is null?");
  addExprOrStmt(I, *binop);
  return;
}

void CFGBuilder::visitReturnInst(llvm::ReturnInst &I) {
  clang::Stmt *ret;
  clang::Expr *retVal = nullptr;
  if (I.getReturnValue() != nullptr) {
    retVal = EB.visitValue(I.getReturnValue(), &I, 0);
  }
  ret =
      clang::ReturnStmt::Create(Ctx, clang::SourceLocation(), retVal, nullptr);
  addExprOrStmt(I, *ret);
  // not add to terminator!
  // Blk->setTerminator(CFGTerminator(ret));
}

/// Convert SelectInst to Ternary operator `?:`.
/// If the usage matches the and/or logical operator, then convert to && or ||
void CFGBuilder::visitSelectInst(llvm::SelectInst &I) {
  auto &TB = FCtx.getTypeBuilder();
  auto RTy = TB.getTypeL(&I, nullptr, -1);
  clang::Expr *cond = EB.visitValue(I.getCondition(), &I, 0);
  if (I.getType()->isIntegerTy(1)) {
    // select i1 expr1, i1 true, i1 expr2 -> expr1 || expr2
    if (auto i = llvm::dyn_cast<llvm::ConstantInt>(I.getTrueValue())) {
      if (i->isOne()) {
        clang::Expr *fl = EB.visitValue(I.getFalseValue(), &I, 2);
        auto exp = createBinaryOperator(Ctx, cond, fl, clang::BO_LOr, RTy,
                                        clang::VK_PRValue);
        addExprOrStmt(I, *exp);
        return;
      }
    }
    // select i1 expr1, i1 expr2, i1 false -> expr1 && expr2
    if (auto i = llvm::dyn_cast<llvm::ConstantInt>(I.getFalseValue())) {
      if (i->isZero()) {
        clang::Expr *tr = EB.visitValue(I.getTrueValue(), &I, 1);
        auto exp = createBinaryOperator(Ctx, cond, tr, clang::BO_LAnd, RTy,
                                        clang::VK_PRValue);
        addExprOrStmt(I, *exp);
        return;
      }
    }
  }
  // TODO: find a common type, ensure same type.
  clang::Expr *tr = EB.visitValue(I.getTrueValue(), &I, 1, RTy);
  clang::Expr *fl = EB.visitValue(I.getFalseValue(), &I, 2, RTy);
  auto exp =
      createConditionalOperator(Ctx, cond, tr, fl, RTy, clang::VK_PRValue);
  addExprOrStmt(I, *exp);
}

void CFGBuilder::visitSwitchInst(llvm::SwitchInst &I) {
  assert(I.getNumSuccessors() > 2 &&
         "CFGBuilder.visitSwitchInst: SwitchInst with less than 2 successors?");
  FCtx.setTerminator(
      *Blk, SwitchTerminator(EB.visitValue(I.getCondition(), &I, 0)), I);
  // Add case expressions
  auto cases = std::get_if<SwitchTerminator>(&Blk->getTerminator());
  long ind = 0;
  for (auto &expr : I.cases()) {
    cases->cases().push_back(EB.visitValue(expr.getCaseValue(), &I, ind));
    ind++;
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
  return Ctx.getASTContext();
}

void demoteSSAFixHT(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                    HTypeResult &HT, const char *DebugDir) {
  std::map<std::pair<llvm::Function *, std::string>,
           std::pair<HType *, HType *>>
      NameMap;
  auto PointerSize = M.getDataLayout().getPointerSize();
  // first find all phi and its address. move the type to name map.
  for (llvm::Function &F : M) {
    for (llvm::BasicBlock &BB : F) {
      for (llvm::Instruction &I : BB) {
        if (llvm::isa<llvm::PHINode>(&I)) {
          llvm::PHINode &PN = llvm::cast<llvm::PHINode>(I);
          HType *T = nullptr;
          HType *TU = nullptr;
          if (HT.ValueTypes.count(&PN)) {
            T = HT.ValueTypes.at(&PN);
            if (T != nullptr) {
              T = HT.HTCtx->getPointerType(false, PointerSize, T);
            }
          }
          HT.ValueTypes.erase(&PN);
          if (HT.ValueTypesLowerBound.count(&PN)) {
            TU = HT.ValueTypesLowerBound.at(&PN);
            if (TU != nullptr) {
              TU = HT.HTCtx->getPointerType(false, PointerSize, TU);
            }
            HT.ValueTypesLowerBound.erase(&PN);
          }
          auto it =
              NameMap.insert({{&F, PN.getName().str()}, std::make_pair(T, TU)});
          assert(it.second && "decompileModule: duplicate phi name?");
        }
      }
    }
  }

  if (DebugDir) {
    // demote SSA using reg2mem
    printModule(M, join(DebugDir, "llvm2c-before-demotessa.ll").c_str());
  }
  notdec::llvm2c::demoteSSA(M, MAM);
  if (DebugDir) {
    // demote SSA using reg2mem
    printModule(M, join(DebugDir, "llvm2c-after-demotessa.ll").c_str());
  }

  // find all phi alloca and phi reload, set type as previous.
  for (llvm::Function &F : M) {
    for (llvm::BasicBlock &BB : F) {
      for (llvm::Instruction &I : BB) {
        if (llvm::isa<llvm::AllocaInst>(&I)) {
          llvm::AllocaInst &AI = llvm::cast<llvm::AllocaInst>(I);
          if (HT.ValueTypes.count(&AI) == 0) {
            auto N1 = AI.getName().str();
            assert(N1.rfind(".reg2mem", N1.length() - 8) == (N1.length() - 8) &&
                   "decompileModule: alloca not from reg2mem?");
            auto N = N1.substr(0, N1.length() - 8);
            AI.setName(N);
            auto Ts = NameMap.at({&F, N});
            if (Ts.first != nullptr) {
              auto I1 = HT.ValueTypes.insert({&AI, Ts.first});
              assert(I1.second && "decompileModule: duplicate alloca name?");
            }
            if (Ts.second != nullptr) {
              auto I2 = HT.ValueTypesLowerBound.insert({&AI, Ts.second});
              assert(I2.second && "decompileModule: duplicate alloca name?");
            }
          }
        }
      }
    }
  }
}

/// Decompile the module to c and print to a file.
void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_fd_ostream &OS, Options opts,
                     std::unique_ptr<HTypeResult> HT) {

  auto DebugDir = std::getenv("NOTDEC_DEBUG_DIR");
  if (DebugDir) {
    llvm::sys::fs::create_directories(DebugDir);
  }

  if (!opts.noDemoteSSA) {
    if (HT) {
      demoteSSAFixHT(M, MAM, *HT, DebugDir);
    } else {
      demoteSSA(M, MAM);
    }
  }
  LLVM_DEBUG(
      llvm::dbgs() << "\n========= IR before structural analysis =========\n");
  LLVM_DEBUG(llvm::dbgs() << M);
  LLVM_DEBUG(llvm::dbgs()
             << "\n========= End IR before structural analysis =========\n");

  // convert to shared ptr for easier usage
  std::shared_ptr<HTypeResult> HTR = (HT == nullptr ? nullptr : std::move(HT));
  std::shared_ptr<ASTUnit> AST = buildAST(M.getName());
  std::shared_ptr<ASTManager> AM = std::make_shared<ASTManager>(OS, AST);
  std::shared_ptr<ClangTypeResult> CT =
      HTR == nullptr ? nullptr : std::make_shared<ClangTypeResult>(HTR, AM);

  if (CT != nullptr) {
    CT->declareDecls();
  }
  if (CT != nullptr) {
    CT->defineDecls();
  }

  SAContext Ctx(const_cast<llvm::Module &>(M), MAM, AM, opts, CT);
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

  // Print the AST
  DeclPrinter DP(OS, Ctx.getASTContext().getPrintingPolicy(),
                 Ctx.getASTContext(), 0, MyPrintingPolicy(), CT);
  AM->print(DP);
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

bool hasOneUseIgnoreCast(llvm::Value &Val) {
  if (Val.hasOneUse()) {
    if (auto Cast = llvm::dyn_cast<llvm::CastInst>(*Val.user_begin())) {
      return Cast->hasOneUse();
    } else {
      return true;
    }
  }
  return false;
}

// Has one use and is in the same block
bool onlyUsedInCurrentBlock(llvm::Instruction &inst) {
  llvm::BasicBlock *BB = inst.getParent();
  if (hasOneUseIgnoreCast(inst)) {
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

clang::FunctionDecl *
SAContext::createFunctionDecl(TranslationUnitDecl *TUD, const char *Name,
                              ArrayRef<const char *> ParamNames,
                              ArrayRef<QualType> ParamTys, QualType RetTy,
                              bool isVariadic) {
  auto ParamSize = ParamNames.size();
  assert(ParamSize == ParamTys.size());
  clang::FunctionDecl *FD = nullptr;
  // create function type
  auto FTy = getASTContext().getFunctionType(
      RetTy, ParamTys, clang::FunctionProtoType::ExtProtoInfo());

  // create function decl
  clang::IdentifierInfo *II = getIdentifierInfo(Name);
  clang::FunctionProtoType::ExtProtoInfo EPI;
  EPI.Variadic = isVariadic;
  FD = clang::FunctionDecl::Create(
      getASTContext(), TUD, clang::SourceLocation(), clang::SourceLocation(),
      II, FTy, nullptr, clang::SC_Extern);

  // create function parameters
  llvm::SmallVector<clang::ParmVarDecl *, 4> Params;
  for (int i = 0; i < ParamSize; i++) {
    clang::IdentifierInfo *ArgII = getIdentifierInfo(ParamNames[i]);
    clang::ParmVarDecl *PVD = clang::ParmVarDecl::Create(
        getASTContext(), FD, clang::SourceLocation(), clang::SourceLocation(),
        ArgII, ParamTys[i], nullptr, clang::SC_None, nullptr);
    Params.push_back(PVD);
  }
  FD->setParams(Params);
  return FD;
}

clang::FunctionDecl *SAContext::getIntrinsic(std::string FName) {
  auto *TUD = AM->getFunctionDeclarations();
  clang::FunctionDecl *FD = nullptr;
  if (FName == "alloca") {
    if (auto F1 = AM->getFuncDeclaration("alloca")) {
      return F1;
    }
    // create a declaration
    // void * alloca ( size_t size );
    constexpr int ParamSize = 1;
    const char *Names[ParamSize] = {"size"};
    QualType ParamTys[ParamSize] = {getASTContext().UnsignedLongTy};
    QualType RetTy = getASTContext().VoidPtrTy;

    FD = createFunctionDecl(TUD, "alloca", Names, ParamTys, RetTy);
    TUD->addDecl(FD);
  } else {
    assert(false && "unhandled intrinsic.");
  }
  return FD;
}

clang::FunctionDecl *SAContext::getIntrinsic(llvm::Function &F) {
  auto *TUD = AM->getFunctionDeclarations();
  clang::FunctionDecl *FD = nullptr;
  if (F.getIntrinsicID() == llvm::Intrinsic::memset) {
    if (auto F1 = AM->getFuncDeclaration("memset")) {
      return F1;
    }
    // create a declaration
    // void * memset ( void * ptr, int value, size_t num );
    // const int ParamSize = 3;
    const char *Names[] = {"ptr", "value", "num"};
    QualType ParamTys[3] = {getASTContext().VoidPtrTy, getASTContext().IntTy,
                            getASTContext().UnsignedLongTy};
    QualType RetTy = getASTContext().VoidPtrTy;

    FD = createFunctionDecl(TUD, "memset", Names, ParamTys, RetTy);
    TUD->addDecl(FD);
  } else if (F.getIntrinsicID() == llvm::Intrinsic::memcpy) {
    // call void @llvm.memcpy.p0i8.p0i8.i64(i8* %6, i8* inttoptr (i32 1136 to
    // i8*), i64 44, i1 false)

    if (auto F1 = AM->getFuncDeclaration("memcpy")) {
      return F1;
    }
    // void *memcpy(void *destination, const void *source, size_t num);
    const char *Names[] = {"destination", "source", "num"};
    QualType ParamTys[3] = {getASTContext().VoidPtrTy,
                            getASTContext().VoidPtrTy.withConst(),
                            getASTContext().UnsignedLongTy};
    QualType RetTy = getASTContext().VoidPtrTy;

    FD = createFunctionDecl(TUD, "memcpy", Names, ParamTys, RetTy);
    TUD->addDecl(FD);
  } else if (F.getIntrinsicID() == llvm::Intrinsic::fabs) {
    if (auto F1 = AM->getFuncDeclaration("fabs")) {
      return F1;
    }
    const char *Names[] = {"x"};
    QualType RetTy = getTypeBuilder().visitType(*F.getReturnType());
    QualType ParamTys[1] = {RetTy};

    FD = createFunctionDecl(TUD, "fabs", Names, ParamTys, RetTy);
    TUD->addDecl(FD);
  } else if (F.getIntrinsicID() == llvm::Intrinsic::abs) {
    if (auto F1 = AM->getFuncDeclaration("abs")) {
      return F1;
    }
    const char *Names[] = {"x"};
    QualType RetTy = getASTContext().getIntTypeForBitwidth(
        F.getReturnType()->getIntegerBitWidth(), true);
    QualType ParamTys[1] = {RetTy};

    FD = createFunctionDecl(TUD, "abs", Names, ParamTys, RetTy);
    TUD->addDecl(FD);
  } else {
    assert(false && "unhandled intrinsic.");
  }
  return FD;
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

  auto *TUD = AM->getFunctionDeclarations();
  // create function decls
  for (llvm::Function &F : M) {
    if (F.isIntrinsic()) {
      continue;
    }
    getValueNamer().clearFuncCount();
    llvm::errs() << "SAContext: Declare Function: " << F.getName() << "\n";

    // create function decl
    clang::IdentifierInfo *II =
        getIdentifierInfo(getValueNamer().getFuncName(F));
    clang::FunctionProtoType::ExtProtoInfo EPI;
    EPI.Variadic = F.isVarArg();
    clang::FunctionDecl *FD = clang::FunctionDecl::Create(
        getASTContext(), TUD, clang::SourceLocation(), clang::SourceLocation(),
        II, TB.getFunctionType(F, EPI), nullptr, getStorageClass(F));

    // create function parameters
    llvm::SmallVector<clang::ParmVarDecl *, 8> Params;
    for (llvm::Argument &Arg : F.args()) {
      clang::IdentifierInfo *ArgII =
          getIdentifierInfo(getValueNamer().getArgName(Arg));
      clang::ParmVarDecl *PVD = clang::ParmVarDecl::Create(
          getASTContext(), FD, clang::SourceLocation(), clang::SourceLocation(),
          ArgII, TB.getTypeL(&Arg, &F, -1), nullptr, clang::SC_None, nullptr);
      Params.push_back(PVD);
    }
    FD->setParams(Params);
    TUD->addDecl(FD);

    globalDecls.insert(std::make_pair(&F, FD));
  }

  // handle intrinsic functions
  for (llvm::Function &F : M) {
    if (F.getName().startswith("llvm.dbg") ||
        F.getName().startswith("llvm.lifetime")) {
      continue;
    }
    if (!F.isIntrinsic()) {
      continue;
    }
    clang::FunctionDecl *FD = getIntrinsic(F);

    globalDecls.insert(std::make_pair(&F, FD));
  }

  TUD = AM->getGlobalDefinitions();
  // create global variable decls
  for (llvm::GlobalVariable &GV : M.globals()) {
    if (CT) {
      if (isSPByMetadata(&GV)) {
        if (GV.getNumUses() == 0) {
          continue;
        }
      }
    }
    clang::IdentifierInfo *II =
        getIdentifierInfo(getValueNamer().getGlobName(GV));
    QualType PTy;

    if (GV.getName().startswith("table_")) {
      PTy = TB.visitType(*GV.getType());
    } else {
      PTy = TB.getType(&GV, nullptr, -1)->getPointeeType();
    }

    if (GV.isConstant()) {
      PTy = PTy.withConst();
    }
    clang::VarDecl *VD = clang::VarDecl::Create(
        getASTContext(), TUD, clang::SourceLocation(), clang::SourceLocation(),
        II, PTy->getPointeeType().isNull() ? PTy : PTy->getPointeeType(),
        nullptr, getStorageClass(GV));

    TUD->addDecl(VD);
    globalDecls.insert(std::make_pair(&GV, VD));
  }

  // create global variable initializers
  // defer because initializer may refer to the address of globals that have not
  // created.
  for (llvm::GlobalVariable &GV : M.globals()) {
    if (!globalDecls.count(&GV)) {
      continue;
    }
    auto VD = getGlobalVarDecl(GV);
    if (GV.hasInitializer()) {
      VD->setInit(
          EB.visitInitializer(GV.getInitializer(), &GV, 0, VD->getType()));
    }
  }
}

void SAFuncContext::run() {
  // will not run over declaration
  assert(!Func.isDeclaration());
  Ctx.getValueNamer().clearFuncCount();
  auto PrevFD = Ctx.getFunctionDecl(Func);
  assert(PrevFD != nullptr && "SAFuncContext::run: FunctionDecl is null, not "
                              "created by `SAContext::createDecls`?");
  // 1. build the CFGBlocks
  CFGBuilder Builder(*this, VarDecls);
  auto *TUD = Ctx.getASTManager().getFunctionDefinitions();

  // create function decl again, and set the previous declaration.
  clang::IdentifierInfo *II =
      Ctx.getIdentifierInfo(getSAContext().getValueNamer().getFuncName(Func));
  clang::FunctionProtoType::ExtProtoInfo EPI;
  EPI.Variadic = Func.isVarArg();
  FD = clang::FunctionDecl::Create(
      getASTContext(), TUD, clang::SourceLocation(), clang::SourceLocation(),
      II, PrevFD->getType(), nullptr, SAContext::getStorageClass(Func));
  FD->setPreviousDeclaration(PrevFD);

  // We do not need to create ParamDecl because we handle it in the entry
  // block.

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
  std::vector<clang::Stmt *> Stmts = std::move(VarDecls);

  assert(&Cfg->front() == &Cfg->getEntry());
  for (auto BB : *Cfg) {
    // add labelStmt
    IStructuralAnalysis::addAllStmtTo(BB, Stmts, true);
  }

  // create a compound stmt as function body
  auto CS = clang::CompoundStmt::Create(
      getASTContext(), Stmts, clang::SourceLocation(), clang::SourceLocation());
  FD->setBody(CS);
  TUD->addDecl(FD);
}

clang::Expr *ExprBuilder::createCompoundLiteralExpr(llvm::Value *Val,
                                                    llvm::User *User,
                                                    long OpInd) {
  auto ObjTy = TB.getType(Val, User, OpInd);
  clang::Expr *ret = new (Ctx) clang::CompoundLiteralExpr(
      clang::SourceLocation(), Ctx.getTrivialTypeSourceInfo(ObjTy), ObjTy,
      clang::VK_LValue, visitValue(Val, User, OpInd), false);
  ret = clang::ImplicitCastExpr::Create(Ctx, ObjTy, clang::CK_LValueToRValue,
                                        ret, nullptr, clang::VK_PRValue,
                                        clang::FPOptionsOverride());
  return ret;
}

clang::Expr *ExprBuilder::visitValue(llvm::Value *Val, llvm::User *User,
                                     long OpInd, clang::QualType Ty) {
  // if (Ty.isNull()) {
  //   Ty = TB.getType(Val, User, OpInd);
  // }

  clang::Expr *Ret = nullptr;

  // Check for ExprMap
  if (FCtx != nullptr && FCtx->isExpr(Val)) {
    Ret = FCtx->getExpr(Val);
  } else {
    // if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (Val == nullptr) {
      return nullptr;
    }
    if (llvm::Instruction *Inst =
            llvm::dyn_cast_or_null<llvm::Instruction>(Val)) {
      Ret = visit(*Inst);
    } else if (llvm::Constant *C =
                   llvm::dyn_cast_or_null<llvm::Constant>(Val)) {
      Ret = visitConstant(*C, User, OpInd, Ty);
    } else {
      llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                   << "UnImplemented: ExprBuilder.visitValue cannot handle: "
                   << *Val << "\n";
      std::abort();
    }
  }

  // Add cast if needed
  if (!Ty.isNull() && !Ret->getType().isNull()) {
    Ret = TB.checkCast(Ret, Ty);
  }
  return Ret;
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
      Args[i] = getTypeL(ActualFunc->getArg(i), nullptr, -1);
    } else {
      Args[i] = visitType(*Ty.getParamType(i));
    }
  }
  clang::QualType RetTy;
  bool InHighType = false;
  if (ActualFunc != nullptr && !ActualFunc->getReturnType()->isVoidTy()) {
    auto RetV = ReturnValue{.Func = ActualFunc};
    if (CT != nullptr && CT->hasType(RetV)) {
      InHighType = true;
      RetTy = getTypeL(RetV, nullptr, -1);
    }
  }

  if (!InHighType) {
    // if (ActualFunc != nullptr) {
    //   llvm::errs() << "Function: " << ActualFunc->getName()
    //                << " has no return value type in HighTypes"
    //                << "\n";
    // }
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

  auto TUD = CT->getASTManager()->getGlobalDefinitions();
  clang::RecordDecl *prev = nullptr;
  clang::RecordDecl *decl = clang::RecordDecl::Create(
      Ctx, clang::TagDecl::TagKind::TTK_Struct, TUD, clang::SourceLocation(),
      clang::SourceLocation(), II, prev);
  // Set free standing so that unnamed struct can be combined:
  // (`struct {int x;} a,b`)
  // This also requires that the var decl uses a ElaboratedType whose owned
  // tag decl is the previous RecordDecl See also
  // https://lists.llvm.org/pipermail/cfe-dev/2021-February/067631.html
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
  TUD->addDecl(decl);
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
    // https://lists.llvm.org/pipermail/cfe-dev/2021-February/067631.html
    // create a non-freestanding RecordDecl in place.
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

clang::QualType TypeBuilder::getType(ExtValuePtr Val, llvm::User *User,
                                     long OpInd) {
  clang::QualType Ret;
  llvmValue2ExtVal(Val, User, OpInd);
  auto *V = std::get_if<llvm::Value *>(&Val);

  // Override main func sig
  if (V != nullptr) {
    if (auto Arg = llvm::dyn_cast<llvm::Argument>(*V)) {
      if (Arg->getParent()->getName() == "main") {
        if (Arg->getArgNo() == 0) {
          return Ctx.IntTy;
        } else if (Arg->getArgNo() == 1) {
          return Ctx.getPointerType(Ctx.getPointerType(Ctx.CharTy));
        }
      }
    }
  }

  llvm::Function *F;
  if (CT != nullptr && CT->hasType(Val)) {
    Ret = CT->getType(Val);
    assert(!Ret.isNull() && "TypeBuilder.getType: Ret is null?");
  } else if ((V != nullptr) &&
             (F = llvm::dyn_cast_or_null<llvm::Function>(*V))) {
    llvm::errs()
        << "Warning: getType for function (call getFunctionType instead!): "
        << F->getName() << "\n";
    Ret = getFunctionType(*F, clang::FunctionProtoType::ExtProtoInfo());
    Ret = Ctx.getPointerType(Ret);
  } else {
    Ret = visitType(*notdec::getType(Val));
    // TODO if int type, and if value is negative constant, we use signed type.
  }

  assert(!Ret.isNull() && "TypeBuilder.getType: Ret is null?");
  return Ret;
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
  } else if (Ty.isIntegerTy(8)) {
    return Ctx.CharTy;
  }

  if (Ty.isIntegerTy()) {
    // default to signed
    // TODO create unk32
    auto ret = Ctx.getIntTypeForBitwidth(Ty.getIntegerBitWidth(), true);
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
clang::Expr *ExprBuilder::visitInitializer(llvm::Value *Val, llvm::User *User,
                                           long OpInd, clang::QualType Ty) {

  return visitValue(Val, User, OpInd, Ty);
}

clang::Expr *ExprBuilder::getNull(clang::QualType Ty) {
  bool isCpp = false;
  if (isCpp) {
    return new (Ctx) clang::CXXNullPtrLiteralExpr(Ty, clang::SourceLocation());
  } else {
    clang::Expr *Zero = clang::IntegerLiteral::Create(
        Ctx, llvm::APInt(32, 0, false), Ctx.IntTy, clang::SourceLocation());
    if (!Ty.isNull()) {
      Zero = createCStyleCastExpr(Ctx, Ty, clang::VK_PRValue,
                                  clang::CK_NullToPointer, Zero);
    }
    return Zero;
  }
}

clang::Expr *ExprBuilder::visitConstant(llvm::Constant &C, llvm::User *User,
                                        long OpInd, clang::QualType Ty) {
  // Check HighTypes for possible type.
  auto CT = TB.CT;
  if (CT != nullptr) {
    ExtValuePtr Val = &C;
    llvmValue2ExtVal(Val, User, OpInd);
    if (CT->hasType(Val)) {
      Ty = CT->getType(Val);
    } else if (CT->hasType(&C)) {
      Ty = CT->getType(&C);
    }
  }

  if (llvm::UndefValue *UV = llvm::dyn_cast<llvm::UndefValue>(&C)) {
    if (Ty.isNull()) {
      Ty = TB.visitType(*UV->getType());
    }
    return getNull(Ty);
  } else if (llvm::GlobalObject *GO = llvm::dyn_cast<llvm::GlobalObject>(&C)) {
    // global variables and functions
    return addrOf(Ctx, makeDeclRefExpr(SCtx.getGlobalDecl(*GO)));
  } else if (llvm::ConstantAggregate *CA =
                 llvm::dyn_cast<llvm::ConstantAggregate>(&C)) {
    // struct and array
    llvm::SmallVector<clang::Expr *> vec(CA->getNumOperands());
    assert(Ty->isArrayType());
    auto ElemTy = Ty->getArrayElementTypeNoTypeQual();
    for (unsigned i = 0; i < CA->getNumOperands(); i++) {
      // TODO Pass type to operand.
      vec[i] = visitValue(CA->getOperand(i), CA, i, clang::QualType(ElemTy, 0));
    }
    return new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), vec,
                                         clang::SourceLocation());
  } else if (llvm::ConstantDataSequential *CS =
                 llvm::dyn_cast<llvm::ConstantDataSequential>(&C)) {
    // TODO split into multiple C string?
    if (CS->isString()) {
      return clang::StringLiteral::Create(
          Ctx, CS->getAsString(), clang::StringLiteral::Ascii, false,
          Ctx.getStringLiteralArrayType(Ctx.CharTy, CS->getNumElements()),
          clang::SourceLocation());
    }
    // struct and array
    llvm::SmallVector<clang::Expr *> vec(CS->getNumElements());
    for (unsigned i = 0; i < CS->getNumElements(); i++) {
      vec[i] =
          visitValue(CS->getElementAsConstant(i), CS, i, clang::QualType());
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
    // We create ImplicitValueInitExpr for zero initializer but it requires
    // type information, so use visitInitializer instead llvm::errs() <<
    // __FILE__ << ":" << __LINE__ << ": "
    //              << "Error: ExprBuilder.visitConstant cannot handle "
    //                 "ConstantAggregateZero, use visitInitializer
    //                 instead.\n";
  } else if (llvm::ConstantPointerNull *CPN =
                 llvm::dyn_cast<llvm::ConstantPointerNull>(&C)) {
    return getNull(Ty);
  } else if (llvm::ConstantInt *CI = llvm::dyn_cast<llvm::ConstantInt>(&C)) {
    auto Val = CI->getValue();
    if (CI->getType()->getBitWidth() == 1) {
      bool useBool = true;
      if (useBool) {
        return new (Ctx) clang::CXXBoolLiteralExpr(!Val.isZero(), Ctx.BoolTy,
                                                   clang::SourceLocation());
      } else {
        return createCStyleCastExpr(
            Ctx, Ctx.BoolTy, clang::VK_PRValue, clang::CK_BitCast,
            clang::IntegerLiteral::Create(Ctx,
                                          Val.zext(Ctx.getIntWidth(Ctx.IntTy)),
                                          Ctx.IntTy, clang::SourceLocation()));
      }
    }
    if (Val.getBitWidth() == 1) {
      Val = Val.zext(32);
    }

    if (Ty.isNull()) {
      Ty = getType(CI, User, OpInd);
    }

    // if constant is zero, and type is pointer type, the use nullptr
    if (Ty->isPointerType() || Ty->isArrayType()) {
      if (Val.isNullValue()) {
        return getNull(Ty);
      }
      // try to get global variable
      if (auto Ret = TB.CT->getGlobal(Val.getZExtValue())) {
        return Ret;
      } else {
        return createCStyleCastExpr(Ctx, Ty, clang::VK_PRValue,
                                    clang::CK_BitCast,
                                    clang::IntegerLiteral::Create(
                                        Ctx, Val, TB.visitType(*CI->getType()),
                                        clang::SourceLocation()));
      }
    }
    if (!Ty->isIntegerType()) {
      return createCStyleCastExpr(
          Ctx, Ty, clang::VK_PRValue, clang::CK_BitCast,
          clang::IntegerLiteral::Create(Ctx, Val, TB.visitType(*CI->getType()),
                                        clang::SourceLocation()));
    }

    // TODO: eliminate Ui8 Ui16 i8 i16 suffix?
    // https://stackoverflow.com/questions/33659846/microsoft-integer-literal-extensions-where-documented
    // if value is negative, create signed literal then cast.
    if (Val.getBitWidth() > 8 && Val.isNegative() &&
        Ty->isUnsignedIntegerType()) {
      clang::Expr *Ret = clang::IntegerLiteral::Create(
          Ctx, Val, Ctx.getIntTypeForBitwidth(Val.getBitWidth(), true),
          clang::SourceLocation());
      Ret = createCStyleCastExpr(Ctx, Ty, clang::VK_PRValue, clang::CK_BitCast,
                                 Ret);
      return Ret;
    } else {
      return clang::IntegerLiteral::Create(Ctx, Val, Ty,
                                           clang::SourceLocation());
    }

  } else if (llvm::ConstantFP *CFP = llvm::dyn_cast<llvm::ConstantFP>(&C)) {
    return clang::FloatingLiteral::Create(Ctx, CFP->getValueAPF(), true,
                                          getType(CFP, User, OpInd),
                                          clang::SourceLocation());
  } else if (llvm::ConstantExpr *CE = llvm::dyn_cast<llvm::ConstantExpr>(&C)) {
    // https://llvm.org/docs/LangRef.html#constant-expressions
    // handle gep
    if (CE->getOpcode() == llvm::Instruction::GetElementPtr) {
      return handleGEP(Ctx, *this, *llvm::cast<llvm::GEPOperator>(CE));
    }
    // handle casts
    switch (CE->getOpcode()) {
    case llvm::Instruction::IntToPtr:
      return visitValue(CE->getOperand(0), CE, 0, Ty);

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
    case llvm::Instruction::BitCast:
    case llvm::Instruction::AddrSpaceCast:
      return createCCast(
          Ctx, *this, TB, (llvm::Instruction::CastOps)CE->getOpcode(), CE,
          TB.getType(CE, User, OpInd), visitValue(CE->getOperand(0), CE, 0));

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
                          CE->getOperand(0), CE->getOperand(1), nullptr);
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

SAFuncContext::SAFuncContext(SAContext &Ctx, llvm::Function &Func,
                             llvm::FunctionAnalysisManager &FAM)
    : Ctx(Ctx), Func(Func), FAM(FAM), TB(Ctx.getTypeBuilder()) {
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
    if (std::isdigit(Val.getName().front())) {
      OS << "_";
    }
    OS << Val.getName();
    escapeBuf();

    if (isAllUnderline()) {
      // not a useful name
      Buf.clear();
    }
  }
  if (Buf.empty()) {
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

const Options &SAFuncContext::getOpts() const { return Ctx.getOpts(); }

} // namespace notdec::llvm2c
