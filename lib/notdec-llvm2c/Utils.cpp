#include <cassert>
#include <clang/AST/Decl.h>
#include <iostream>
#include <llvm/IR/Verifier.h>
#include <type_traits>
#include <vector>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Casting.h>
#include <llvm/Transforms/Scalar/Reg2Mem.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Local.h>

#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <clang/AST/OperationKinds.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Tooling/Tooling.h>

#include "notdec-llvm2c/Utils.h"

#define DEBUG_TYPE "notdec-backend-utils"

namespace notdec::llvm2c {

const char *KIND_STACK_POINTER = "notdec.stackpointer";

bool isSPByMetadata(llvm::GlobalVariable *GV) {
  if (llvm::MDNode *MD = GV->getMetadata(KIND_STACK_POINTER)) {
    if (MD->getNumOperands() > 0) {
      if (auto *SizeMD = llvm::dyn_cast<llvm::MDString>(MD->getOperand(0))) {
        if (SizeMD->getString() == "true") {
          return true;
        }
      }
    }
  }
  return false;
}

clang::QualType getBoolTy(clang::ASTContext &Ctx) {
  static std::map<clang::ASTContext *, clang::TypedefNameDecl *> BoolDecls;
  if (BoolDecls.count(&Ctx)) {
    return Ctx.getTypedefType(BoolDecls.at(&Ctx));
  }
  auto BoolDecl = clang::TypedefDecl::Create(
      Ctx, Ctx.getTranslationUnitDecl(), clang::SourceLocation(),
      clang::SourceLocation(), &Ctx.Idents.get("bool"),
      Ctx.getTrivialTypeSourceInfo(Ctx.BoolTy));
  BoolDecls.insert({&Ctx, BoolDecl});
  return Ctx.getTypedefType(BoolDecl);
}

unsigned getLLVMTypeSize(llvm::Type *Ty, unsigned PointerSizeInBits) {
  if (Ty->isPointerTy()) {
    assert(PointerSizeInBits != 0);
    return PointerSizeInBits;
  }
  auto Size = Ty->getPrimitiveSizeInBits();
  assert(Size != 0);
  return Size;
}

std::string join(std::string path, std::string elem) {
  return path.back() == '/' ? path + elem : path + "/" + elem;
}

void printModule(llvm::Module &M, const char *path) {
  std::error_code EC;
  llvm::raw_fd_ostream os(path, EC);
  if (EC) {
    std::cerr << "Cannot open output file: " << path << std::endl;
    std::cerr << EC.message() << std::endl;
    std::abort();
  }
  M.print(os, nullptr);
}

std::unique_ptr<clang::ASTUnit> buildAST(llvm::StringRef FileName) {
  auto FileNameStr = FileName.str();
  if (!FileName.endswith(".c") || !FileName.endswith(".cpp") ||
      !FileName.endswith(".cc")) {
    FileNameStr += ".c";
  }
  auto AST = clang::tooling::buildASTFromCodeWithArgs(
      "", {"-target", "wasm32-unknown-wasi", "-fparse-all-comments"},
      FileNameStr, "clang-tool",
      std::make_shared<clang::PCHContainerOperations>());
  assert(AST != nullptr && "Failed to build AST");
  auto Int64Ty = AST->getASTContext().getIntTypeForBitwidth(64, true);
  auto Int64Name = clangObjToString(Int64Ty);
  assert(Int64Name != "long" && "long should not be 64 bit in wasm32");
  return AST;
}

/// Run the RegToMemPass to demote SSA to memory, i.e., eliminate Phi nodes.
void demoteSSA(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  // llvm::errs() << "demoteSSA\n";
  using namespace llvm;

  ModulePassManager MPM;

  MPM.addPass(createModuleToFunctionPassAdaptor(AdjustCFGPass()));
  // MPM.addPass(VerifierPass());
  MPM.addPass(createModuleToFunctionPassAdaptor(RetDupPass()));
  // MPM.addPass(VerifierPass());
  MPM.addPass(createModuleToFunctionPassAdaptor(DemotePhiPass()));
  // MPM.addPass(VerifierPass());
  MPM.addPass(createModuleToFunctionPassAdaptor(AdjustCFGPass()));
  MPM.run(M, MAM);
}

// ===============
// Pass
// ===============

/*
In InstCombinerImpl::mergeStoreIntoSuccessor

*/

llvm::Value *PhiHaveAddress(llvm::PHINode *P) {
  if (auto SI = llvm::dyn_cast<llvm::StoreInst>(P->getNextNode())) {
    if (SI->getValueOperand() == P) {
      auto Ret = SI->getPointerOperand();
      llvm::AllocaInst *AI = nullptr;
      if ((AI = llvm::dyn_cast<llvm::AllocaInst>(Ret)) &&
          AI->getParent()->isEntryBlock()) {
        SI->eraseFromParent();
        return Ret;
      }
    }
  }
  return nullptr;
}

/// DemotePHIToStack - This function takes a virtual register computed by a PHI
/// node and replaces it with a slot in the stack frame allocated via alloca.
/// The PHI node is deleted. It returns the pointer to the alloca inserted.
llvm::Value *DemotePHIToStack2(llvm::PHINode *P,
                               llvm::Instruction *AllocaPoint) {
  using namespace llvm;
  if (P->use_empty()) {
    P->eraseFromParent();
    return nullptr;
  }

  const DataLayout &DL = P->getModule()->getDataLayout();

  // Create a stack slot to hold the value.
  Value *Slot = nullptr;
  if ((Slot = PhiHaveAddress(P))) {
  } else if (AllocaPoint) {
    Slot = new AllocaInst(P->getType(), DL.getAllocaAddrSpace(), nullptr,
                          P->getName() + ".reg2mem", AllocaPoint);
  } else {
    Function *F = P->getParent()->getParent();
    Slot =
        new AllocaInst(P->getType(), DL.getAllocaAddrSpace(), nullptr,
                       P->getName() + ".reg2mem", &F->getEntryBlock().front());
  }

  // Iterate over each operand inserting a store in each predecessor.
  for (unsigned i = 0, e = P->getNumIncomingValues(); i < e; ++i) {
    if (InvokeInst *II = dyn_cast<InvokeInst>(P->getIncomingValue(i))) {
      assert(II->getParent() != P->getIncomingBlock(i) &&
             "Invoke edge not supported yet");
      (void)II;
    }
    new StoreInst(P->getIncomingValue(i), Slot,
                  P->getIncomingBlock(i)->getTerminator());
  }

  // Insert a load in place of the PHI and replace all uses.
  BasicBlock::iterator InsertPt = P->getIterator();

  for (; isa<PHINode>(InsertPt) || InsertPt->isEHPad(); ++InsertPt)
    /* empty */; // Don't insert before PHI nodes or landingpad instrs.

  Value *V =
      new LoadInst(P->getType(), Slot, P->getName() + ".reload", &*InsertPt);
  P->replaceAllUsesWith(V);

  // Delete PHI.
  P->eraseFromParent();
  return Slot;
}

llvm::PreservedAnalyses DemotePhiPass::run(llvm::Function &F,
                                           llvm::FunctionAnalysisManager &) {
  using namespace llvm;
  // Insert all new allocas into entry block.
  BasicBlock *BBEntry = &F.getEntryBlock();
  if (!pred_empty(BBEntry)) {
    auto NewEntry = BasicBlock::Create(F.getContext(), "entry", &F, BBEntry);
    BranchInst::Create(BBEntry, NewEntry);
    BBEntry = NewEntry;
  }

  // Find first non-alloca instruction and create insertion point. This is
  // safe if block is well-formed: it always have terminator, otherwise
  // we'll get and assertion.
  BasicBlock::iterator I = BBEntry->begin();
  while (isa<AllocaInst>(I))
    ++I;

  CastInst *AllocaInsertionPoint = new BitCastInst(
      Constant::getNullValue(Type::getInt32Ty(F.getContext())),
      Type::getInt32Ty(F.getContext()), "reg2mem alloca point", &*I);
  std::list<Instruction *> WorkList;
  // Find all phi's
  for (BasicBlock &BB : F)
    for (auto &Phi : BB.phis())
      WorkList.push_front(&Phi);

  // Demote phi nodes
  for (Instruction *I : WorkList)
    DemotePHIToStack2(cast<PHINode>(I), AllocaInsertionPoint);
  return llvm::PreservedAnalyses::none();
}

// https://github.com/llvm/llvm-project/blob/7cf1fef45f13991e2d3b97e0612cfb88bf906a50/llvm/examples/IRTransforms/SimplifyCFG.cpp#L63
static bool removeDeadBlocks_v1(llvm::Function &F) {
  using namespace llvm;
  bool Changed = false;

  // Remove trivially dead blocks.
  for (BasicBlock &BB : make_early_inc_range(F)) {
    // Skip blocks we know to not be trivially dead. We know a block is
    // guaranteed to be dead, iff it is neither the entry block nor
    // has any predecessors.
    if (&F.getEntryBlock() == &BB || !pred_empty(&BB))
      continue;

    // Notify successors of BB that BB is going to be removed. This removes
    // incoming values from BB from PHIs in the successors. Note that this will
    // not actually remove BB from the predecessor lists of its successors.
    for (BasicBlock *Succ : successors(&BB))
      Succ->removePredecessor(&BB);
    // TODO: Find a better place to put such small variations.
    // Alternatively, we can update the PHI nodes manually:
    // for (PHINode &PN : make_early_inc_range(Succ->phis()))
    //  PN.removeIncomingValue(&BB);

    // Replace all instructions in BB with an undef constant. The block is
    // unreachable, so the results of the instructions should never get used.
    while (!BB.empty()) {
      Instruction &I = BB.back();
      I.replaceAllUsesWith(UndefValue::get(I.getType()));
      I.eraseFromParent();
    }

    // Finally remove the basic block.
    BB.eraseFromParent();
    Changed = true;
  }

  return Changed;
}

static bool mergeIntoSinglePredecessor_v1(llvm::Function &F) {
  using namespace llvm;
  bool Changed = false;

  // Merge blocks with single predecessors.
  for (BasicBlock &BB : make_early_inc_range(F)) {
    BasicBlock *Pred = BB.getSinglePredecessor();
    // Make sure  BB has a single predecessor Pred and BB is the single
    // successor of Pred.
    if (!Pred || Pred->getSingleSuccessor() != &BB)
      continue;

    // Do not try to merge self loops. That can happen in dead blocks.
    if (Pred == &BB)
      continue;

    // Need to replace it before nuking the branch.
    BB.replaceAllUsesWith(Pred);
    // PHI nodes in BB can only have a single incoming value. Remove them.
    for (PHINode &PN : make_early_inc_range(BB.phis())) {
      assert(PN.getNumIncomingValues() == 1);
      PN.replaceAllUsesWith(PN.getIncomingValue(0));
      PN.eraseFromParent();
    }
    // Move all instructions from BB to Pred.
    for (Instruction &I : make_early_inc_range(BB))
      I.moveBefore(Pred->getTerminator());

    // Remove the Pred's terminator (which jumped to BB). BB's terminator
    // will become Pred's terminator.
    Pred->getTerminator()->eraseFromParent();
    BB.eraseFromParent();

    Changed = true;
  }

  return Changed;
}

static bool simplifyCondBrSameLabel(llvm::Function &F) {
  using namespace llvm;
  bool Changed = false;
  for (BasicBlock &BB : make_early_inc_range(F)) {
    if (auto br = dyn_cast<BranchInst>(BB.getTerminator())) {
      if (br->isConditional() && br->getSuccessor(0) == br->getSuccessor(1)) {
        BranchInst::Create(br->getSuccessor(0), br);
        br->eraseFromParent();
        Changed = true;
      }
    }
  }
  return Changed;
}

// eliminate a block with only uncond br.
static bool eliminateEmptyBr(llvm::Function &F) {
  using namespace llvm;
  bool Changed = false;
  // Remove blocks with single successors.
  for (BasicBlock &BB : make_early_inc_range(F)) {
    if (BB.isEntryBlock()) {
      continue;
    }
    BasicBlock *Succ = BB.getSingleSuccessor();
    // Make sure BB has a single successor Succ and BB is empty
    if (!Succ || BB.size() > 1)
      continue;
    assert(isa<BranchInst>(BB.front()));
    assert(cast<BranchInst>(BB.front()).isUnconditional());

    // If there is phi nodes.
    if (!Succ->phis().empty()) {
      // cannot eliminate entry block if there is a phi. Because entry block
      // cannot have phi nodes.
      if (BB.isEntryBlock()) {
        continue;
      }
      // Cannot handle multiple phi nodes.
      if (pred_size(&BB) > 1) {
        continue;
      }
      auto Pred = *pred_begin(&BB);
      if (Pred->isEntryBlock()) {
        continue;
      }
      // Check if Succ's phi, have different incoming values for BB and
      // Pred, then we cannot eliminate the block
      bool canEliminate = true;
      for (PHINode &PN : Succ->phis()) {
        Value *BBVal = nullptr;
        Value *PredVal = nullptr;
        auto BBInd = PN.getBasicBlockIndex(&BB);
        // PredInd is likely to be -1
        auto PredInd = PN.getBasicBlockIndex(Pred);
        if (BBInd >= 0) {
          BBVal = PN.getIncomingValue(BBInd);
        }
        if (PredInd >= 0) {
          PredVal = PN.getIncomingValue(PredInd);
        }

        if (BBVal != nullptr && PredVal != nullptr) {
          canEliminate = false;
        }
        if (!canEliminate)
          break;
      }
      if (!canEliminate)
        continue;

      Pred->getTerminator()->replaceSuccessorWith(&BB, Succ);
      Succ->replacePhiUsesWith(&BB, Pred);
      BB.eraseFromParent();
      Changed = true;
    } else {
      BB.replaceAllUsesWith(Succ);
      // for (BasicBlock *Pred : make_early_inc_range(predecessors(&BB))) {
      //   Pred->getTerminator()->replaceSuccessorWith(&BB, Succ);
      // }

      if (!BB.materialized_use_empty()) {
        errs() << "While deleting BasicBlock: %" << BB.getName() << "\n";
        for (auto *U : BB.users())
          errs() << "Use still stuck around after Def is destroyed:" << *U
                 << "\n";
      }
      BB.eraseFromParent();
      Changed = true;
    }
  }

  return Changed;
}

llvm::PreservedAnalyses AdjustCFGPass::run(llvm::Function &F,
                                           llvm::FunctionAnalysisManager &) {
  bool Changed;
  do {
    Changed = false;
    Changed |= removeDeadBlocks_v1(F);
    // if (llvm::verifyFunction(F, &llvm::errs())) {
    //   assert(false && "Broken Function!");
    // }
    Changed |= mergeIntoSinglePredecessor_v1(F);
    // if (llvm::verifyFunction(F, &llvm::errs())) {
    //   assert(false && "Broken Function!");
    // }
    Changed |= eliminateEmptyBr(F);
    // if (llvm::verifyFunction(F, &llvm::errs())) {
    //   assert(false && "Broken Function!");
    // }
    Changed |= simplifyCondBrSameLabel(F);
    // if (llvm::verifyFunction(F, &llvm::errs())) {
    //   assert(false && "Broken Function!");
    // }
  } while (Changed);
  return llvm::PreservedAnalyses::none();
}

static llvm::Value *matchReturn(llvm::BasicBlock &BB) {
  auto it = BB.begin();
  if (auto *r = llvm::dyn_cast<llvm::ReturnInst>(&*it)) {
    return r->getReturnValue();
  }
  if (auto *p = llvm::dyn_cast<llvm::PHINode>(&*it)) {
    it++;
    if (auto *r = llvm::dyn_cast<llvm::ReturnInst>(&*it)) {
      if (p->hasOneUse() && r->getReturnValue() == p) {
        return p;
      }
    }
  }
  return nullptr;
}

llvm::PreservedAnalyses RetDupPass::run(llvm::Function &F,
                                        llvm::FunctionAnalysisManager &) {
  using namespace llvm;
  auto &C = F.getContext();
  IRBuilder<> builder(C);
  std::vector<llvm::BasicBlock *> BBS;
  for (auto &B : F) {
    BBS.push_back(&B);
  }
  for (auto B : BBS) {
    // match the block with only phi and return
    auto r = matchReturn(*B);
    if (r == nullptr) {
      continue;
    }
    std::set<llvm::BasicBlock *> preds(pred_begin(B), pred_end(B));
    if (preds.size() > 1) {
      for (auto pred : preds) {
        if (pred->getSingleSuccessor() == B) {
          auto br = pred->getTerminator();
          builder.SetInsertPoint(br);
          PHINode *p = nullptr;
          if ((p = llvm::dyn_cast<llvm::PHINode>(r)) && p->getParent() == B) {
            auto rv = p->getIncomingValueForBlock(pred);
            builder.CreateRet(rv);
          } else {
            builder.CreateRet(r);
          }
          B->removePredecessor(pred, true);
          br->eraseFromParent();
        } else {
          BasicBlock *N = BasicBlock::Create(C, B->getName() + "_dup", &F, B);
          builder.SetInsertPoint(N);
          if (auto *p = llvm::dyn_cast<llvm::PHINode>(r)) {
            if (p->getParent() == B) {
              auto rv = p->getIncomingValueForBlock(pred);
              builder.CreateRet(rv);
            } else {
              builder.CreateRet(r);
            }
          } else {
            builder.CreateRet(r);
          }
          // update the pred
          B->removePredecessor(pred, true);
          pred->getTerminator()->replaceSuccessorWith(B, N);
        }
      }

      assert(B->hasNPredecessors(0));
      assert(B->getNumUses() == 0);
      // std::vector<Instruction *> Vec;
      // for (auto &I : *B) {
      //   Vec.push_back(&I);
      // }
      // for (auto It = Vec.rbegin(); It != Vec.rend(); It++) {
      //   (*It)->eraseFromParent();
      // }
      B->eraseFromParent();
    }
  }
  return PreservedAnalyses::none();
}

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

std::string llvmObjToString(const llvm::Module *t) {
  std::string str;
  llvm::raw_string_ostream ss(str);
  if (t)
    t->print(ss, nullptr);
  else
    ss << "nullptr";
  return ss.str();
}

clang::Expr *getNoCast(clang::Expr *E) {
  clang::Expr *ENoCast = E;
  while (auto Cast = llvm::dyn_cast<clang::CastExpr>(ENoCast)) {
    ENoCast = Cast->getSubExpr();
  }
  return ENoCast;
}

clang::Expr *makeNonArrow(clang::MemberExpr *M) {
  assert(M->isArrow());
  M->setArrow(false);
  return M;
  // assert(!M->hasExplicitTemplateArgs());
  // assert(!M->getTemplateArgs());

  // clang::Expr *base = M->getBase();
  // bool isArrow = false; // convert from -> to .

  // return clang::MemberExpr::Create(
  //     Ctx, base, isArrow, M->getOperatorLoc(), M->getQualifierLoc(),
  //     M->getTemplateKeywordLoc(), M->getMemberDecl(), M->getFoundDecl(),
  //     M->getMemberNameInfo(), nullptr, M->getType(), M->getValueKind(),
  //     M->getObjectKind(), clang::NOUR_None);
}

clang::Expr *makeArrow(clang::MemberExpr *M) {
  assert(!M->isArrow());
  M->setArrow(true);
  return M;
}

clang::Expr *createMemberExpr(clang::ASTContext &Ctx, clang::Expr *Base,
                              clang::FieldDecl *Field) {
  // check if the val is deref, if so, then remove it and use arrow expr.
  bool useArrow = false;
  if (auto DerefInner = getDerefInner(Base)) {
    Base = DerefInner;
    useArrow = true;
  }
  return clang::MemberExpr::Create(
      Ctx, addParenthesis<clang::MemberExpr>(Ctx, Base, true), useArrow,
      clang::SourceLocation(), clang::NestedNameSpecifierLoc(),
      clang::SourceLocation(), Field,
      clang::DeclAccessPair::make(Field, Field->getAccess()),
      clang::DeclarationNameInfo(), nullptr, Field->getType(), clang::VK_LValue,
      clang::OK_Ordinary, clang::NOUR_None);
}

clang::Expr *getDerefInner(clang::Expr *E) {
  clang::Expr *ENoCast = getNoCast(E);
  if (llvm::isa<clang::UnaryOperator>(ENoCast) &&
      llvm::cast<clang::UnaryOperator>(ENoCast)->getOpcode() ==
          clang::UO_Deref) {
    return llvm::cast<clang::UnaryOperator>(ENoCast)->getSubExpr();
  }
  // eliminate arrow operator is incorrect.
  // // arrow operator = deref + non-arrow. return non-arrow
  // if (auto MemberExpr = llvm::dyn_cast<clang::MemberExpr>(E)) {
  //   if (MemberExpr->isArrow()) {
  //     return makeNonArrow(MemberExpr);
  //   }
  // }
  return nullptr;
}

clang::Expr *getAddrofInner(clang::Expr *E) {
  clang::Expr *ENoCast = getNoCast(E);
  if (llvm::isa<clang::UnaryOperator>(ENoCast) &&
      llvm::cast<clang::UnaryOperator>(ENoCast)->getOpcode() ==
          clang::UO_AddrOf) {
    return llvm::cast<clang::UnaryOperator>(ENoCast)->getSubExpr();
  }
  return nullptr;
}

clang::Expr *addrOf(clang::ASTContext &Ctx, clang::Expr *E, bool NoElimMember) {
  // eliminate addrOf + deref
  clang::Expr *ENoCast = getNoCast(E);
  if (llvm::isa<clang::UnaryOperator>(ENoCast) &&
      llvm::cast<clang::UnaryOperator>(ENoCast)->getOpcode() ==
          clang::UO_Deref) {
    return llvm::cast<clang::UnaryOperator>(ENoCast)->getSubExpr();
  }
  // eliminate addrOf + array subscript 0.
  if (!NoElimMember) {
    if (auto ArraySub = llvm::dyn_cast<clang::ArraySubscriptExpr>(E)) {
      if (auto Index =
              llvm::dyn_cast<clang::IntegerLiteral>(ArraySub->getIdx())) {
        if (Index->getValue() == 0) {
          return llvm::cast<clang::ArraySubscriptExpr>(E)->getBase();
        }
      }
    }
  }

  // eliminate arrow operator is incorrect.
  // // eliminate arrow operator.
  // if (auto MemberExpr = llvm::dyn_cast<clang::MemberExpr>(E)) {
  //   if (MemberExpr->isArrow()) {
  //     return makeNonArrow(MemberExpr);
  //   }
  // }
  return createUnaryOperator(Ctx, E, clang::UO_AddrOf,
                             Ctx.getPointerType(E->getType()),
                             clang::VK_LValue);
}

clang::Expr *deref(clang::ASTContext &Ctx, clang::Expr *E, bool SkipCast) {
  // eliminate deref + addrOf
  clang::Expr *ENoCast = E;
  if (SkipCast) {
    clang::CastExpr *Cast = nullptr;
    // only when same size.
    while ((Cast = llvm::dyn_cast<clang::CastExpr>(ENoCast)) &&
           (Ctx.getTypeSize(Cast->getType()) ==
            Ctx.getTypeSize(ENoCast->getType()))) {
      ENoCast = Cast->getSubExpr();
    }
  }

  if (llvm::isa<clang::UnaryOperator>(ENoCast) &&
      llvm::cast<clang::UnaryOperator>(ENoCast)->getOpcode() ==
          clang::UO_AddrOf) {
    return llvm::cast<clang::UnaryOperator>(ENoCast)->getSubExpr();
  }
  // use arrow operator is incorrect
  // // use arrow operator.
  // if (auto MemberExpr = llvm::dyn_cast<clang::MemberExpr>(E)) {
  //   if (!MemberExpr->isArrow()) {
  //     return makeArrow(MemberExpr);
  //   }
  // }
  clang::QualType Ty = E->getType();
  if (Ty->isPointerType()) {
    assert(!Ty->isVoidPointerType());
    Ty = Ty->getPointeeType();
  } else if (Ty->isArrayType()) {
    Ty = Ty->castAsArrayTypeUnsafe()->getElementType();
    // Create array subscript operator
    auto ArrSub = new (Ctx) clang::ArraySubscriptExpr(
        addParenthesis<clang::ArraySubscriptExpr>(Ctx, (E), false),
        clang::IntegerLiteral::Create(Ctx, llvm::APInt::getZero(32), Ctx.IntTy,
                                      clang::SourceLocation()),
        Ty, clang::VK_LValue, clang::OK_Ordinary, clang::SourceLocation());
    return ArrSub;
  } else {
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "ERROR: CFGBuilder.deref: unexpected type: ";
    Ty.dump();
    llvm::errs() << "\n";
    // std::abort();
  }
  return createUnaryOperator(Ctx, E, clang::UO_Deref, Ty, clang::VK_LValue);
}

} // namespace notdec::llvm2c
