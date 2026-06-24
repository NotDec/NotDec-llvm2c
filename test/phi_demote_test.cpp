#include "notdec-llvm2c/StructuralAnalysis.h"

#include <cassert>
#include <memory>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/Casting.h>

namespace {

bool hasPhi(const llvm::Function &F) {
  for (const llvm::BasicBlock &BB : F) {
    for (const llvm::Instruction &I : BB) {
      if (llvm::isa<llvm::PHINode>(&I)) {
        return true;
      }
    }
  }
  return false;
}

void testDemoteSSAFixHTKeepsUnnamedPhiTypes() {
  llvm::LLVMContext Ctx;
  llvm::Module M("phi-demote-test", Ctx);
  M.setDataLayout("e-p:32:32");

  llvm::Type *I1 = llvm::Type::getInt1Ty(Ctx);
  llvm::Type *I32 = llvm::Type::getInt32Ty(Ctx);
  auto *FTy = llvm::FunctionType::get(I32, {I1, I32, I32}, false);
  auto *F = llvm::Function::Create(FTy, llvm::GlobalValue::ExternalLinkage,
                                   "with_unnamed_phi", M);

  auto ArgIt = F->arg_begin();
  llvm::Argument *Cond = &*ArgIt++;
  llvm::Argument *A = &*ArgIt++;
  llvm::Argument *B = &*ArgIt++;

  auto *Entry = llvm::BasicBlock::Create(Ctx, "entry", F);
  auto *Then = llvm::BasicBlock::Create(Ctx, "then", F);
  auto *Else = llvm::BasicBlock::Create(Ctx, "else", F);
  auto *Merge = llvm::BasicBlock::Create(Ctx, "merge", F);

  llvm::IRBuilder<> Builder(Entry);
  Builder.CreateCondBr(Cond, Then, Else);
  Builder.SetInsertPoint(Then);
  Builder.CreateBr(Merge);
  Builder.SetInsertPoint(Else);
  Builder.CreateBr(Merge);
  Builder.SetInsertPoint(Merge);

  auto *Phi0 = Builder.CreatePHI(I32, 2);
  Phi0->addIncoming(A, Then);
  Phi0->addIncoming(B, Else);
  auto *Phi1 = Builder.CreatePHI(I32, 2);
  Phi1->addIncoming(B, Then);
  Phi1->addIncoming(A, Else);
  Builder.CreateRet(Builder.CreateAdd(Phi0, Phi1));

  notdec::llvm2c::HTypeResult HT;
  HT.HTCtx = std::make_shared<notdec::ast::HTypeContext>();
  auto *I32Ty = HT.HTCtx->getIntegerType(false, 32, false);
  HT.ValueTypesLower.insert({Phi0, I32Ty});
  HT.ValueTypesUpper.insert({Phi0, I32Ty});
  HT.ValueTypesLower.insert({Phi1, I32Ty});
  HT.ValueTypesUpper.insert({Phi1, I32Ty});
  HT.ContraVariantValues.insert(Phi1);

  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  notdec::llvm2c::demoteSSAFixHT(M, MAM, HT, nullptr);

  assert(!hasPhi(*F));
  assert(!HT.hasValueType(Phi0, true));
  assert(!HT.hasValueType(Phi0, false));
  assert(!HT.hasValueType(Phi1, true));
  assert(!HT.hasValueType(Phi1, false));

  unsigned TypedPhiSlots = 0;
  unsigned ContraVariantSlots = 0;
  for (llvm::BasicBlock &BB : *F) {
    for (llvm::Instruction &I : BB) {
      auto *AI = llvm::dyn_cast<llvm::AllocaInst>(&I);
      if (AI == nullptr || !AI->getName().starts_with("notdec.phi")) {
        continue;
      }

      ++TypedPhiSlots;
      auto *LowerTy = HT.getValueType(AI, true);
      auto *UpperTy = HT.getValueType(AI, false);
      assert(LowerTy != nullptr && LowerTy->isPointerType());
      assert(UpperTy != nullptr && UpperTy->isPointerType());
      assert(LowerTy->getPointeeType() == I32Ty);
      assert(UpperTy->getPointeeType() == I32Ty);
      if (HT.ContraVariantValues.count(AI) != 0) {
        ++ContraVariantSlots;
      }
    }
  }

  assert(TypedPhiSlots == 2);
  assert(ContraVariantSlots == 1);
}

} // namespace

int main() {
  testDemoteSSAFixHTKeepsUnnamedPhiTypes();
  return 0;
}
