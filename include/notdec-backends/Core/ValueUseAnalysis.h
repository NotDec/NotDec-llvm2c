#ifndef NOTDEC_BACKENDS_CORE_VALUEUSEANALYSIS_H
#define NOTDEC_BACKENDS_CORE_VALUEUSEANALYSIS_H

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

namespace notdec::backend::core {

// SSA value-use predicates shared by the code backends.  The llvm2c C backend
// (StructuralAnalysis.cpp:addExprOrStmt) uses them to decide whether an
// instruction expression can be folded into its use sites or has to be cached
// in a local variable; the Solidity backend uses the same decision for private
// helper call results.  They live in this clang-free header so the two backends
// cannot drift apart: StructuralAnalysis.h pulls in clang, which the Solidity
// backend must not depend on.
//
// Which values a backend actually caches stays backend-specific.  The C backend
// materializes everything that is not foldable and never folds LoadInst; the
// Solidity backend currently applies the same decision only to rendered helper
// calls, because its other expressions are side-effect free.

// Exactly one use.  A cast on the way does not count as a second use, but that
// cast must then have a single use itself.
inline bool hasOneUseIgnoreCast(const llvm::Value &Val) {
  if (Val.hasOneUse()) {
    if (const auto *Cast = llvm::dyn_cast<llvm::CastInst>(*Val.user_begin())) {
      return Cast->hasOneUse();
    }
    return true;
  }
  return false;
}

// Any user of the instruction lives in the given block.
inline bool usedInBlock(const llvm::Instruction &Inst,
                        const llvm::BasicBlock &BB) {
  for (const llvm::User *U : Inst.users()) {
    if (const auto *UI = llvm::dyn_cast<llvm::Instruction>(U)) {
      if (UI->getParent() == &BB) {
        return true;
      }
    }
  }
  return false;
}

// Has one use ignoring casts, and that use is in the defining block.
inline bool onlyUsedInCurrentBlock(const llvm::Instruction &Inst) {
  const llvm::BasicBlock *BB = Inst.getParent();
  return BB != nullptr && hasOneUseIgnoreCast(Inst) && usedInBlock(Inst, *BB);
}

} // namespace notdec::backend::core

#endif
