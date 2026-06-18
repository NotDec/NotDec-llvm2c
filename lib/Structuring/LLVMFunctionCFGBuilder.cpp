#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"

#include <map>
#include <utility>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

namespace notdec::backend::structuring {

StructuredCFG
LLVMFunctionCFGBuilder::build(const llvm::Function &F,
                              LLVMFunctionCFGBuilder::PayloadProvider &Provider) {
  StructuredCFG Cfg;
  std::map<const llvm::BasicBlock *, BlockId> BlockIds;

  for (const llvm::BasicBlock &BB : F) {
    BlockId Id = static_cast<BlockId>(BlockIds.size());
    BlockIds[&BB] = Id;
  }

  for (const llvm::BasicBlock &BB : F) {
    CFGBlock Block;
    Block.Id = BlockIds[&BB];
    Provider.collectStatements(BB, Block.Statements);

    const llvm::Instruction *Term = BB.getTerminator();
    if (const auto *Br = llvm::dyn_cast_or_null<llvm::BranchInst>(Term)) {
      if (Br->isConditional()) {
        Block.Terminator = TerminatorKind::Branch;
        Block.Condition = Provider.getCondition(*Br->getCondition(), "cond");
      } else {
        Block.Terminator = TerminatorKind::Fallthrough;
      }
      for (const llvm::BasicBlock *Succ : Br->successors()) {
        Block.Successors.push_back(BlockIds[Succ]);
      }
    } else if (const auto *Sw = llvm::dyn_cast_or_null<llvm::SwitchInst>(Term)) {
      Block.Terminator = TerminatorKind::Switch;
      Block.Condition = Provider.getCondition(*Sw->getCondition(), "switch");
      for (auto Case : Sw->cases()) {
        SwitchCase OutCase;
        OutCase.Value = Provider.getSwitchCase(*Case.getCaseValue());
        OutCase.Target = BlockIds[Case.getCaseSuccessor()];
        Block.Cases.push_back(OutCase);
      }
      for (unsigned I = 0; I < Sw->getNumSuccessors(); ++I) {
        Block.Successors.push_back(BlockIds[Sw->getSuccessor(I)]);
      }
    } else if (llvm::isa_and_nonnull<llvm::ReturnInst>(Term)) {
      Block.Terminator = TerminatorKind::Return;
    } else if (llvm::isa_and_nonnull<llvm::UnreachableInst>(Term)) {
      Block.Terminator = TerminatorKind::Unreachable;
    } else {
      Block.Terminator = TerminatorKind::Unreachable;
    }

    Cfg.addBlock(std::move(Block));
  }

  return Cfg;
}

} // namespace notdec::backend::structuring
