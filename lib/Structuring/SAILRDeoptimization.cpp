#include "notdec-backends/Structuring/SAILRDeoptimization.h"

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <vector>

namespace notdec::backend::structuring {
namespace {

bool hasSuccessor(const CFGBlock &Block, BlockId Target) {
  return std::find(Block.Successors.begin(), Block.Successors.end(), Target) !=
         Block.Successors.end();
}

std::vector<BlockId> predecessorsOf(const StructuredCFG &Graph,
                                    BlockId Target) {
  std::vector<BlockId> Preds;
  for (const CFGBlock &Block : Graph.blocks()) {
    if (hasSuccessor(Block, Target)) {
      Preds.push_back(Block.Id);
    }
  }
  return Preds;
}

bool sameBlockSet(std::vector<BlockId> A, std::vector<BlockId> B) {
  std::sort(A.begin(), A.end());
  std::sort(B.begin(), B.end());
  return A == B;
}

bool replaceSuccessor(CFGBlock &Block, BlockId OldTarget, BlockId NewTarget) {
  bool Changed = false;
  for (BlockId &Succ : Block.Successors) {
    if (Succ == OldTarget) {
      Succ = NewTarget;
      Changed = true;
    }
  }
  for (SwitchCase &Case : Block.Cases) {
    if (Case.Target == OldTarget) {
      Case.Target = NewTarget;
      Changed = true;
    }
  }
  return Changed;
}

} // namespace

StructuringOptimizationOptions CrossJumpReverter::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.StrictlyLessGotos = true;
  Options.MaxOptIters = 3;
  return Options;
}

bool CrossJumpReverter::runOnGraph(StructuredCFG &Graph,
                                   const StructuringEvaluation &Current) {
  std::map<BlockId, std::vector<BlockId>> ToUpdate;

  for (const CFGBlock &Block : Graph.blocks()) {
    std::vector<StructuredGoto> Gotos = Current.Gotos.gotosInBlock(Block.Id);
    if (Gotos.empty() || Gotos.size() >= 2) {
      continue;
    }

    BlockId Target = Gotos.front().Target;
    if (!hasSuccessor(Block, Target)) {
      continue;
    }

    const CFGBlock *TargetBlock = Graph.getBlock(Target);
    if (TargetBlock == nullptr || TargetBlock->Successors.size() != 1) {
      continue;
    }
    if (TargetBlock->Statements.size() > MaxDuplicatedStatements) {
      continue;
    }

    ToUpdate[Target].push_back(Block.Id);
  }

  bool Changed = false;
  for (auto &Entry : ToUpdate) {
    BlockId Target = Entry.first;
    std::vector<BlockId> &PredsToUpdate = Entry.second;
    std::sort(PredsToUpdate.begin(), PredsToUpdate.end());
    PredsToUpdate.erase(std::unique(PredsToUpdate.begin(), PredsToUpdate.end()),
                        PredsToUpdate.end());

    const CFGBlock *TargetBlock = Graph.getBlock(Target);
    if (TargetBlock == nullptr) {
      continue;
    }
    std::vector<BlockId> TargetSuccessors = TargetBlock->Successors;

    std::vector<BlockId> CurrentPreds = predecessorsOf(Graph, Target);
    bool DeleteOriginal = sameBlockSet(CurrentPreds, PredsToUpdate);

    std::vector<BlockId> Copies;
    for (BlockId Pred : PredsToUpdate) {
      CFGBlock *PredBlock = Graph.getBlock(Pred);
      if (PredBlock == nullptr || !hasSuccessor(*PredBlock, Target)) {
        continue;
      }

      BlockId Copy = Graph.duplicateBlock(Target, TargetSuccessors);
      if (Copy == InvalidBlockId) {
        continue;
      }
      PredBlock = Graph.getBlock(Pred);
      if (PredBlock == nullptr) {
        Graph.removeBlock(Copy);
        continue;
      }
      if (!replaceSuccessor(*PredBlock, Target, Copy)) {
        Graph.removeBlock(Copy);
        continue;
      }

      Copies.push_back(Copy);
      Changed = true;
    }

    if (DeleteOriginal && !Copies.empty()) {
      Graph.removeBlock(Target);
    }
  }

  return Changed;
}

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline() {
  StructuringOptimizationPipeline Pipeline;
  Pipeline.addPass(std::make_unique<CrossJumpReverter>());
  return Pipeline;
}

} // namespace notdec::backend::structuring
