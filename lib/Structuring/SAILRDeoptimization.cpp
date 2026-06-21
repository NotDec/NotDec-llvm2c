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

bool containsBlock(const std::vector<BlockId> &Blocks, BlockId Id) {
  return std::find(Blocks.begin(), Blocks.end(), Id) != Blocks.end();
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

struct ReturnRegion {
  BlockId Head = InvalidBlockId;
  std::vector<BlockId> Blocks;
};

std::vector<BlockId> externalPredecessorsOf(const StructuredCFG &Graph,
                                            const ReturnRegion &Region) {
  std::vector<BlockId> Preds;
  for (BlockId Pred : predecessorsOf(Graph, Region.Head)) {
    if (!containsBlock(Region.Blocks, Pred)) {
      Preds.push_back(Pred);
    }
  }
  return Preds;
}

ReturnRegion findLinearReturnRegion(const StructuredCFG &Graph,
                                    BlockId ReturnBlock) {
  ReturnRegion Region;
  const CFGBlock *EndBlock = Graph.getBlock(ReturnBlock);
  if (EndBlock == nullptr || EndBlock->Terminator != TerminatorKind::Return ||
      !EndBlock->Successors.empty()) {
    return Region;
  }

  Region.Head = ReturnBlock;
  Region.Blocks.push_back(ReturnBlock);

  std::set<BlockId> Seen;
  Seen.insert(ReturnBlock);
  while (true) {
    std::vector<BlockId> Preds = predecessorsOf(Graph, Region.Head);
    if (Preds.size() != 1) {
      break;
    }

    BlockId Pred = Preds.front();
    if (Seen.count(Pred) != 0) {
      break;
    }

    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    // Without payload-level Phi/vvar rewriting, only copy straight-line
    // return tails. Branch and switch regions need richer shared semantics.
    if (PredBlock == nullptr ||
        PredBlock->Terminator != TerminatorKind::Fallthrough ||
        PredBlock->Successors.size() != 1) {
      break;
    }

    Region.Head = Pred;
    Region.Blocks.push_back(Pred);
    Seen.insert(Pred);
  }

  std::reverse(Region.Blocks.begin(), Region.Blocks.end());
  return Region;
}

bool copyRegionForPredecessor(StructuredCFG &Graph, const ReturnRegion &Region,
                              BlockId Pred, std::vector<BlockId> &OutCopies) {
  std::map<BlockId, BlockId> CopyByOriginal;
  std::vector<BlockId> Copies;

  for (BlockId Original : Region.Blocks) {
    BlockId Copy = Graph.duplicateBlock(Original, {});
    if (Copy == InvalidBlockId) {
      for (BlockId OldCopy : Copies) {
        Graph.removeBlock(OldCopy);
      }
      return false;
    }
    CopyByOriginal[Original] = Copy;
    Copies.push_back(Copy);
  }

  for (BlockId Original : Region.Blocks) {
    const CFGBlock *OriginalBlock = Graph.getBlock(Original);
    CFGBlock *CopyBlock = Graph.getBlock(CopyByOriginal[Original]);
    if (OriginalBlock == nullptr || CopyBlock == nullptr) {
      for (BlockId Copy : Copies) {
        Graph.removeBlock(Copy);
      }
      return false;
    }

    CopyBlock->Successors.clear();
    for (BlockId Succ : OriginalBlock->Successors) {
      auto It = CopyByOriginal.find(Succ);
      CopyBlock->Successors.push_back(It == CopyByOriginal.end() ? Succ
                                                                 : It->second);
    }
    for (SwitchCase &Case : CopyBlock->Cases) {
      auto It = CopyByOriginal.find(Case.Target);
      if (It != CopyByOriginal.end()) {
        Case.Target = It->second;
      }
    }
  }

  CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr ||
      !replaceSuccessor(*PredBlock, Region.Head, CopyByOriginal[Region.Head])) {
    for (BlockId Copy : Copies) {
      Graph.removeBlock(Copy);
    }
    return false;
  }

  OutCopies.insert(OutCopies.end(), Copies.begin(), Copies.end());
  return true;
}

} // namespace

StructuringOptimizationOptions ReturnDuplicatorLow::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 4;
  return Options;
}

bool ReturnDuplicatorLow::runOnGraph(StructuredCFG &Graph,
                                     const StructuringEvaluation &Current) {
  std::map<BlockId, ReturnRegion> Regions;

  for (const CFGBlock &Block : Graph.blocks()) {
    ReturnRegion Region = findLinearReturnRegion(Graph, Block.Id);
    if (Region.Head == InvalidBlockId) {
      continue;
    }
    std::vector<BlockId> Preds = externalPredecessorsOf(Graph, Region);
    if (Preds.size() <= 1) {
      continue;
    }
    Regions.emplace(Region.Head, std::move(Region));
  }

  bool Changed = false;
  for (auto &Entry : Regions) {
    const ReturnRegion &Region = Entry.second;
    std::vector<BlockId> CurrentPreds = externalPredecessorsOf(Graph, Region);
    std::vector<BlockId> GotoPreds;
    for (BlockId Pred : CurrentPreds) {
      if (Current.Gotos.isGotoEdge(Pred, Region.Head)) {
        GotoPreds.push_back(Pred);
      }
    }

    if (GotoPreds.empty()) {
      continue;
    }

    std::vector<BlockId> PredsToUpdate = GotoPreds;
    if (CurrentPreds.size() > 2 &&
        GotoPreds.size() >= CurrentPreds.size() - 2) {
      PredsToUpdate = CurrentPreds;
    }

    std::sort(PredsToUpdate.begin(), PredsToUpdate.end());
    PredsToUpdate.erase(std::unique(PredsToUpdate.begin(), PredsToUpdate.end()),
                        PredsToUpdate.end());
    bool DeleteOriginal = sameBlockSet(CurrentPreds, PredsToUpdate);

    std::vector<BlockId> Copies;
    std::vector<BlockId> UpdatedPreds;
    for (BlockId Pred : PredsToUpdate) {
      if (!copyRegionForPredecessor(Graph, Region, Pred, Copies)) {
        continue;
      }
      UpdatedPreds.push_back(Pred);
      Changed = true;
    }

    if (DeleteOriginal && UpdatedPreds.size() == CurrentPreds.size() &&
        sameBlockSet(CurrentPreds, UpdatedPreds)) {
      for (BlockId Block : Region.Blocks) {
        Graph.removeBlock(Block);
      }
    }
  }

  return Changed;
}

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
    std::vector<BlockId> UpdatedPreds;
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
      UpdatedPreds.push_back(Pred);
      Changed = true;
    }

    if (DeleteOriginal && Copies.size() == CurrentPreds.size() &&
        sameBlockSet(CurrentPreds, UpdatedPreds)) {
      Graph.removeBlock(Target);
    }
  }

  return Changed;
}

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline() {
  StructuringOptimizationPipeline Pipeline;
  Pipeline.addPass(std::make_unique<ReturnDuplicatorLow>());
  Pipeline.addPass(std::make_unique<CrossJumpReverter>());
  return Pipeline;
}

} // namespace notdec::backend::structuring
