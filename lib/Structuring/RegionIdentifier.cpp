#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <map>
#include <set>
#include <vector>

namespace notdec::backend::structuring {

namespace {

bool contains(const std::set<BlockId> &Values, BlockId Id) {
  return Values.find(Id) != Values.end();
}

bool contains(const std::vector<BlockId> &Values, BlockId Id) {
  return std::find(Values.begin(), Values.end(), Id) != Values.end();
}

bool isSubset(const std::vector<BlockId> &A, const std::vector<BlockId> &B) {
  for (BlockId Id : A) {
    if (!contains(B, Id)) {
      return false;
    }
  }
  return true;
}

bool overlaps(const std::vector<BlockId> &A, const std::vector<BlockId> &B) {
  for (BlockId Id : A) {
    if (contains(B, Id)) {
      return true;
    }
  }
  return false;
}

std::set<BlockId> intersectSets(const std::set<BlockId> &A,
                                const std::set<BlockId> &B) {
  std::set<BlockId> Result;
  std::set_intersection(A.begin(), A.end(), B.begin(), B.end(),
                        std::inserter(Result, Result.begin()));
  return Result;
}

std::map<BlockId, std::set<BlockId>>
buildPredecessors(const StructuredCFG &Cfg) {
  std::map<BlockId, std::set<BlockId>> Preds;
  for (const CFGBlock &Block : Cfg.blocks()) {
    Preds[Block.Id];
    for (BlockId Succ : Block.Successors) {
      Preds[Succ].insert(Block.Id);
    }
  }
  return Preds;
}

std::map<BlockId, std::set<BlockId>>
computeDominators(const StructuredCFG &Cfg, BlockId Entry,
                  const std::map<BlockId, std::set<BlockId>> &Preds) {
  std::set<BlockId> All;
  for (const CFGBlock &Block : Cfg.blocks()) {
    All.insert(Block.Id);
  }

  std::map<BlockId, std::set<BlockId>> Dom;
  for (const CFGBlock &Block : Cfg.blocks()) {
    Dom[Block.Id] = Block.Id == Entry ? std::set<BlockId>{Block.Id} : All;
  }

  bool Changed = false;
  do {
    Changed = false;
    for (const CFGBlock &Block : Cfg.blocks()) {
      if (Block.Id == Entry) {
        continue;
      }

      std::set<BlockId> NewDom = All;
      bool HasPred = false;
      auto PredIt = Preds.find(Block.Id);
      if (PredIt != Preds.end()) {
        for (BlockId Pred : PredIt->second) {
          NewDom = HasPred ? intersectSets(NewDom, Dom[Pred]) : Dom[Pred];
          HasPred = true;
        }
      }
      if (!HasPred) {
        NewDom.clear();
      }
      NewDom.insert(Block.Id);
      if (NewDom != Dom[Block.Id]) {
        Dom[Block.Id] = std::move(NewDom);
        Changed = true;
      }
    }
  } while (Changed);

  return Dom;
}

std::vector<BlockId> collectNaturalLoop(
    BlockId Head, BlockId Latch,
    const std::map<BlockId, std::set<BlockId>> &Preds,
    const std::vector<BlockId> &BlockOrder) {
  std::set<BlockId> Loop;
  std::vector<BlockId> Worklist;
  Loop.insert(Head);
  if (Loop.insert(Latch).second) {
    Worklist.push_back(Latch);
  }

  while (!Worklist.empty()) {
    BlockId Current = Worklist.back();
    Worklist.pop_back();
    auto PredIt = Preds.find(Current);
    if (PredIt == Preds.end()) {
      continue;
    }
    for (BlockId Pred : PredIt->second) {
      if (Loop.insert(Pred).second && Pred != Head) {
        Worklist.push_back(Pred);
      }
    }
  }

  std::vector<BlockId> Result;
  for (BlockId Block : BlockOrder) {
    if (contains(Loop, Block)) {
      Result.push_back(Block);
    }
  }
  return Result;
}

std::vector<BlockId> collectSuccessors(const StructuredCFG &Cfg,
                                       const std::set<BlockId> &Blocks) {
  std::set<BlockId> Successors;
  for (BlockId Id : Blocks) {
    const CFGBlock *Block = Cfg.getBlock(Id);
    if (Block == nullptr) {
      continue;
    }
    for (BlockId Succ : Block->Successors) {
      if (!contains(Blocks, Succ)) {
        Successors.insert(Succ);
      }
    }
  }
  return {Successors.begin(), Successors.end()};
}

struct LoopCandidate {
  BlockId Head = InvalidBlockId;
  BlockId Latch = InvalidBlockId;
  std::vector<BlockId> Blocks;
};

LoopCandidate makeLoopCandidate(BlockId Head, BlockId Latch,
                                std::vector<BlockId> Blocks) {
  LoopCandidate Candidate;
  Candidate.Head = Head;
  Candidate.Latch = Latch;
  Candidate.Blocks = std::move(Blocks);
  return Candidate;
}

void mergeLoopCandidate(std::vector<LoopCandidate> &Candidates,
                        LoopCandidate Candidate,
                        const std::vector<BlockId> &BlockOrder) {
  for (LoopCandidate &Existing : Candidates) {
    if (Existing.Head != Candidate.Head) {
      continue;
    }

    std::set<BlockId> Blocks(Existing.Blocks.begin(), Existing.Blocks.end());
    Blocks.insert(Candidate.Blocks.begin(), Candidate.Blocks.end());
    std::vector<BlockId> Merged;
    for (BlockId Id : BlockOrder) {
      if (contains(Blocks, Id)) {
        Merged.push_back(Id);
      }
    }
    Existing.Blocks = std::move(Merged);
    Existing.Latch = Candidate.Latch;
    return;
  }
  Candidates.push_back(std::move(Candidate));
}

std::vector<LoopCandidate>
collectLoopCandidates(const StructuredCFG &Cfg,
                      const std::map<BlockId, std::set<BlockId>> &Preds,
                      const std::map<BlockId, std::set<BlockId>> &Dom,
                      const std::vector<BlockId> &BlockOrder) {
  std::vector<LoopCandidate> Candidates;
  for (const CFGBlock &Latch : Cfg.blocks()) {
    for (BlockId Succ : Latch.Successors) {
      auto DomIt = Dom.find(Latch.Id);
      if (DomIt == Dom.end() || !contains(DomIt->second, Succ)) {
        continue;
      }

      std::vector<BlockId> Blocks =
          collectNaturalLoop(Succ, Latch.Id, Preds, BlockOrder);
      if (Blocks.empty()) {
        continue;
      }
      mergeLoopCandidate(Candidates,
                         makeLoopCandidate(Succ, Latch.Id, std::move(Blocks)),
                         BlockOrder);
    }
  }
  return Candidates;
}

std::vector<LoopCandidate>
filterLaminarLoops(std::vector<LoopCandidate> Candidates) {
  std::vector<LoopCandidate> Result;
  std::sort(Candidates.begin(), Candidates.end(),
            [](const LoopCandidate &A, const LoopCandidate &B) {
              if (A.Blocks.size() != B.Blocks.size()) {
                return A.Blocks.size() < B.Blocks.size();
              }
              return A.Head < B.Head;
            });

  for (LoopCandidate &Candidate : Candidates) {
    bool HasPartialOverlap = false;
    for (const LoopCandidate &Existing : Result) {
      if (!overlaps(Candidate.Blocks, Existing.Blocks)) {
        continue;
      }
      if (!isSubset(Candidate.Blocks, Existing.Blocks) &&
          !isSubset(Existing.Blocks, Candidate.Blocks)) {
        HasPartialOverlap = true;
        break;
      }
    }
    if (!HasPartialOverlap) {
      Result.push_back(std::move(Candidate));
    }
  }

  return Result;
}

Region makeLoopRegion(const StructuredCFG &Cfg, const LoopCandidate &Candidate) {
  std::set<BlockId> BlockSet(Candidate.Blocks.begin(), Candidate.Blocks.end());
  std::vector<BlockId> Successors = collectSuccessors(Cfg, BlockSet);

  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = Candidate.Head;
  Loop.Latch = Candidate.Latch;
  if (Successors.size() == 1) {
    Loop.Follow = Successors.front();
  }
  Loop.Blocks = Candidate.Blocks;
  Loop.Successors = std::move(Successors);
  return Loop;
}

void identifyNaturalLoopChildren(const StructuredCFG &Cfg, RegionTree &Tree,
                                 RegionId RootId) {
  Region *Root = Tree.getRegion(RootId);
  if (Root == nullptr || Root->Head == InvalidBlockId) {
    return;
  }

  std::map<BlockId, std::set<BlockId>> Preds = buildPredecessors(Cfg);
  std::map<BlockId, std::set<BlockId>> Dom =
      computeDominators(Cfg, Root->Head, Preds);

  std::vector<BlockId> BlockOrder;
  BlockOrder.reserve(Cfg.blocks().size());
  for (const CFGBlock &Block : Cfg.blocks()) {
    BlockOrder.push_back(Block.Id);
  }

  std::vector<LoopCandidate> Candidates =
      filterLaminarLoops(collectLoopCandidates(Cfg, Preds, Dom, BlockOrder));
  std::map<RegionId, std::vector<BlockId>> RegionBlocks;
  std::vector<RegionId> LoopRegionIds;
  RegionBlocks[RootId] = Root->Blocks;

  for (const LoopCandidate &Candidate : Candidates) {
    Region Loop = makeLoopRegion(Cfg, Candidate);
    RegionId LoopId = Tree.addRegion(std::move(Loop));
    RegionBlocks[LoopId] = Candidate.Blocks;
    LoopRegionIds.push_back(LoopId);
  }

  for (RegionId ChildId : LoopRegionIds) {
    RegionId ParentId = RootId;
    for (RegionId CandidateParent : LoopRegionIds) {
      if (CandidateParent == ChildId) {
        continue;
      }
      if (!isSubset(RegionBlocks[ChildId], RegionBlocks[CandidateParent])) {
        continue;
      }
      if (RegionBlocks[CandidateParent].size() <
          RegionBlocks[ParentId].size()) {
        ParentId = CandidateParent;
      }
    }

    Region *Parent = Tree.getRegion(ParentId);
    if (Parent != nullptr) {
      Parent->Children.push_back(ChildId);
    }
  }
}

} // namespace

RegionTree RegionIdentifier::identifyRoot(const StructuredCFG &Cfg) {
  RegionTree Tree;

  Region Root;
  if (!Cfg.blocks().empty()) {
    Root.Head = Cfg.blocks().front().Id;
  }
  Root.Blocks.reserve(Cfg.blocks().size());
  for (const CFGBlock &Block : Cfg.blocks()) {
    Root.Blocks.push_back(Block.Id);
  }

  RegionId RootId = Tree.addRegion(std::move(Root));
  Tree.setRoot(RootId);
  identifyNaturalLoopChildren(Cfg, Tree, RootId);
  return Tree;
}

OverlayManager RegionIdentifier::identifyOverlay(const StructuredCFG &Cfg) {
  return OverlayManager(identifyRoot(Cfg), Cfg);
}

} // namespace notdec::backend::structuring
