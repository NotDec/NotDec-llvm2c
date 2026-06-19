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

  std::set<std::vector<BlockId>> SeenLoops;
  for (const CFGBlock &Latch : Cfg.blocks()) {
    for (BlockId Succ : Latch.Successors) {
      auto DomIt = Dom.find(Latch.Id);
      if (DomIt == Dom.end() || !contains(DomIt->second, Succ)) {
        continue;
      }

      std::vector<BlockId> Blocks =
          collectNaturalLoop(Succ, Latch.Id, Preds, BlockOrder);
      if (Blocks.empty() || !SeenLoops.insert(Blocks).second) {
        continue;
      }

      std::set<BlockId> BlockSet(Blocks.begin(), Blocks.end());
      std::vector<BlockId> Successors = collectSuccessors(Cfg, BlockSet);
      Region Loop;
      Loop.Kind = RegionKind::NaturalLoop;
      Loop.Head = Succ;
      Loop.Latch = Latch.Id;
      if (Successors.size() == 1) {
        Loop.Follow = Successors.front();
      }
      Loop.Blocks = std::move(Blocks);
      Loop.Successors = std::move(Successors);
      RegionId LoopId = Tree.addRegion(std::move(Loop));
      Root = Tree.getRegion(RootId);
      if (Root != nullptr) {
        Root->Children.push_back(LoopId);
      }
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

} // namespace notdec::backend::structuring
