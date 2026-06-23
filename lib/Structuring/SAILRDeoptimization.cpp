#include "notdec-backends/Structuring/SAILRDeoptimization.h"

#include <algorithm>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <vector>

namespace notdec::backend::structuring {
namespace {

bool containsBlock(const std::vector<BlockId> &Blocks, BlockId Id) {
  return std::find(Blocks.begin(), Blocks.end(), Id) != Blocks.end();
}

bool samePayload(PayloadRef Lhs, PayloadRef Rhs) { return Lhs.Id == Rhs.Id; }

bool samePayloads(const std::vector<PayloadRef> &Lhs,
                  const std::vector<PayloadRef> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!samePayload(Lhs[I], Rhs[I])) {
      return false;
    }
  }
  return true;
}

bool sameSwitchCases(const std::vector<SwitchCase> &Lhs,
                     const std::vector<SwitchCase> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!samePayload(Lhs[I].Value, Rhs[I].Value) ||
        Lhs[I].Target != Rhs[I].Target) {
      return false;
    }
  }
  return true;
}

bool sameBlockShape(const CFGBlock &Lhs, const CFGBlock &Rhs) {
  return Lhs.Terminator == Rhs.Terminator &&
         samePayload(Lhs.Condition, Rhs.Condition) &&
         Lhs.Successors == Rhs.Successors &&
         sameSwitchCases(Lhs.Cases, Rhs.Cases) &&
         samePayloads(Lhs.Statements, Rhs.Statements);
}

bool sameBlockIdentityKind(const CFGBlock &Lhs, const CFGBlock &Rhs) {
  if (Lhs.Origin != Rhs.Origin || Lhs.CopyKind != Rhs.CopyKind ||
      Lhs.CreatedBy != Rhs.CreatedBy) {
    return false;
  }
  if (Lhs.Origin == CFGBlockOrigin::Original) {
    return true;
  }
  return Lhs.SourceBlock == Rhs.SourceBlock;
}

std::vector<BlockId> predecessorsOf(const StructuredCFG &Graph,
                                    BlockId Target) {
  return Graph.predecessorsOf(Target);
}

bool sameBlockSet(std::vector<BlockId> A, std::vector<BlockId> B) {
  std::sort(A.begin(), A.end());
  std::sort(B.begin(), B.end());
  return A == B;
}

struct ReturnRegion {
  BlockId Head = InvalidBlockId;
  std::vector<BlockId> Blocks;
};

struct LinearRegion {
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

bool isClosedTerminal(const CFGBlock &Block) {
  return Block.Successors.empty() &&
         (Block.Terminator == TerminatorKind::Return ||
          Block.Terminator == TerminatorKind::Unreachable);
}

bool prependTerminalForkRegion(const StructuredCFG &Graph, ReturnRegion &Region,
                               BlockId Pred, std::set<BlockId> &Seen) {
  const CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr || PredBlock->Terminator != TerminatorKind::Branch ||
      PredBlock->Successors.size() != 2) {
    return false;
  }

  std::vector<BlockId> ForkBlocks;
  for (BlockId Succ : PredBlock->Successors) {
    if (Seen.count(Succ) != 0 && Succ != Region.Head) {
      return false;
    }

    const CFGBlock *SuccBlock = Graph.getBlock(Succ);
    if (SuccBlock == nullptr || !isClosedTerminal(*SuccBlock)) {
      return false;
    }

    std::vector<BlockId> SuccPreds = predecessorsOf(Graph, Succ);
    if (SuccPreds.size() != 1 || SuccPreds.front() != Pred) {
      return false;
    }

    ForkBlocks.push_back(Succ);
  }

  Region.Head = Pred;
  for (BlockId ForkBlock : ForkBlocks) {
    if (Seen.insert(ForkBlock).second) {
      Region.Blocks.push_back(ForkBlock);
    }
  }
  Region.Blocks.push_back(Pred);
  Seen.insert(Pred);
  return true;
}

bool prependReturnTailForkRegion(const StructuredCFG &Graph,
                                 ReturnRegion &Region, BlockId Pred,
                                 std::set<BlockId> &Seen) {
  const CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr || PredBlock->Terminator != TerminatorKind::Branch ||
      PredBlock->Successors.size() != 2) {
    return false;
  }

  std::vector<BlockId> ForkBlocks;
  bool ReachesRegionHead = false;
  for (BlockId Succ : PredBlock->Successors) {
    if (Succ == Region.Head) {
      ReachesRegionHead = true;
      continue;
    }

    if (Seen.count(Succ) != 0) {
      return false;
    }

    const CFGBlock *SuccBlock = Graph.getBlock(Succ);
    if (SuccBlock == nullptr || !isClosedTerminal(*SuccBlock)) {
      return false;
    }

    std::vector<BlockId> SuccPreds = predecessorsOf(Graph, Succ);
    if (SuccPreds.size() != 1 || SuccPreds.front() != Pred) {
      return false;
    }

    ForkBlocks.push_back(Succ);
  }

  if (!ReachesRegionHead || ForkBlocks.empty()) {
    return false;
  }

  Region.Head = Pred;
  for (BlockId ForkBlock : ForkBlocks) {
    Region.Blocks.push_back(ForkBlock);
    Seen.insert(ForkBlock);
  }
  Region.Blocks.push_back(Pred);
  Seen.insert(Pred);
  return true;
}

bool collectClosedLinearReturnTail(const StructuredCFG &Graph, BlockId Head,
                                   BlockId ExpectedPred,
                                   std::set<BlockId> &Seen,
                                   std::vector<BlockId> &Blocks) {
  BlockId Current = Head;
  BlockId Pred = ExpectedPred;
  while (true) {
    if (Seen.count(Current) != 0) {
      return false;
    }

    const CFGBlock *Block = Graph.getBlock(Current);
    if (Block == nullptr) {
      return false;
    }

    std::vector<BlockId> Preds = predecessorsOf(Graph, Current);
    if (Preds.size() != 1 || Preds.front() != Pred) {
      return false;
    }

    Seen.insert(Current);
    Blocks.push_back(Current);
    if (isClosedTerminal(*Block)) {
      return true;
    }

    if (Block->Terminator != TerminatorKind::Fallthrough ||
        Block->Successors.size() != 1) {
      return false;
    }

    Pred = Current;
    Current = Block->Successors.front();
  }
}

bool prependBranchReturnRegion(const StructuredCFG &Graph, ReturnRegion &Region,
                               BlockId Pred, std::set<BlockId> &Seen) {
  const CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr || PredBlock->Terminator != TerminatorKind::Branch ||
      PredBlock->Successors.size() != 2) {
    return false;
  }

  std::set<BlockId> NewSeen = Seen;
  std::vector<BlockId> BranchBlocks;
  bool ReachesRegionHead = false;
  for (BlockId Succ : PredBlock->Successors) {
    if (Succ == Region.Head) {
      ReachesRegionHead = true;
      continue;
    }

    if (!collectClosedLinearReturnTail(Graph, Succ, Pred, NewSeen,
                                       BranchBlocks)) {
      return false;
    }
  }

  if (!ReachesRegionHead || BranchBlocks.empty()) {
    return false;
  }

  Region.Head = Pred;
  Region.Blocks.insert(Region.Blocks.end(), BranchBlocks.begin(),
                       BranchBlocks.end());
  Region.Blocks.push_back(Pred);
  Seen = std::move(NewSeen);
  Seen.insert(Pred);
  return true;
}

ReturnRegion findLinearReturnRegion(const StructuredCFG &Graph,
                                    BlockId ReturnBlock,
                                    bool AllowBranchReturnRegion) {
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
    if (prependTerminalForkRegion(Graph, Region, Pred, Seen)) {
      continue;
    }
    if (prependReturnTailForkRegion(Graph, Region, Pred, Seen)) {
      continue;
    }
    if (AllowBranchReturnRegion &&
        prependBranchReturnRegion(Graph, Region, Pred, Seen)) {
      continue;
    }

    // Switch regions and branch regions without payload rewrite coverage still
    // need richer shared semantics before they can be copied safely.
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

bool disjointPredecessorSets(const StructuredCFG &Graph, BlockId Lhs,
                             BlockId Rhs) {
  std::vector<BlockId> LhsPreds = predecessorsOf(Graph, Lhs);
  std::vector<BlockId> RhsPreds = predecessorsOf(Graph, Rhs);
  for (BlockId Pred : LhsPreds) {
    if (containsBlock(RhsPreds, Pred)) {
      return false;
    }
  }
  return true;
}

// Group selected predecessors by connectivity so one copied return tail can
// be shared by all predecessors in the same component instead of duplicating
// the same linear tail twice.
std::vector<std::vector<BlockId>>
connectedPredecessorComponents(const StructuredCFG &Graph,
                               const std::vector<BlockId> &Blocks) {
  std::set<BlockId> NodeSet(Blocks.begin(), Blocks.end());
  std::set<BlockId> Visited;
  std::vector<std::vector<BlockId>> Components;

  for (BlockId Seed : Blocks) {
    if (Visited.count(Seed) != 0) {
      continue;
    }

    std::vector<BlockId> Stack = {Seed};
    std::vector<BlockId> Component;
    while (!Stack.empty()) {
      BlockId Current = Stack.back();
      Stack.pop_back();
      if (Visited.count(Current) != 0) {
        continue;
      }
      Visited.insert(Current);
      Component.push_back(Current);

      for (BlockId Pred : predecessorsOf(Graph, Current)) {
        if (NodeSet.count(Pred) != 0 && Visited.count(Pred) == 0) {
          Stack.push_back(Pred);
        }
      }

      for (BlockId Succ : Graph.successorsOf(Current)) {
        if (NodeSet.count(Succ) != 0 && Visited.count(Succ) == 0) {
          Stack.push_back(Succ);
        }
      }
    }

    std::sort(Component.begin(), Component.end());
    Component.erase(std::unique(Component.begin(), Component.end()),
                    Component.end());
    Components.push_back(std::move(Component));
  }

  return Components;
}

bool switchCaseReachesBlock(const CFGBlock &Block, BlockId Target) {
  if (Block.Terminator != TerminatorKind::Switch) {
    return false;
  }
  for (const SwitchCase &Case : Block.Cases) {
    if (Case.Target == Target) {
      return true;
    }
  }
  return false;
}

bool gotoEdgeFromSourceOrParent(const StructuredCFG &Graph,
                                const GotoManager &Gotos, BlockId Source,
                                BlockId Target) {
  if (Gotos.isGotoEdge(Source, Target)) {
    return true;
  }

  // Angr's low return duplicator is willing to look one step above the source
  // when an adjacent block is the real goto source. Keep the same shared CFG
  // rule here so we do not miss the narrow if-adjacent shape.
  for (BlockId Parent : predecessorsOf(Graph, Source)) {
    const CFGBlock *ParentBlock = Graph.getBlock(Parent);
    if (ParentBlock != nullptr &&
        ParentBlock->Terminator == TerminatorKind::Branch) {
      continue;
    }
    if (Gotos.isGotoEdge(Parent, Target)) {
      return true;
    }
  }

  // Angr also handles Phoenix loop-region cases where the tested edge enters a
  // return region, but the actual goto appears on the region's linear tail.
  std::set<BlockId> Seen;
  BlockId Node = Target;
  while (Seen.insert(Node).second) {
    std::vector<BlockId> Succs = Graph.successorsOf(Node);
    if (Succs.size() != 1) {
      break;
    }

    BlockId Succ = Succs.front();
    if (Succ == Node) {
      break;
    }

    std::vector<BlockId> SuccPreds = predecessorsOf(Graph, Succ);
    if (SuccPreds.size() != 1) {
      break;
    }

    if (Gotos.isGotoEdge(Node, Succ)) {
      return true;
    }
    Node = Succ;
  }

  return false;
}

BlockId defaultSwitchSuccessor(const StructuredCFG &Graph,
                              const CFGBlock &Block) {
  if (Block.Terminator != TerminatorKind::Switch) {
    return InvalidBlockId;
  }
  std::vector<BlockId> Succs = Graph.successorsOf(Block.Id);
  if (Succs.empty()) {
    return InvalidBlockId;
  }
  return Succs.front();
}

bool replaceDefaultSwitchSuccessor(StructuredCFG &Graph, BlockId SwitchId,
                                   BlockId OldTarget, BlockId NewTarget) {
  CFGBlock *Block = Graph.getBlock(SwitchId);
  if (Block == nullptr || Block->Terminator != TerminatorKind::Switch ||
      Block->Successors.empty() || Block->Successors.front() != OldTarget) {
    return false;
  }

  Block->Successors.front() = NewTarget;
  return true;
}

bool reachesBlock(const StructuredCFG &Graph, BlockId Start, BlockId Target) {
  std::set<BlockId> Seen;
  std::vector<BlockId> Worklist = {Start};
  while (!Worklist.empty()) {
    BlockId Current = Worklist.back();
    Worklist.pop_back();
    if (!Seen.insert(Current).second) {
      continue;
    }
    if (Current == Target) {
      return true;
    }
    for (BlockId Succ : Graph.successorsOf(Current)) {
      Worklist.push_back(Succ);
    }
  }
  return false;
}

bool reachesBlockFromNonDefaultSwitchSuccessor(const StructuredCFG &Graph,
                                               const CFGBlock &Switch,
                                               BlockId DefaultTarget,
                                               BlockId Target) {
  for (BlockId Succ : Graph.successorsOf(Switch.Id)) {
    if (Succ == DefaultTarget) {
      continue;
    }
    if (reachesBlock(Graph, Succ, Target)) {
      return true;
    }
  }
  return false;
}

// Copy the whole return region once, then retarget every predecessor in the
// component to the copied head block. The shared CFG keeps the copied body
// separate from the original body through BodyBlock.
bool copyRegionForPredecessors(StructuredCFG &Graph, const ReturnRegion &Region,
                               const std::vector<BlockId> &Preds,
                               std::vector<BlockId> &OutCopies) {
  // Keep the helper transactional: a failed copy should leave the caller's
  // graph exactly as it was before we started cloning the region.
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion = Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId OriginalPred =
      Preds.size() == 1 ? Preds.front() : InvalidBlockId;
  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    if (!Graph.materializeBlockBody(Copy, OriginalPred, OriginalPred)) {
      for (const auto &[_, OldCopy] : CopyRegion->Blocks) {
        Graph.removeBlock(OldCopy);
      }
      Graph = std::move(Snapshot);
      return false;
    }
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !Graph.redirectPredecessors(Region.Head, CopyHead, Preds)) {
    for (const auto &[_, Copy] : CopyRegion->Blocks) {
      Graph.removeBlock(Copy);
    }
    Graph = std::move(Snapshot);
    return false;
  }

  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    OutCopies.push_back(Copy);
  }
  return true;
}

bool appendTerminalForkRegion(const StructuredCFG &Graph, LinearRegion &Region,
                              BlockId Branch, std::set<BlockId> &Seen) {
  const CFGBlock *BranchBlock = Graph.getBlock(Branch);
  if (BranchBlock == nullptr ||
      BranchBlock->Terminator != TerminatorKind::Branch ||
      BranchBlock->Successors.size() != 2) {
    return false;
  }

  std::vector<BlockId> ForkBlocks;
  for (BlockId Succ : BranchBlock->Successors) {
    if (Seen.count(Succ) != 0) {
      return false;
    }

    const CFGBlock *SuccBlock = Graph.getBlock(Succ);
    if (SuccBlock == nullptr || !isClosedTerminal(*SuccBlock)) {
      return false;
    }

    std::vector<BlockId> SuccPreds = predecessorsOf(Graph, Succ);
    if (SuccPreds.size() != 1 || SuccPreds.front() != Branch) {
      return false;
    }

    ForkBlocks.push_back(Succ);
  }

  for (BlockId ForkBlock : ForkBlocks) {
    Region.Blocks.push_back(ForkBlock);
    Seen.insert(ForkBlock);
  }
  return true;
}

LinearRegion findLinearRegionFromHead(const StructuredCFG &Graph, BlockId Head) {
  LinearRegion Region;
  const CFGBlock *HeadBlock = Graph.getBlock(Head);
  if (HeadBlock == nullptr) {
    return Region;
  }

  Region.Head = Head;
  Region.Blocks.push_back(Head);

  std::set<BlockId> Seen;
  Seen.insert(Head);
  while (true) {
    const CFGBlock *TailBlock = Graph.getBlock(Region.Blocks.back());
    if (TailBlock == nullptr || TailBlock->Successors.size() != 1) {
      break;
    }

    BlockId Succ = TailBlock->Successors.front();
    if (Seen.count(Succ) != 0) {
      break;
    }

    const CFGBlock *SuccBlock = Graph.getBlock(Succ);
    if (SuccBlock == nullptr || predecessorsOf(Graph, Succ).size() != 1) {
      break;
    }
    if (SuccBlock->Terminator == TerminatorKind::Switch) {
      break;
    }

    Region.Blocks.push_back(Succ);
    Seen.insert(Succ);
  }

  return Region;
}

LinearRegion findLinearCopyRegion(const StructuredCFG &Graph, BlockId Head) {
  LinearRegion Region;
  const CFGBlock *HeadBlock = Graph.getBlock(Head);
  if (HeadBlock == nullptr) {
    return Region;
  }

  Region.Head = Head;
  Region.Blocks.push_back(Head);

  std::set<BlockId> Seen;
  Seen.insert(Head);
  while (true) {
    const CFGBlock *TailBlock = Graph.getBlock(Region.Blocks.back());
    if (TailBlock == nullptr) {
      break;
    }
    if (appendTerminalForkRegion(Graph, Region, TailBlock->Id, Seen)) {
      break;
    }
    if (TailBlock->Successors.size() != 1) {
      break;
    }

    BlockId Succ = TailBlock->Successors.front();
    if (Seen.count(Succ) != 0) {
      break;
    }

    const CFGBlock *SuccBlock = Graph.getBlock(Succ);
    if (SuccBlock == nullptr || predecessorsOf(Graph, Succ).size() != 1) {
      break;
    }
    if (SuccBlock->Terminator == TerminatorKind::Switch) {
      break;
    }

    Region.Blocks.push_back(Succ);
    Seen.insert(Succ);
  }

  return Region;
}

bool copyLinearRegionForPredecessors(StructuredCFG &Graph,
                                     const LinearRegion &Region,
                                     const std::vector<BlockId> &Preds,
                                     std::vector<BlockId> &OutCopies) {
  // Keep the helper transactional: a failed copy should leave the caller's
  // graph exactly as it was before we started cloning the region.
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion = Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId OriginalPred =
      Preds.size() == 1 ? Preds.front() : InvalidBlockId;
  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    if (!Graph.materializeBlockBody(Copy, OriginalPred, OriginalPred)) {
      for (const auto &[_, OldCopy] : CopyRegion->Blocks) {
        Graph.removeBlock(OldCopy);
      }
      Graph = std::move(Snapshot);
      return false;
    }
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !Graph.redirectPredecessors(Region.Head, CopyHead, Preds)) {
    for (const auto &[_, Copy] : CopyRegion->Blocks) {
      Graph.removeBlock(Copy);
    }
    Graph = std::move(Snapshot);
    return false;
  }

  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    OutCopies.push_back(Copy);
  }
  return true;
}

std::size_t statementCountInRegion(const StructuredCFG &Graph,
                                   const LinearRegion &Region) {
  std::size_t Count = 0;
  for (BlockId Id : Region.Blocks) {
    const CFGBlock *Block = Graph.getBlock(Id);
    if (Block == nullptr) {
      return std::numeric_limits<std::size_t>::max();
    }
    Count += Block->Statements.size();
  }
  return Count;
}

} // namespace

StructuringOptimizationOptions SwitchReusedEntryRewriter::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool SwitchReusedEntryRewriter::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  std::vector<BlockId> EntryIds;
  EntryIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Entry : Graph.blocks()) {
    EntryIds.push_back(Entry.Id);
  }

  std::map<BlockId, std::vector<BlockId>> SwitchPredsByEntry;
  for (BlockId EntryId : EntryIds) {
    const CFGBlock *Entry = Graph.getBlock(EntryId);
    if (Entry == nullptr) {
      continue;
    }

    std::vector<BlockId> SwitchPreds;
    for (const CFGBlock &PredBlock : Graph.blocks()) {
      if (switchCaseReachesBlock(PredBlock, EntryId)) {
        SwitchPreds.push_back(PredBlock.Id);
      }
    }

    if (SwitchPreds.size() <= 1) {
      continue;
    }

    std::sort(SwitchPreds.begin(), SwitchPreds.end());
    SwitchPreds.erase(std::unique(SwitchPreds.begin(), SwitchPreds.end()),
                      SwitchPreds.end());
    if (SwitchPreds.size() > MaxEntryReuseCount) {
      return false;
    }
    SwitchPredsByEntry.emplace(EntryId, std::move(SwitchPreds));
  }

  if (SwitchPredsByEntry.size() > MaxReusedEntries) {
    return false;
  }

  bool Changed = false;
  for (const auto &[EntryId, SwitchPreds] : SwitchPredsByEntry) {
    const CFGBlock *Entry = Graph.getBlock(EntryId);
    if (Entry == nullptr) {
      continue;
    }

    if (Graph.successorsOf(EntryId).empty()) {
      continue;
    }

    LinearRegion Region = findLinearRegionFromHead(Graph, EntryId);
    if (Region.Head == InvalidBlockId) {
      continue;
    }

    std::vector<BlockId> PredsToUpdate;
    for (BlockId Pred : SwitchPreds) {
      if (Pred == SwitchPreds.front()) {
        continue;
      }
      PredsToUpdate.push_back(Pred);
    }

    std::vector<std::vector<BlockId>> PredComponents =
        connectedPredecessorComponents(Graph, PredsToUpdate);
    for (const std::vector<BlockId> &Component : PredComponents) {
      bool AllStillReachEntry = true;
      for (BlockId Pred : Component) {
        const CFGBlock *PredBlock = Graph.getBlock(Pred);
        if (PredBlock == nullptr || !switchCaseReachesBlock(*PredBlock, EntryId)) {
          AllStillReachEntry = false;
          break;
        }
      }
      if (!AllStillReachEntry) {
        continue;
      }

      std::vector<BlockId> Copies;
      if (!copyLinearRegionForPredecessors(Graph, Region, Component, Copies)) {
        continue;
      }

      Changed = true;
    }
  }

  return Changed;
}

StructuringOptimizationOptions LoweredSwitchSimplifier::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool LoweredSwitchSimplifier::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  std::vector<BlockId> TargetIds;
  TargetIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    TargetIds.push_back(Block.Id);
  }

  bool Changed = false;
  for (BlockId TargetId : TargetIds) {
    const CFGBlock *Target = Graph.getBlock(TargetId);
    if (Target == nullptr) {
      continue;
    }

    std::vector<BlockId> Preds;
    for (const CFGBlock &PredBlock : Graph.blocks()) {
      if (PredBlock.Terminator != TerminatorKind::Switch ||
          !switchCaseReachesBlock(PredBlock, TargetId)) {
        continue;
      }
      Preds.push_back(PredBlock.Id);
    }

    if (Preds.size() <= 1) {
      continue;
    }

    std::vector<BlockId> AllPreds = predecessorsOf(Graph, TargetId);
    bool AllSwitchPreds = true;
    for (BlockId Pred : AllPreds) {
      const CFGBlock *PredBlock = Graph.getBlock(Pred);
      if (PredBlock == nullptr || PredBlock->Terminator != TerminatorKind::Switch) {
        AllSwitchPreds = false;
        break;
      }
    }
    if (!AllSwitchPreds) {
      continue;
    }

    LinearRegion Region = findLinearCopyRegion(Graph, TargetId);
    if (Region.Blocks.size() <= 1) {
      continue;
    }

    StructuredCFG Candidate = Graph;
    std::vector<std::vector<BlockId>> PredComponents =
        connectedPredecessorComponents(Graph, Preds);
    std::vector<BlockId> UpdatedPreds;
    std::vector<BlockId> Copies;
    bool DeleteOriginal = sameBlockSet(AllPreds, Preds);
    if (DeleteOriginal) {
      StructuredCFG DeletionProbe = Candidate;
      if (!DeletionProbe.removeBlocks(Region.Blocks)) {
        continue;
      }
    }
    for (const std::vector<BlockId> &Component : PredComponents) {
      if (!copyLinearRegionForPredecessors(Candidate, Region, Component,
                                           Copies)) {
        continue;
      }
      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(), Component.end());
    }

    if (DeleteOriginal && sameBlockSet(AllPreds, UpdatedPreds)) {
      if (!Candidate.removeBlocks(Region.Blocks)) {
        return false;
      }
    }

    if (!Copies.empty()) {
      Graph = std::move(Candidate);
      Changed = true;
    }
  }

  return Changed;
}

StructuringOptimizationOptions SwitchDefaultCaseDuplicator::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool SwitchDefaultCaseDuplicator::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  std::map<BlockId, std::vector<BlockId>> SwitchPredsByDefault;
  std::map<BlockId, BlockId> KeepPredForDefault;
  std::map<BlockId, LinearRegion> DefaultRegions;

  for (const CFGBlock &Block : Graph.blocks()) {
    BlockId DefaultTarget = defaultSwitchSuccessor(Graph, Block);
    if (DefaultTarget == InvalidBlockId) {
      continue;
    }

    const CFGBlock *DefaultBlock = Graph.getBlock(DefaultTarget);
    if (DefaultBlock == nullptr ||
        Graph.successorsOf(DefaultTarget).size() != 1) {
      continue;
    }
    SwitchPredsByDefault[DefaultTarget].push_back(Block.Id);

    std::vector<BlockId> Preds = predecessorsOf(Graph, DefaultTarget);
    if (Preds.size() <= 1) {
      continue;
    }

    LinearRegion Region = findLinearRegionFromHead(Graph, DefaultTarget);
    if (Region.Head == InvalidBlockId) {
      continue;
    }

    KeepPredForDefault.emplace(DefaultTarget, Block.Id);
    DefaultRegions.emplace(DefaultTarget, std::move(Region));
  }

  bool Changed = false;
  for (const auto &[DefaultTarget, SwitchPreds] : SwitchPredsByDefault) {
    if (SwitchPreds.size() <= 1) {
      continue;
    }

    StructuredCFG Candidate = Graph;
    bool RewroteDefault = false;
    bool Failed = false;
    for (BlockId SwitchPred : SwitchPreds) {
      if (!Candidate.hasEdge(SwitchPred, DefaultTarget)) {
        Failed = true;
        break;
      }

      BlockId Forwarder = Candidate.createSyntheticBlock(
          {DefaultTarget}, CFGBlockCreator::SAILRDeoptimization);
      if (Forwarder == InvalidBlockId ||
          !replaceDefaultSwitchSuccessor(Candidate, SwitchPred, DefaultTarget,
                                         Forwarder)) {
        if (Forwarder != InvalidBlockId) {
          Candidate.removeBlock(Forwarder);
        }
        Failed = true;
        break;
      }

      RewroteDefault = true;
    }

    if (RewroteDefault && !Failed) {
      Graph = std::move(Candidate);
      Changed = true;
    }
  }

  for (const auto &[DefaultTarget, KeepPred] : KeepPredForDefault) {
    const CFGBlock *KeepSwitch = Graph.getBlock(KeepPred);
    std::vector<BlockId> Preds = predecessorsOf(Graph, DefaultTarget);
    std::vector<BlockId> PredsToUpdate;
    for (BlockId Pred : Preds) {
      if (Pred == KeepPred) {
        continue;
      }
      if (KeepSwitch != nullptr &&
          reachesBlockFromNonDefaultSwitchSuccessor(Graph, *KeepSwitch,
                                                    DefaultTarget, Pred)) {
        continue;
      }
      PredsToUpdate.push_back(Pred);
    }
    if (PredsToUpdate.empty()) {
      continue;
    }

    auto RegionIt = DefaultRegions.find(DefaultTarget);
    if (RegionIt == DefaultRegions.end()) {
      continue;
    }

    const LinearRegion &Region = RegionIt->second;

    StructuredCFG Candidate = Graph;
    std::vector<BlockId> Copies;
    std::vector<std::vector<BlockId>> PredComponents =
        connectedPredecessorComponents(Graph, PredsToUpdate);
    bool Failed = false;
    for (const std::vector<BlockId> &Component : PredComponents) {
      if (!copyLinearRegionForPredecessors(Candidate, Region, Component,
                                           Copies)) {
        Failed = true;
        break;
      }
    }

    if (Failed) {
      continue;
    }

    if (!Copies.empty()) {
      Graph = std::move(Candidate);
      Changed = true;
    }
  }

  return Changed;
}

StructuringOptimizationOptions DuplicationReverter::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 4;
  return Options;
}

bool DuplicationReverter::runOnGraph(StructuredCFG &Graph,
                                     const StructuringEvaluation &Current) {
  (void)Current;

  std::vector<BlockId> BlockIds;
  BlockIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    BlockIds.push_back(Block.Id);
  }
  std::sort(BlockIds.begin(), BlockIds.end());

  for (std::size_t I = 0; I < BlockIds.size(); ++I) {
    const CFGBlock *Keep = Graph.getBlock(BlockIds[I]);
    if (Keep == nullptr) {
      continue;
    }

    for (std::size_t J = I + 1; J < BlockIds.size(); ++J) {
      BlockId DropId = BlockIds[J];
      const CFGBlock *Drop = Graph.getBlock(DropId);
      if (Drop == nullptr || !sameBlockIdentityKind(*Keep, *Drop) ||
          !sameBlockShape(*Keep, *Drop)) {
        continue;
      }
      if (Graph.hasEdge(Keep->Id, DropId) || Graph.hasEdge(DropId, Keep->Id)) {
        continue;
      }
      if (!disjointPredecessorSets(Graph, Keep->Id, DropId)) {
        continue;
      }

      std::vector<BlockId> DropPreds = predecessorsOf(Graph, DropId);
      if (DropPreds.empty()) {
        continue;
      }

      StructuredCFG Candidate = Graph;
      if (!Candidate.redirectPredecessors(DropId, Keep->Id, DropPreds) ||
          !Candidate.removeBlock(DropId)) {
        continue;
      }

      Graph = std::move(Candidate);
      return true;
    }
  }

  return false;
}

StructuringOptimizationOptions ReturnDuplicatorLow::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 4;
  return Options;
}

bool ReturnDuplicatorLow::runOnGraph(StructuredCFG &Graph,
                                     const StructuringEvaluation &Current) {
  if (Graph.blocks().size() > MaxFunctionBlocks) {
    return false;
  }

  std::map<BlockId, ReturnRegion> Regions;
  bool AllowBranchReturnRegion = Graph.hasPayloadMaterializeHook();

  for (const CFGBlock &Block : Graph.blocks()) {
    ReturnRegion Region =
        findLinearReturnRegion(Graph, Block.Id, AllowBranchReturnRegion);
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
      if (gotoEdgeFromSourceOrParent(Graph, Current.Gotos, Pred, Region.Head)) {
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
    StructuredCFG Candidate = Graph;
    if (DeleteOriginal) {
      StructuredCFG DeletionProbe = Candidate;
      if (!DeletionProbe.removeBlocks(Region.Blocks)) {
        continue;
      }
    }

    std::vector<BlockId> Copies;
    std::vector<std::vector<BlockId>> PredComponents =
        connectedPredecessorComponents(Graph, PredsToUpdate);
    std::vector<BlockId> UpdatedPreds;
    for (const std::vector<BlockId> &Component : PredComponents) {
      if (!copyRegionForPredecessors(Candidate, Region, Component, Copies)) {
        continue;
      }
      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(),
                          Component.end());
    }

    if (DeleteOriginal && UpdatedPreds.size() == CurrentPreds.size() &&
        sameBlockSet(CurrentPreds, UpdatedPreds)) {
      if (!Candidate.removeBlocks(Region.Blocks)) {
        return false;
      }
    }

    if (!Copies.empty()) {
      Graph = std::move(Candidate);
      Changed = true;
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
  std::map<BlockId, std::vector<BlockId>> PredsByTarget;
  std::map<BlockId, LinearRegion> RegionsByTarget;

  for (const CFGBlock &Block : Graph.blocks()) {
    std::vector<StructuredGoto> Gotos = Current.Gotos.gotosInBlock(Block.Id);
    if (Gotos.empty() || Gotos.size() >= 2) {
      continue;
    }

    BlockId Target = Gotos.front().Target;
    if (!Graph.hasEdge(Block.Id, Target)) {
      continue;
    }

    LinearRegion Region = findLinearRegionFromHead(Graph, Target);
    if (Region.Head == InvalidBlockId || Graph.successorsOf(Target).size() != 1) {
      continue;
    }
    if (statementCountInRegion(Graph, Region) > MaxDuplicatedStatements) {
      continue;
    }

    PredsByTarget[Target].push_back(Block.Id);
    RegionsByTarget.emplace(Target, std::move(Region));
  }

  bool Changed = false;
  for (auto &Entry : PredsByTarget) {
    BlockId Target = Entry.first;
    std::vector<BlockId> &PredsToUpdate = Entry.second;
    std::sort(PredsToUpdate.begin(), PredsToUpdate.end());
    PredsToUpdate.erase(std::unique(PredsToUpdate.begin(), PredsToUpdate.end()),
                        PredsToUpdate.end());

    auto RegionIt = RegionsByTarget.find(Target);
    if (RegionIt == RegionsByTarget.end()) {
      continue;
    }
    const LinearRegion &Region = RegionIt->second;

    std::vector<BlockId> CurrentPreds = predecessorsOf(Graph, Target);
    bool DeleteOriginal = sameBlockSet(CurrentPreds, PredsToUpdate);
    StructuredCFG Candidate = Graph;
    if (DeleteOriginal) {
      StructuredCFG DeletionProbe = Candidate;
      if (!DeletionProbe.removeBlocks(Region.Blocks)) {
        continue;
      }
    }

    std::vector<BlockId> UpdatedPreds;
    std::vector<std::vector<BlockId>> PredComponents =
        connectedPredecessorComponents(Graph, PredsToUpdate);
    for (const std::vector<BlockId> &Component : PredComponents) {
      bool AllStillReachTarget = true;
      for (BlockId Pred : Component) {
        if (!Candidate.hasEdge(Pred, Target)) {
          AllStillReachTarget = false;
          break;
        }
      }
      if (!AllStillReachTarget) {
        continue;
      }

      std::vector<BlockId> RegionCopies;
      if (!copyLinearRegionForPredecessors(Candidate, Region, Component,
                                           RegionCopies)) {
        continue;
      }

      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(),
                          Component.end());
    }

    if (DeleteOriginal && UpdatedPreds.size() == CurrentPreds.size() &&
        sameBlockSet(CurrentPreds, UpdatedPreds)) {
      if (!Candidate.removeBlocks(Region.Blocks)) {
        return false;
      }
    }

    if (!UpdatedPreds.empty()) {
      Graph = std::move(Candidate);
      Changed = true;
    }
  }

  return Changed;
}

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline() {
  StructuringOptimizationPipeline Pipeline;
  Pipeline.addPass(std::make_unique<SwitchDefaultCaseDuplicator>());
  Pipeline.addPass(std::make_unique<DuplicationReverter>());
  Pipeline.addPass(std::make_unique<LoweredSwitchSimplifier>());
  Pipeline.addPass(std::make_unique<ReturnDuplicatorLow>());
  Pipeline.addPass(std::make_unique<CrossJumpReverter>());
  Pipeline.addPass(std::make_unique<SwitchReusedEntryRewriter>());
  return Pipeline;
}

} // namespace notdec::backend::structuring
