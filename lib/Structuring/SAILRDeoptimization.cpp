#include "notdec-backends/Structuring/SAILRDeoptimization.h"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <vector>

namespace notdec::backend::structuring {
namespace {

bool containsBlock(const std::vector<BlockId> &Blocks, BlockId Id) {
  return std::find(Blocks.begin(), Blocks.end(), Id) != Blocks.end();
}

bool samePayload(const StructuredCFG &Graph, PayloadRef Lhs, PayloadRef Rhs) {
  return Graph.payloadOrigin(Lhs.Id) == Graph.payloadOrigin(Rhs.Id);
}

bool samePayloads(const StructuredCFG &Graph,
                  const std::vector<PayloadRef> &Lhs,
                  const std::vector<PayloadRef> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!samePayload(Graph, Lhs[I], Rhs[I])) {
      return false;
    }
  }
  return true;
}

bool sameSwitchCaseValues(const StructuredCFG &Graph,
                          const std::vector<SwitchCase> &Lhs,
                          const std::vector<SwitchCase> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!samePayload(Graph, Lhs[I].Value, Rhs[I].Value)) {
      return false;
    }
  }
  return true;
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

bool sameBlockReference(const StructuredCFG &Graph, BlockId Lhs, BlockId Rhs) {
  if (Lhs == Rhs) {
    return true;
  }

  const CFGBlock *LhsBlock = Graph.getBlock(Lhs);
  const CFGBlock *RhsBlock = Graph.getBlock(Rhs);
  if (LhsBlock == nullptr || RhsBlock == nullptr) {
    return false;
  }

  if (LhsBlock->Origin == CFGBlockOrigin::Copied &&
      RhsBlock->Origin == CFGBlockOrigin::Original) {
    return LhsBlock->SourceBlock == Rhs;
  }
  if (LhsBlock->Origin == CFGBlockOrigin::Original &&
      RhsBlock->Origin == CFGBlockOrigin::Copied) {
    return RhsBlock->SourceBlock == Lhs;
  }

  return LhsBlock->Origin == CFGBlockOrigin::Copied &&
         RhsBlock->Origin == CFGBlockOrigin::Copied &&
         sameBlockIdentityKind(*LhsBlock, *RhsBlock);
}

bool sameSuccessorListByReference(const StructuredCFG &Graph,
                                  const std::vector<BlockId> &Lhs,
                                  const std::vector<BlockId> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!sameBlockReference(Graph, Lhs[I], Rhs[I])) {
      return false;
    }
  }
  return true;
}

bool sameSwitchCasesByReference(const StructuredCFG &Graph,
                                const std::vector<SwitchCase> &Lhs,
                                const std::vector<SwitchCase> &Rhs) {
  if (Lhs.size() != Rhs.size()) {
    return false;
  }
  for (std::size_t I = 0; I < Lhs.size(); ++I) {
    if (!samePayload(Graph, Lhs[I].Value, Rhs[I].Value) ||
        !sameBlockReference(Graph, Lhs[I].Target, Rhs[I].Target)) {
      return false;
    }
  }
  return true;
}

bool sameBlockShapeByReference(const StructuredCFG &Graph, const CFGBlock &Lhs,
                               const CFGBlock &Rhs) {
  return Lhs.Terminator == Rhs.Terminator &&
         samePayload(Graph, Lhs.Condition, Rhs.Condition) &&
         sameSuccessorListByReference(Graph, Lhs.Successors, Rhs.Successors) &&
         sameSwitchCasesByReference(Graph, Lhs.Cases, Rhs.Cases) &&
         samePayloads(Graph, Lhs.Statements, Rhs.Statements);
}

bool sameBlockControlShapeByReference(const StructuredCFG &Graph,
                                      const CFGBlock &Lhs,
                                      const CFGBlock &Rhs) {
  return Lhs.Terminator == Rhs.Terminator &&
         samePayload(Graph, Lhs.Condition, Rhs.Condition) &&
         sameSuccessorListByReference(Graph, Lhs.Successors, Rhs.Successors) &&
         sameSwitchCasesByReference(Graph, Lhs.Cases, Rhs.Cases);
}

bool sameRegionTailShapeByReference(const StructuredCFG &Graph,
                                    const CFGBlock &Lhs,
                                    const CFGBlock &Rhs) {
  return Lhs.Terminator == Rhs.Terminator &&
         samePayload(Graph, Lhs.Condition, Rhs.Condition) &&
         Lhs.Successors.size() == Rhs.Successors.size() &&
         sameSwitchCaseValues(Graph, Lhs.Cases, Rhs.Cases) &&
         samePayloads(Graph, Lhs.Statements, Rhs.Statements);
}

struct LinearRegion;

LinearRegion findLinearRegionFromHead(const StructuredCFG &Graph, BlockId Head);

std::vector<BlockId> predecessorsOf(const StructuredCFG &Graph,
                                    BlockId Target) {
  return Graph.predecessorsOf(Target);
}

bool gotoEdgeFromSourceOrParent(const StructuredCFG &Graph,
                                const GotoManager &Gotos, BlockId Source,
                                BlockId Target);

bool reachesBlock(const StructuredCFG &Graph, BlockId Start, BlockId Target);
bool blockOnlyReachedFrom(const StructuredCFG &Graph, BlockId Target,
                          const std::set<BlockId> &AllowedPreds);

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

// A lowered switch chain is still a branch chain in the shared CFG. Keep the
// collected shape small and explicit so the rewrite does not guess renderer
// payload text.
struct LoweredSwitchIfCase {
  PayloadRef Value;
  std::optional<ConditionCompare> Compare;
  BlockId Target = InvalidBlockId;
};

struct LoweredSwitchIfChain {
  BlockId Head = InvalidBlockId;
  PayloadRef ComparedValue;
  BlockId DefaultTarget = InvalidBlockId;
  std::vector<LoweredSwitchIfCase> Cases;
  std::vector<BlockId> RemovedBlocks;
};

constexpr std::size_t MaxLinearRegionMergeBlocks = 12;
constexpr std::size_t MaxLoweredSwitchContinuousCases = 6;
constexpr std::uint64_t LoweredSwitchI32AllOnesSentinel = 0xffff'ffffULL;
constexpr std::uint64_t LoweredSwitchI64AllOnesSentinel =
    0xffff'ffff'ffff'ffffULL;

bool collectJoinedDiamondReturnRegion(const StructuredCFG &Graph,
                                      BlockId Terminal,
                                      ReturnRegion &Region);
bool collectClosedReturnTail(const StructuredCFG &Graph, BlockId Head,
                             BlockId ExpectedPred, std::set<BlockId> &Seen,
                             std::vector<BlockId> &Blocks);

const LinearRegion *cachedLinearRegion(const StructuredCFG &Graph, BlockId Head,
                                       std::map<BlockId, LinearRegion> &Cache) {
  auto It = Cache.find(Head);
  if (It != Cache.end()) {
    return &It->second;
  }

  auto Inserted = Cache.emplace(Head, findLinearRegionFromHead(Graph, Head));
  return &Inserted.first->second;
}

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

bool isClosedTerminalForkLeaf(const CFGBlock &Block) {
  return isClosedTerminal(Block) ||
         (Block.Successors.empty() &&
          Block.Terminator == TerminatorKind::Fallthrough &&
          Block.Statements.empty());
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
    if (SuccBlock == nullptr || !isClosedTerminalForkLeaf(*SuccBlock)) {
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

    if (Block->Terminator == TerminatorKind::Branch &&
        Block->Successors.size() == 2) {
      for (BlockId Succ : Block->Successors) {
        if (Seen.count(Succ) != 0) {
          return false;
        }
        const CFGBlock *SuccBlock = Graph.getBlock(Succ);
        if (SuccBlock == nullptr || !isClosedTerminal(*SuccBlock)) {
          return false;
        }
        std::vector<BlockId> SuccPreds = predecessorsOf(Graph, Succ);
        if (SuccPreds.size() != 1 || SuccPreds.front() != Current) {
          return false;
        }
      }

      for (BlockId Succ : Block->Successors) {
        Seen.insert(Succ);
        Blocks.push_back(Succ);
      }
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

bool collectFallthroughPathToClosedTerminal(
    const StructuredCFG &Graph, BlockId Start, BlockId ExpectedPred,
    const std::set<BlockId> &RegionSeen, std::vector<BlockId> &Path,
    BlockId &Terminal) {
  BlockId Current = Start;
  BlockId Pred = ExpectedPred;
  std::set<BlockId> PathSeen;
  while (true) {
    if (RegionSeen.count(Current) != 0 || !PathSeen.insert(Current).second) {
      return false;
    }

    const CFGBlock *Block = Graph.getBlock(Current);
    if (Block == nullptr) {
      return false;
    }

    Path.push_back(Current);
    if (isClosedTerminal(*Block)) {
      Terminal = Current;
      return true;
    }

    std::vector<BlockId> Preds = predecessorsOf(Graph, Current);
    if (Preds.size() != 1 || Preds.front() != Pred ||
        Block->Terminator != TerminatorKind::Fallthrough ||
        Block->Successors.size() != 1) {
      return false;
    }

    Pred = Current;
    Current = Block->Successors.front();
  }
}

BlockId terminalPredecessorForReturnPath(const std::vector<BlockId> &Path,
                                         BlockId Head) {
  if (Path.empty()) {
    return InvalidBlockId;
  }
  // A direct branch-to-return side has no private block before the terminal;
  // the branch head itself is the terminal predecessor in the region.
  return Path.size() == 1 ? Head : Path[Path.size() - 2];
}

// This is intentionally narrower than Angr's full single-entry region search:
// it only accepts a branch whose two private fallthrough paths meet at one
// closed terminal.
bool collectClosedDiamondReturnTail(const StructuredCFG &Graph, BlockId Head,
                                    BlockId ExpectedPred,
                                    std::set<BlockId> &Seen,
                                    std::vector<BlockId> &Blocks) {
  const CFGBlock *HeadBlock = Graph.getBlock(Head);
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Branch ||
      HeadBlock->Successors.size() != 2 || Seen.count(Head) != 0 ||
      HeadBlock->Successors[0] == HeadBlock->Successors[1]) {
    return false;
  }

  std::vector<BlockId> HeadPreds = predecessorsOf(Graph, Head);
  if (HeadPreds.size() != 1 || HeadPreds.front() != ExpectedPred) {
    return false;
  }

  std::vector<BlockId> LeftPath;
  std::vector<BlockId> RightPath;
  BlockId LeftTerminal = InvalidBlockId;
  BlockId RightTerminal = InvalidBlockId;
  if (!collectFallthroughPathToClosedTerminal(
          Graph, HeadBlock->Successors[0], Head, Seen, LeftPath,
          LeftTerminal) ||
      !collectFallthroughPathToClosedTerminal(
          Graph, HeadBlock->Successors[1], Head, Seen, RightPath,
          RightTerminal)) {
    return false;
  }

  if (LeftTerminal == InvalidBlockId || LeftTerminal != RightTerminal ||
      LeftPath.empty() || RightPath.empty()) {
    return false;
  }

  for (std::size_t I = 0; I + 1 < LeftPath.size(); ++I) {
    for (std::size_t J = 0; J + 1 < RightPath.size(); ++J) {
      if (LeftPath[I] == RightPath[J]) {
        return false;
      }
    }
  }

  BlockId LeftTerminalPred = terminalPredecessorForReturnPath(LeftPath, Head);
  BlockId RightTerminalPred = terminalPredecessorForReturnPath(RightPath, Head);
  if (LeftTerminalPred == RightTerminalPred) {
    return false;
  }

  std::vector<BlockId> TerminalPreds = predecessorsOf(Graph, LeftTerminal);
  if (TerminalPreds.size() != 2 ||
      !containsBlock(TerminalPreds, LeftTerminalPred) ||
      !containsBlock(TerminalPreds, RightTerminalPred)) {
    return false;
  }

  std::set<BlockId> NewSeen = Seen;
  std::vector<BlockId> NewBlocks;
  if (!NewSeen.insert(Head).second) {
    return false;
  }
  NewBlocks.push_back(Head);

  auto AppendSidePath = [&](const std::vector<BlockId> &Path) {
    for (std::size_t I = 0; I + 1 < Path.size(); ++I) {
      if (!NewSeen.insert(Path[I]).second) {
        return false;
      }
      NewBlocks.push_back(Path[I]);
    }
    return true;
  };

  if (!AppendSidePath(LeftPath) || !AppendSidePath(RightPath) ||
      !NewSeen.insert(LeftTerminal).second) {
    return false;
  }
  NewBlocks.push_back(LeftTerminal);

  Seen = std::move(NewSeen);
  Blocks.insert(Blocks.end(), NewBlocks.begin(), NewBlocks.end());
  return true;
}

// Conservative wrapper-tail support for:
//   branch -> private left/right fallthrough paths -> shared fallthrough tail
//          -> closed terminal
// The body shape is shared with collectJoinedDiamondReturnRegion(); this wrapper
// only anchors it to the side head being absorbed by a branch/switch parent.
bool collectClosedJoinedDiamondReturnTail(const StructuredCFG &Graph,
                                          BlockId Head, BlockId ExpectedPred,
                                          std::set<BlockId> &Seen,
                                          std::vector<BlockId> &Blocks) {
  std::vector<BlockId> HeadPreds = predecessorsOf(Graph, Head);
  if (HeadPreds.size() != 1 || HeadPreds.front() != ExpectedPred ||
      Seen.count(Head) != 0) {
    return false;
  }

  for (const CFGBlock &Block : Graph.blocks()) {
    if (!isClosedTerminal(Block)) {
      continue;
    }

    ReturnRegion Region;
    if (!collectJoinedDiamondReturnRegion(Graph, Block.Id, Region) ||
        Region.Head != Head) {
      continue;
    }

    std::set<BlockId> NewSeen = Seen;
    bool OverlapsSeen = false;
    for (BlockId Id : Region.Blocks) {
      if (!NewSeen.insert(Id).second) {
        OverlapsSeen = true;
        break;
      }
    }
    if (OverlapsSeen) {
      continue;
    }

    Seen = std::move(NewSeen);
    Blocks.insert(Blocks.end(), Region.Blocks.begin(), Region.Blocks.end());
    return true;
  }

  return false;
}

// Conservative switch-tail support for wrapper regions. The switch itself must
// be single-predecessor and every unique successor must already be a closed
// return tail.
bool collectClosedSwitchReturnTail(const StructuredCFG &Graph, BlockId Head,
                                   BlockId ExpectedPred,
                                   std::set<BlockId> &Seen,
                                   std::vector<BlockId> &Blocks) {
  const CFGBlock *HeadBlock = Graph.getBlock(Head);
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Switch ||
      Seen.count(Head) != 0) {
    return false;
  }

  std::vector<BlockId> Succs = Graph.successorsOf(Head);
  if (Succs.empty()) {
    return false;
  }

  std::vector<BlockId> HeadPreds = predecessorsOf(Graph, Head);
  if (HeadPreds.size() != 1 || HeadPreds.front() != ExpectedPred) {
    return false;
  }

  std::set<BlockId> NewSeen = Seen;
  std::vector<BlockId> NewBlocks = {Head};
  if (!NewSeen.insert(Head).second) {
    return false;
  }

  for (BlockId Succ : Succs) {
    if (!collectClosedReturnTail(Graph, Succ, Head, NewSeen, NewBlocks)) {
      return false;
    }
  }

  Seen = std::move(NewSeen);
  Blocks.insert(Blocks.end(), NewBlocks.begin(), NewBlocks.end());
  return true;
}

bool collectReverseFallthroughSideToBranch(
    const StructuredCFG &Graph, BlockId Start, BlockId TailHead,
    const std::set<BlockId> &TailBlocks, BlockId &Head,
    std::vector<BlockId> &ReverseSide) {
  BlockId Current = Start;
  BlockId ExpectedSucc = TailHead;
  std::set<BlockId> Seen;
  while (true) {
    if (TailBlocks.count(Current) != 0 || !Seen.insert(Current).second) {
      return false;
    }

    const CFGBlock *CurrentBlock = Graph.getBlock(Current);
    if (CurrentBlock == nullptr) {
      return false;
    }

    if (ReverseSide.empty() &&
        CurrentBlock->Terminator == TerminatorKind::Branch &&
        CurrentBlock->Successors.size() == 2 &&
        containsBlock(CurrentBlock->Successors, TailHead)) {
      Head = Current;
      return true;
    }

    if (CurrentBlock->Terminator != TerminatorKind::Fallthrough ||
        CurrentBlock->Successors.size() != 1) {
      return false;
    }
    if (ExpectedSucc != InvalidBlockId &&
        CurrentBlock->Successors.front() != ExpectedSucc) {
      return false;
    }

    std::vector<BlockId> Preds = predecessorsOf(Graph, Current);
    if (Preds.size() != 1) {
      return false;
    }
    BlockId Pred = Preds.front();
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (PredBlock == nullptr) {
      return false;
    }

    ReverseSide.push_back(Current);
    if (PredBlock->Terminator == TerminatorKind::Branch &&
        PredBlock->Successors.size() == 2) {
      Head = Pred;
      return true;
    }

    if (PredBlock->Terminator != TerminatorKind::Fallthrough ||
        PredBlock->Successors.size() != 1 ||
        PredBlock->Successors.front() != Current) {
      return false;
    }
    ExpectedSucc = Current;
    Current = Pred;
  }
}

// Conservative joined-diamond support for:
//   branch -> private left/right fallthrough paths -> shared fallthrough tail
//          -> closed terminal
// This is still much narrower than Angr's general single-entry region search:
// it rejects loops, all-direct joins, nested branch sides, and non-linear shared
// tails.
bool collectJoinedDiamondReturnRegion(const StructuredCFG &Graph,
                                      BlockId Terminal,
                                      ReturnRegion &Region) {
  const CFGBlock *TerminalBlock = Graph.getBlock(Terminal);
  if (TerminalBlock == nullptr || !isClosedTerminal(*TerminalBlock)) {
    return false;
  }

  std::vector<BlockId> ReverseTail = {Terminal};
  BlockId Current = Terminal;
  std::set<BlockId> TailSeen = {Terminal};
  while (true) {
    std::vector<BlockId> Preds = predecessorsOf(Graph, Current);
    if (Preds.size() != 1) {
      break;
    }

    BlockId Pred = Preds.front();
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (PredBlock == nullptr ||
        PredBlock->Terminator != TerminatorKind::Fallthrough ||
        PredBlock->Successors.size() != 1 ||
        PredBlock->Successors.front() != Current ||
        !TailSeen.insert(Pred).second) {
      return false;
    }

    ReverseTail.push_back(Pred);
    Current = Pred;
  }

  BlockId Join = Current;
  if (Join == Terminal || ReverseTail.size() < 2) {
    return false;
  }

  std::vector<BlockId> JoinPreds = predecessorsOf(Graph, Join);
  if (JoinPreds.size() != 2 || JoinPreds[0] == JoinPreds[1]) {
    return false;
  }

  BlockId LeftHead = InvalidBlockId;
  BlockId RightHead = InvalidBlockId;
  std::vector<BlockId> LeftReverse;
  std::vector<BlockId> RightReverse;
  std::set<BlockId> TailBlocks(ReverseTail.begin(), ReverseTail.end());
  if (!collectReverseFallthroughSideToBranch(Graph, JoinPreds[0], Join,
                                             TailBlocks, LeftHead,
                                             LeftReverse) ||
      !collectReverseFallthroughSideToBranch(Graph, JoinPreds[1], Join,
                                             TailBlocks, RightHead,
                                             RightReverse) ||
      LeftHead == InvalidBlockId || LeftHead != RightHead) {
    return false;
  }

  const CFGBlock *HeadBlock = Graph.getBlock(LeftHead);
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Branch ||
      HeadBlock->Successors.size() != 2 ||
      (LeftReverse.empty() && RightReverse.empty())) {
    return false;
  }

  std::vector<BlockId> LeftForward(LeftReverse.rbegin(), LeftReverse.rend());
  std::vector<BlockId> RightForward(RightReverse.rbegin(), RightReverse.rend());
  if ((!LeftForward.empty() &&
       !containsBlock(HeadBlock->Successors, LeftForward.front())) ||
      (!RightForward.empty() &&
       !containsBlock(HeadBlock->Successors, RightForward.front())) ||
      (!LeftForward.empty() && !RightForward.empty() &&
       LeftForward.front() == RightForward.front())) {
    return false;
  }

  std::set<BlockId> RegionSeen;
  auto AddUnique = [&](BlockId Id, std::vector<BlockId> &Out) {
    if (!RegionSeen.insert(Id).second) {
      return false;
    }
    Out.push_back(Id);
    return true;
  };

  std::vector<BlockId> Blocks;
  if (!AddUnique(LeftHead, Blocks)) {
    return false;
  }
  for (BlockId Id : LeftForward) {
    if (!AddUnique(Id, Blocks)) {
      return false;
    }
  }
  for (BlockId Id : RightForward) {
    if (!AddUnique(Id, Blocks)) {
      return false;
    }
  }
  for (auto It = ReverseTail.rbegin(); It != ReverseTail.rend(); ++It) {
    if (!AddUnique(*It, Blocks)) {
      return false;
    }
  }

  Region.Head = LeftHead;
  Region.Blocks = std::move(Blocks);
  return true;
}

bool collectClosedReturnTail(const StructuredCFG &Graph, BlockId Head,
                             BlockId ExpectedPred, std::set<BlockId> &Seen,
                             std::vector<BlockId> &Blocks) {
  // Try each shape transactionally. Some helpers may touch Seen before they
  // discover a later mismatch.
  std::set<BlockId> CandidateSeen = Seen;
  std::vector<BlockId> CandidateBlocks;
  if (collectClosedLinearReturnTail(Graph, Head, ExpectedPred, CandidateSeen,
                                    CandidateBlocks)) {
    Seen = std::move(CandidateSeen);
    Blocks.insert(Blocks.end(), CandidateBlocks.begin(),
                  CandidateBlocks.end());
    return true;
  }

  CandidateSeen = Seen;
  CandidateBlocks.clear();
  if (collectClosedDiamondReturnTail(Graph, Head, ExpectedPred, CandidateSeen,
                                     CandidateBlocks)) {
    Seen = std::move(CandidateSeen);
    Blocks.insert(Blocks.end(), CandidateBlocks.begin(),
                  CandidateBlocks.end());
    return true;
  }

  CandidateSeen = Seen;
  CandidateBlocks.clear();
  if (collectClosedJoinedDiamondReturnTail(Graph, Head, ExpectedPred,
                                           CandidateSeen, CandidateBlocks)) {
    Seen = std::move(CandidateSeen);
    Blocks.insert(Blocks.end(), CandidateBlocks.begin(),
                  CandidateBlocks.end());
    return true;
  }

  CandidateSeen = Seen;
  CandidateBlocks.clear();
  if (collectClosedSwitchReturnTail(Graph, Head, ExpectedPred, CandidateSeen,
                                    CandidateBlocks)) {
    Seen = std::move(CandidateSeen);
    Blocks.insert(Blocks.end(), CandidateBlocks.begin(),
                  CandidateBlocks.end());
    return true;
  }

  return false;
}

struct DiamondReturnSide {
  BlockId Head = InvalidBlockId;
  std::vector<BlockId> Blocks;
};

bool collectDiamondReturnSide(const StructuredCFG &Graph, BlockId Start,
                              std::set<BlockId> &Seen,
                              DiamondReturnSide &Side) {
  BlockId Current = Start;
  while (true) {
    const CFGBlock *Block = Graph.getBlock(Current);
    if (Block == nullptr) {
      return false;
    }

    if (Block->Terminator == TerminatorKind::Branch) {
      if (Block->Successors.size() != 2) {
        return false;
      }
      Side.Head = Current;
      return true;
    }

    if (Block->Terminator != TerminatorKind::Fallthrough ||
        Block->Successors.size() != 1) {
      return false;
    }

    if (!Seen.insert(Current).second) {
      return false;
    }

    Side.Blocks.push_back(Current);
    std::vector<BlockId> Preds = predecessorsOf(Graph, Current);
    if (Preds.size() != 1) {
      return false;
    }
    Current = Preds.front();
  }
}

bool collectDiamondReturnRegion(const StructuredCFG &Graph, BlockId Terminal,
                               ReturnRegion &Region) {
  const CFGBlock *TerminalBlock = Graph.getBlock(Terminal);
  if (TerminalBlock == nullptr || !isClosedTerminal(*TerminalBlock)) {
    return false;
  }

  std::vector<BlockId> TerminalPreds = predecessorsOf(Graph, Terminal);
  if (TerminalPreds.size() != 2) {
    return false;
  }

  std::set<BlockId> Seen = {Terminal};
  std::vector<DiamondReturnSide> Sides;
  Sides.reserve(2);
  for (BlockId Pred : TerminalPreds) {
    DiamondReturnSide Side;
    if (!collectDiamondReturnSide(Graph, Pred, Seen, Side)) {
      return false;
    }
    Sides.push_back(std::move(Side));
  }

  if (Sides[0].Head == InvalidBlockId || Sides[0].Head != Sides[1].Head) {
    return false;
  }

  const CFGBlock *HeadBlock = Graph.getBlock(Sides[0].Head);
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Branch ||
      HeadBlock->Successors.size() != 2) {
    return false;
  }
  bool SawSideBlock = false;
  bool SawDirectSide = false;
  for (const DiamondReturnSide &Side : Sides) {
    if (!Side.Blocks.empty()) {
      SawSideBlock = true;
      if (!containsBlock(HeadBlock->Successors, Side.Blocks.back())) {
        return false;
      }
      continue;
    }

    SawDirectSide = true;
    if (!containsBlock(HeadBlock->Successors, Terminal)) {
      return false;
    }
  }
  if (!SawSideBlock) {
    return false;
  }
  // Do not absorb a component entry as the diamond head. Direct-return sides
  // are only safe when the head is already inside a shared return region.
  if (SawDirectSide && predecessorsOf(Graph, Sides[0].Head).empty()) {
    return false;
  }

  Region.Head = Sides[0].Head;
  Region.Blocks.push_back(Region.Head);
  for (const DiamondReturnSide &Side : Sides) {
    for (auto It = Side.Blocks.rbegin(); It != Side.Blocks.rend(); ++It) {
      Region.Blocks.push_back(*It);
    }
  }
  Region.Blocks.push_back(Terminal);
  return true;
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

    if (!collectClosedReturnTail(Graph, Succ, Pred, NewSeen, BranchBlocks)) {
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

bool prependSwitchReturnRegion(const StructuredCFG &Graph, ReturnRegion &Region,
                               BlockId Pred, std::set<BlockId> &Seen) {
  const CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr || PredBlock->Terminator != TerminatorKind::Switch ||
      PredBlock->Successors.empty()) {
    return false;
  }

  std::set<BlockId> NewSeen = Seen;
  std::vector<BlockId> SwitchBlocks;
  bool ReachesRegionHead = false;
  for (BlockId Succ : Graph.successorsOf(Pred)) {
    if (Succ == Region.Head) {
      ReachesRegionHead = true;
      continue;
    }

    if (!collectClosedReturnTail(Graph, Succ, Pred, NewSeen, SwitchBlocks)) {
      return false;
    }
  }

  if (!ReachesRegionHead || SwitchBlocks.empty()) {
    return false;
  }

  Region.Head = Pred;
  Region.Blocks.insert(Region.Blocks.end(), SwitchBlocks.begin(),
                       SwitchBlocks.end());
  Region.Blocks.push_back(Pred);
  Seen = std::move(NewSeen);
  Seen.insert(Pred);
  return true;
}

ReturnRegion findLinearReturnRegion(const StructuredCFG &Graph,
                                    BlockId ReturnBlock,
                                    bool AllowComplexReturnRegion) {
  ReturnRegion Region;
  const CFGBlock *EndBlock = Graph.getBlock(ReturnBlock);
  if (EndBlock == nullptr || !isClosedTerminal(*EndBlock)) {
    return Region;
  }

  if (AllowComplexReturnRegion) {
    if (collectDiamondReturnRegion(Graph, ReturnBlock, Region)) {
      // The prepend loop below stores blocks tail-to-head until the final
      // reverse. Convert the diamond result so simple wrappers above the
      // diamond can be absorbed by the same code path.
      std::reverse(Region.Blocks.begin(), Region.Blocks.end());
    } else if (collectJoinedDiamondReturnRegion(Graph, ReturnBlock, Region)) {
      // Keep the same tail-to-head convention as collectDiamondReturnRegion()
      // before the prepend loop below.
      std::reverse(Region.Blocks.begin(), Region.Blocks.end());
    }
  }

  std::set<BlockId> Seen;
  for (BlockId Id : Region.Blocks) {
    Seen.insert(Id);
  }
  if (Region.Head == InvalidBlockId) {
    Region.Head = ReturnBlock;
    Region.Blocks.push_back(ReturnBlock);
    Seen.insert(ReturnBlock);
  }

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
    if (AllowComplexReturnRegion &&
        prependBranchReturnRegion(Graph, Region, Pred, Seen)) {
      continue;
    }
    if (AllowComplexReturnRegion &&
        prependSwitchReturnRegion(Graph, Region, Pred, Seen)) {
      continue;
    }

    // Complex regions without payload rewrite coverage still need richer shared
    // semantics before they can be copied safely.
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

bool sameBranchPredecessorPair(const StructuredCFG &Graph, BlockId Lhs,
                               BlockId Rhs, BlockId BranchSource) {
  std::vector<BlockId> LhsPreds = predecessorsOf(Graph, Lhs);
  std::vector<BlockId> RhsPreds = predecessorsOf(Graph, Rhs);
  if (LhsPreds.size() != 1 || RhsPreds.size() != 1 ||
      LhsPreds.front() != RhsPreds.front() ||
      LhsPreds.front() != BranchSource) {
    return false;
  }

  const CFGBlock *Pred = Graph.getBlock(BranchSource);
  if (Pred == nullptr || Pred->Terminator != TerminatorKind::Branch ||
      Pred->Successors.size() != 2) {
    return false;
  }

  return (Pred->Successors[0] == Lhs && Pred->Successors[1] == Rhs) ||
         (Pred->Successors[0] == Rhs && Pred->Successors[1] == Lhs);
}

std::optional<BlockId> privateBranchSibling(const StructuredCFG &Graph,
                                            BlockId Arm) {
  std::vector<BlockId> Preds = predecessorsOf(Graph, Arm);
  if (Preds.size() != 1) {
    return std::nullopt;
  }

  const CFGBlock *Pred = Graph.getBlock(Preds.front());
  if (Pred == nullptr || Pred->Terminator != TerminatorKind::Branch ||
      Pred->Successors.size() != 2) {
    return std::nullopt;
  }

  if (Pred->Successors[0] == Arm) {
    return Pred->Successors[1];
  }
  if (Pred->Successors[1] == Arm) {
    return Pred->Successors[0];
  }
  return std::nullopt;
}

bool hasGotoTouchingBranchPair(const GotoManager &Gotos, BlockId Branch,
                               BlockId Lhs, BlockId Rhs) {
  for (const StructuredGoto &Goto : Gotos.gotos()) {
    if (Goto.Source == Branch &&
        (Goto.Target == Lhs || Goto.Target == Rhs)) {
      return true;
    }
    if ((Goto.Source == Lhs || Goto.Source == Rhs) &&
        (Goto.Target == Lhs || Goto.Target == Rhs ||
         Goto.Target == InvalidBlockId)) {
      return true;
    }
  }
  return false;
}

bool canMergePrivateBranchArm(const StructuredCFG &Graph, BlockId Branch,
                              BlockId Arm, const CFGBlock &Reference) {
  const CFGBlock *Block = Graph.getBlock(Arm);
  return Block != nullptr &&
         sameBranchPredecessorPair(Graph, Reference.Id, Arm, Branch) &&
         sameBlockIdentityKind(Reference, *Block) &&
         sameBlockShapeByReference(Graph, Reference, *Block);
}

bool canDiscardBranchCondition(const StructuredCFG &Graph,
                               const CFGBlock &Branch) {
  if (!Branch.Condition.isValid()) {
    return true;
  }
  return Graph.conditionCompare(Branch.Condition).has_value();
}

bool revertGotoRelatedDuplicateBranchArms(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  if (Current.Gotos.empty()) {
    return false;
  }

  std::vector<BlockId> BlockIds;
  BlockIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    BlockIds.push_back(Block.Id);
  }
  std::sort(BlockIds.begin(), BlockIds.end());

  for (BlockId BranchId : BlockIds) {
    const CFGBlock *Branch = Graph.getBlock(BranchId);
    if (Branch == nullptr || Branch->Terminator != TerminatorKind::Branch ||
        Branch->Successors.size() != 2 ||
        Branch->Successors[0] == Branch->Successors[1] ||
        !canDiscardBranchCondition(Graph, *Branch)) {
      continue;
    }

    BlockId KeepId = std::min(Branch->Successors[0], Branch->Successors[1]);
    BlockId DropId = std::max(Branch->Successors[0], Branch->Successors[1]);
    const CFGBlock *Keep = Graph.getBlock(KeepId);
    if (Keep == nullptr ||
        !hasGotoTouchingBranchPair(Current.Gotos, BranchId, KeepId, DropId) ||
        !canMergePrivateBranchArm(Graph, BranchId, DropId, *Keep)) {
      continue;
    }

    StructuredCFG Candidate = Graph;
    CFGBlock *CandidateBranch = Candidate.getBlock(BranchId);
    if (CandidateBranch == nullptr) {
      continue;
    }
    CandidateBranch->Terminator = TerminatorKind::Fallthrough;
    CandidateBranch->Condition = {};
    CandidateBranch->Successors = {KeepId};
    CandidateBranch->Cases.clear();

    if (!Candidate.removeBlock(DropId)) {
      continue;
    }

    Graph = std::move(Candidate);
    return true;
  }

  return false;
}

bool hasDephicationContext(const StructuredCFG &Graph, BlockId Id) {
  DephicationEdgeContext EdgeContext = Graph.dephicationEdgeContext(Id);
  DephicationEdgeContext BlockContext = Graph.dephicationBlockContext(Id);
  return !EdgeContext.VVars.empty() || !EdgeContext.Incomings.empty() ||
         !BlockContext.VVars.empty() || !BlockContext.Incomings.empty();
}

bool canUseForSharedTailMerge(const StructuredCFG &Graph,
                              const CFGBlock &Block) {
  if (!Block.BodyMaterialized || Block.BodyBlock != Block.Id ||
      hasDephicationContext(Graph, Block.Id)) {
    return false;
  }

  if (Block.Origin == CFGBlockOrigin::Original) {
    return Block.CopyKind == CFGBlockCopyKind::None;
  }

  if (Block.Origin == CFGBlockOrigin::Copied) {
    return Block.CopyKind == CFGBlockCopyKind::RegionCopy;
  }

  return false;
}

bool canSplitCommonStatementTailBlock(const StructuredCFG &Graph,
                                      const CFGBlock &Block) {
  // Shared tail extraction now accepts already-materialized copied region
  // blocks too, but still keeps synthetic and dephication blocks out.
  if (!canUseForSharedTailMerge(Graph, Block) ||
      Block.Terminator != TerminatorKind::Fallthrough ||
      Block.Successors.size() != 1 ||
      Block.Statements.empty()) {
    return false;
  }
  return true;
}

std::size_t commonStatementSuffixLength(const StructuredCFG &Graph,
                                        const std::vector<PayloadRef> &Lhs,
                                        const std::vector<PayloadRef> &Rhs) {
  std::size_t Common = 0;
  while (Common < Lhs.size() && Common < Rhs.size()) {
    PayloadRef LhsPayload = Lhs[Lhs.size() - Common - 1];
    PayloadRef RhsPayload = Rhs[Rhs.size() - Common - 1];
    if (!samePayload(Graph, LhsPayload, RhsPayload)) {
      break;
    }
    ++Common;
  }
  return Common;
}

bool commonStatementTailCandidate(const StructuredCFG &Graph, BlockId LhsId,
                                  BlockId RhsId, std::size_t &CommonSuffix) {
  if (LhsId == RhsId || Graph.hasEdge(LhsId, RhsId) ||
      Graph.hasEdge(RhsId, LhsId)) {
    return false;
  }

  const CFGBlock *Lhs = Graph.getBlock(LhsId);
  const CFGBlock *Rhs = Graph.getBlock(RhsId);
  if (Lhs == nullptr || Rhs == nullptr ||
      !sameBlockControlShapeByReference(Graph, *Lhs, *Rhs) ||
      !canSplitCommonStatementTailBlock(Graph, *Lhs) ||
      !canSplitCommonStatementTailBlock(Graph, *Rhs) ||
      !disjointPredecessorSets(Graph, LhsId, RhsId) ||
      reachesBlock(Graph, LhsId, RhsId) || reachesBlock(Graph, RhsId, LhsId)) {
    return false;
  }

  if (predecessorsOf(Graph, LhsId).empty() ||
      predecessorsOf(Graph, RhsId).empty()) {
    return false;
  }

  CommonSuffix =
      commonStatementSuffixLength(Graph, Lhs->Statements, Rhs->Statements);
  return CommonSuffix > 0 && CommonSuffix < Lhs->Statements.size() &&
         CommonSuffix < Rhs->Statements.size();
}

bool commonBranchStatementTailCandidate(const StructuredCFG &Graph,
                                        BlockId LhsId, BlockId RhsId,
                                        std::size_t &CommonSuffix) {
  if (LhsId == RhsId || Graph.hasEdge(LhsId, RhsId) ||
      Graph.hasEdge(RhsId, LhsId)) {
    return false;
  }

  const CFGBlock *Lhs = Graph.getBlock(LhsId);
  const CFGBlock *Rhs = Graph.getBlock(RhsId);
  std::vector<BlockId> LhsPreds = predecessorsOf(Graph, LhsId);
  if (Lhs == nullptr || Rhs == nullptr ||
      LhsPreds.size() != 1 ||
      !sameBranchPredecessorPair(Graph, LhsId, RhsId, LhsPreds.front()) ||
      !sameBlockControlShapeByReference(Graph, *Lhs, *Rhs) ||
      !canSplitCommonStatementTailBlock(Graph, *Lhs) ||
      !canSplitCommonStatementTailBlock(Graph, *Rhs) ||
      reachesBlock(Graph, LhsId, RhsId) || reachesBlock(Graph, RhsId, LhsId)) {
    return false;
  }

  CommonSuffix =
      commonStatementSuffixLength(Graph, Lhs->Statements, Rhs->Statements);
  return CommonSuffix > 0 && CommonSuffix < Lhs->Statements.size() &&
         CommonSuffix < Rhs->Statements.size();
}

bool canShareLinearRegionTailBlock(const StructuredCFG &Graph,
                                   const CFGBlock &Block) {
  // Linear tail sharing uses the same ownership gate as statement tails:
  // materialized copied region blocks are allowed, but synthetic and
  // dephication blocks stay excluded.
  if (!canUseForSharedTailMerge(Graph, Block) ||
      Block.Statements.empty()) {
    return false;
  }
  return true;
}

bool commonLinearRegionTailCandidate(const StructuredCFG &Graph, BlockId LhsId,
                                     BlockId RhsId, std::size_t &CommonSuffix,
                                     std::map<BlockId, LinearRegion> &Cache) {
  if (LhsId == RhsId || Graph.hasEdge(LhsId, RhsId) ||
      Graph.hasEdge(RhsId, LhsId)) {
    return false;
  }

  const CFGBlock *LhsHead = Graph.getBlock(LhsId);
  const CFGBlock *RhsHead = Graph.getBlock(RhsId);
  if (LhsHead == nullptr || RhsHead == nullptr ||
      !canShareLinearRegionTailBlock(Graph, *LhsHead) ||
      !canShareLinearRegionTailBlock(Graph, *RhsHead) ||
      !disjointPredecessorSets(Graph, LhsId, RhsId) ||
      reachesBlock(Graph, LhsId, RhsId) || reachesBlock(Graph, RhsId, LhsId)) {
    return false;
  }

  const LinearRegion *LhsRegion = cachedLinearRegion(Graph, LhsId, Cache);
  const LinearRegion *RhsRegion = cachedLinearRegion(Graph, RhsId, Cache);
  if (LhsRegion == nullptr || RhsRegion == nullptr ||
      LhsRegion->Head == InvalidBlockId || RhsRegion->Head == InvalidBlockId ||
      LhsRegion->Blocks.size() < 2 || RhsRegion->Blocks.size() < 2 ||
      LhsRegion->Blocks.size() > MaxLinearRegionMergeBlocks ||
      RhsRegion->Blocks.size() > MaxLinearRegionMergeBlocks) {
    return false;
  }

  std::size_t Common = 0;
  while (Common < LhsRegion->Blocks.size() &&
         Common < RhsRegion->Blocks.size()) {
    BlockId LhsBlockId =
        LhsRegion->Blocks[LhsRegion->Blocks.size() - Common - 1];
    BlockId RhsBlockId =
        RhsRegion->Blocks[RhsRegion->Blocks.size() - Common - 1];
    const CFGBlock *LhsBlock = Graph.getBlock(LhsBlockId);
    const CFGBlock *RhsBlock = Graph.getBlock(RhsBlockId);
    if (LhsBlock == nullptr || RhsBlock == nullptr ||
        !canShareLinearRegionTailBlock(Graph, *LhsBlock) ||
        !canShareLinearRegionTailBlock(Graph, *RhsBlock)) {
      break;
    }

    if (Common == 0) {
      if (!sameBlockShapeByReference(Graph, *LhsBlock, *RhsBlock)) {
        break;
      }
    } else if (!sameRegionTailShapeByReference(Graph, *LhsBlock, *RhsBlock)) {
      break;
    }

    ++Common;
  }

  CommonSuffix = Common;
  return Common >= 2 &&
         ((Common == LhsRegion->Blocks.size() &&
           Common == RhsRegion->Blocks.size()) ||
          (Common < LhsRegion->Blocks.size() &&
           Common < RhsRegion->Blocks.size()));
}

bool extractCommonLinearRegionTail(StructuredCFG &Graph,
                                   const LinearRegion &LhsRegion,
                                   const LinearRegion &RhsRegion,
                                   std::size_t CommonSuffix) {
  if (CommonSuffix < 2) {
    return false;
  }

  if (LhsRegion.Head == InvalidBlockId || RhsRegion.Head == InvalidBlockId ||
      CommonSuffix >= LhsRegion.Blocks.size() ||
      CommonSuffix >= RhsRegion.Blocks.size()) {
    if (CommonSuffix == LhsRegion.Blocks.size() &&
        CommonSuffix == RhsRegion.Blocks.size()) {
      StructuredCFG Candidate = Graph;
      std::vector<BlockId> Preds = predecessorsOf(Graph, RhsRegion.Head);
      if (!Candidate.redirectPredecessors(RhsRegion.Head, LhsRegion.Head,
                                          Preds)) {
        return false;
      }
      if (!Candidate.removeBlocks(RhsRegion.Blocks)) {
        return false;
      }
      Graph = std::move(Candidate);
      return true;
    }
    return false;
  }

  std::size_t LhsTailStart = LhsRegion.Blocks.size() - CommonSuffix;
  std::size_t RhsTailStart = RhsRegion.Blocks.size() - CommonSuffix;
  if (LhsTailStart == 0 || RhsTailStart == 0) {
    return false;
  }

  for (std::size_t I = 0; I < CommonSuffix; ++I) {
    BlockId LhsBlockId = LhsRegion.Blocks[LhsTailStart + I];
    BlockId RhsBlockId = RhsRegion.Blocks[RhsTailStart + I];
    const CFGBlock *LhsBlock = Graph.getBlock(LhsBlockId);
    const CFGBlock *RhsBlock = Graph.getBlock(RhsBlockId);
    if (LhsBlock == nullptr || RhsBlock == nullptr ||
        !canShareLinearRegionTailBlock(Graph, *LhsBlock) ||
        !canShareLinearRegionTailBlock(Graph, *RhsBlock) ||
        !sameRegionTailShapeByReference(Graph, *LhsBlock, *RhsBlock)) {
      return false;
    }
  }

  BlockId RhsPrefix = RhsRegion.Blocks[RhsTailStart - 1];
  BlockId LhsTailHead = LhsRegion.Blocks[LhsTailStart];
  BlockId RhsTailHead = RhsRegion.Blocks[RhsTailStart];

  StructuredCFG Candidate = Graph;
  if (!Candidate.replaceEdge(RhsPrefix, RhsTailHead, LhsTailHead)) {
    return false;
  }

  std::vector<BlockId> RhsTailBlocks(RhsRegion.Blocks.begin() + RhsTailStart,
                                     RhsRegion.Blocks.end());
  if (!Candidate.removeBlocks(RhsTailBlocks)) {
    return false;
  }

  Graph = std::move(Candidate);
  return true;
}

bool revertGotoRelatedCommonLinearRegionTail(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  if (Current.Gotos.empty()) {
    return false;
  }

  std::map<BlockId, LinearRegion> RegionCache;
  std::vector<BlockId> BlockIds;
  BlockIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    BlockIds.push_back(Block.Id);
  }
  std::sort(BlockIds.begin(), BlockIds.end());

  for (const StructuredGoto &Goto : Current.Gotos.gotos()) {
    if (Goto.Source == InvalidBlockId || Goto.Target == InvalidBlockId ||
        !gotoEdgeFromSourceOrParent(Graph, Current.Gotos, Goto.Source,
                                    Goto.Target)) {
      continue;
    }

    const LinearRegion *GotoRegion =
        cachedLinearRegion(Graph, Goto.Target, RegionCache);
    if (GotoRegion == nullptr || GotoRegion->Head == InvalidBlockId ||
        GotoRegion->Blocks.size() < 2 ||
        GotoRegion->Blocks.size() > MaxLinearRegionMergeBlocks) {
      continue;
    }

    BlockId BestOther = InvalidBlockId;
    std::size_t BestSuffix = 0;
    for (BlockId OtherId : BlockIds) {
      std::size_t CommonSuffix = 0;
      if (!commonLinearRegionTailCandidate(Graph, Goto.Target, OtherId,
                                          CommonSuffix, RegionCache)) {
        continue;
      }
      if (CommonSuffix > BestSuffix ||
          (CommonSuffix == BestSuffix && OtherId < BestOther)) {
        BestOther = OtherId;
        BestSuffix = CommonSuffix;
      }
    }

    if (BestOther == InvalidBlockId) {
      continue;
    }

    const LinearRegion *BestRegion =
        cachedLinearRegion(Graph, BestOther, RegionCache);
    if (BestRegion == nullptr) {
      continue;
    }

    if (extractCommonLinearRegionTail(Graph, *GotoRegion, *BestRegion,
                                      BestSuffix)) {
      return true;
    }
  }

  return false;
}

bool extractCommonStatementTail(StructuredCFG &Graph, BlockId LhsId,
                                BlockId RhsId, std::size_t CommonSuffix) {
  const CFGBlock *Lhs = Graph.getBlock(LhsId);
  if (Lhs == nullptr || CommonSuffix == 0 ||
      CommonSuffix >= Lhs->Statements.size()) {
    return false;
  }

  std::vector<PayloadRef> TailStatements(Lhs->Statements.end() - CommonSuffix,
                                         Lhs->Statements.end());
  TerminatorKind TailTerminator = Lhs->Terminator;
  PayloadRef TailCondition = Lhs->Condition;
  std::vector<BlockId> TailSuccessors = Lhs->Successors;
  std::vector<SwitchCase> TailCases = Lhs->Cases;

  BlockId TailId = Graph.createSyntheticBlock(
      TailSuccessors, CFGBlockCreator::SAILRDeoptimization);
  CFGBlock *Tail = Graph.getBlock(TailId);
  if (Tail == nullptr) {
    return false;
  }

  Tail->CopyKind = CFGBlockCopyKind::None;
  Tail->Statements = std::move(TailStatements);
  Tail->Terminator = TailTerminator;
  Tail->Condition = TailCondition;
  Tail->Successors = std::move(TailSuccessors);
  Tail->Cases = std::move(TailCases);

  auto RewriteHead = [&](BlockId Id) {
    CFGBlock *Block = Graph.getBlock(Id);
    if (Block == nullptr || CommonSuffix >= Block->Statements.size()) {
      return false;
    }

    Block->Statements.resize(Block->Statements.size() - CommonSuffix);
    Block->Terminator = TerminatorKind::Fallthrough;
    Block->Condition = {};
    Block->Successors = {TailId};
    Block->Cases.clear();
    Block->BodyBlock = Id;
    Block->BodyMaterialized = true;
    return true;
  };

  return RewriteHead(LhsId) && RewriteHead(RhsId);
}

bool revertGotoRelatedCommonStatementTail(StructuredCFG &Graph,
                                          const StructuringEvaluation &Current) {
  if (Current.Gotos.empty()) {
    return false;
  }

  std::vector<BlockId> BlockIds;
  BlockIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    BlockIds.push_back(Block.Id);
  }
  std::sort(BlockIds.begin(), BlockIds.end());

  for (const StructuredGoto &Goto : Current.Gotos.gotos()) {
    if (Goto.Source == InvalidBlockId || Goto.Target == InvalidBlockId ||
        !gotoEdgeFromSourceOrParent(Graph, Current.Gotos, Goto.Source,
                                    Goto.Target)) {
      continue;
    }

    std::optional<BlockId> BranchBase;
    std::optional<BlockId> BranchOther;
    if (const CFGBlock *GotoSourceBlock = Graph.getBlock(Goto.Source)) {
      if (GotoSourceBlock->Terminator == TerminatorKind::Branch &&
          GotoSourceBlock->Successors.size() == 2) {
        if (GotoSourceBlock->Successors[0] == Goto.Target) {
          BranchBase = Goto.Target;
          BranchOther = GotoSourceBlock->Successors[1];
        } else if (GotoSourceBlock->Successors[1] == Goto.Target) {
          BranchBase = Goto.Target;
          BranchOther = GotoSourceBlock->Successors[0];
        }
      }
    }
    if (!BranchOther.has_value() &&
        Graph.hasEdge(Goto.Source, Goto.Target)) {
      BranchBase = Goto.Source;
      BranchOther = privateBranchSibling(Graph, Goto.Source);
    }

    BlockId BestBase = Goto.Target;
    BlockId BestOther = InvalidBlockId;
    std::size_t BestSuffix = 0;
    for (BlockId OtherId : BlockIds) {
      std::size_t CommonSuffix = 0;
      BlockId CandidateBase = Goto.Target;
      bool Candidate =
          commonStatementTailCandidate(Graph, Goto.Target, OtherId,
                                       CommonSuffix);
      if (!Candidate && BranchBase.has_value() && BranchOther.has_value() &&
          OtherId == *BranchOther) {
        CandidateBase = *BranchBase;
        Candidate = commonBranchStatementTailCandidate(
            Graph, CandidateBase, OtherId, CommonSuffix);
      }
      if (!Candidate) {
        continue;
      }
      if (CommonSuffix > BestSuffix ||
          (CommonSuffix == BestSuffix && OtherId < BestOther)) {
        BestBase = CandidateBase;
        BestOther = OtherId;
        BestSuffix = CommonSuffix;
      }
    }

    if (BestOther == InvalidBlockId) {
      continue;
    }

    StructuredCFG Candidate = Graph;
    if (!extractCommonStatementTail(Candidate, BestBase, BestOther,
                                    BestSuffix)) {
      continue;
    }

    Graph = std::move(Candidate);
    return true;
  }

  return false;
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

std::vector<std::vector<BlockId>>
materializePredecessorComponents(const StructuredCFG &Graph,
                                 const std::vector<BlockId> &Blocks) {
  if (Graph.hasPredecessorRewritePayloadMaterializeHook() &&
      !Graph.hasGroupedPredecessorRewritePayloadMaterializeHook()) {
    std::vector<std::vector<BlockId>> Components;
    Components.reserve(Blocks.size());
    for (BlockId Block : Blocks) {
      Components.push_back({Block});
    }
    return Components;
  }

  return connectedPredecessorComponents(Graph, Blocks);
}

std::vector<BlockId>
expandToConnectedPredecessorComponents(const StructuredCFG &Graph,
                                       const std::vector<BlockId> &AllPreds,
                                       const std::vector<BlockId> &Selected) {
  std::set<BlockId> SelectedSet(Selected.begin(), Selected.end());
  std::set<BlockId> Expanded = SelectedSet;

  // A copied component has no single predecessor for the copied head. Keep
  // predecessor-sensitive payload hooks on one-pred copies until the shared
  // payload layer can express grouped incoming-value rewrites.
  if (Graph.hasPredecessorRewritePayloadMaterializeHook() &&
      !Graph.hasGroupedPredecessorRewritePayloadMaterializeHook()) {
    return std::vector<BlockId>(Expanded.begin(), Expanded.end());
  }

  for (const std::vector<BlockId> &Component :
       connectedPredecessorComponents(Graph, AllPreds)) {
    bool HasSelectedPred = false;
    for (BlockId Pred : Component) {
      if (SelectedSet.count(Pred) != 0) {
        HasSelectedPred = true;
        break;
      }
    }
    if (!HasSelectedPred) {
      continue;
    }
    Expanded.insert(Component.begin(), Component.end());
  }

  return std::vector<BlockId>(Expanded.begin(), Expanded.end());
}

std::optional<BlockId>
privateSwitchParentForJumpTo(const StructuredCFG &Graph, BlockId Pred,
                             BlockId Target) {
  const CFGBlock *PredBlock = Graph.getBlock(Pred);
  if (PredBlock == nullptr ||
      PredBlock->Terminator != TerminatorKind::Fallthrough ||
      PredBlock->Successors.size() != 1 ||
      PredBlock->Successors.front() != Target) {
    return std::nullopt;
  }

  std::vector<BlockId> Preds = predecessorsOf(Graph, Pred);
  if (Preds.size() != 1) {
    return std::nullopt;
  }

  const CFGBlock *Parent = Graph.getBlock(Preds.front());
  if (Parent == nullptr || Parent->Terminator != TerminatorKind::Switch ||
      !Graph.hasEdge(Parent->Id, Pred)) {
    return std::nullopt;
  }
  return Parent->Id;
}

bool returnRegionContainsUnreachableTerminal(const StructuredCFG &Graph,
                                             const ReturnRegion &Region) {
  for (BlockId Id : Region.Blocks) {
    const CFGBlock *Block = Graph.getBlock(Id);
    if (Block != nullptr && Block->Terminator == TerminatorKind::Unreachable) {
      return true;
    }
  }
  return false;
}

std::vector<BlockId>
expandToSiblingSwitchJumpPredecessors(const StructuredCFG &Graph,
                                      const std::vector<BlockId> &AllPreds,
                                      const std::vector<BlockId> &Selected,
                                      const ReturnRegion &Region) {
  if (!returnRegionContainsUnreachableTerminal(Graph, Region)) {
    return Selected;
  }

  std::set<BlockId> AllPredSet(AllPreds.begin(), AllPreds.end());
  std::set<BlockId> Expanded(Selected.begin(), Selected.end());

  // Phoenix may put the goto on only one case stub even when sibling stubs from
  // the same switch all jump to the same return fork. Copy all such private
  // stubs together so the original fork can disappear.
  for (BlockId Pred : Selected) {
    std::optional<BlockId> Parent =
        privateSwitchParentForJumpTo(Graph, Pred, Region.Head);
    if (!Parent.has_value()) {
      continue;
    }

    for (BlockId Sibling : Graph.successorsOf(*Parent)) {
      if (AllPredSet.count(Sibling) == 0 || Expanded.count(Sibling) != 0) {
        continue;
      }

      std::optional<BlockId> SiblingParent =
          privateSwitchParentForJumpTo(Graph, Sibling, Region.Head);
      if (SiblingParent.has_value() && *SiblingParent == *Parent) {
        Expanded.insert(Sibling);
      }
    }
  }

  return std::vector<BlockId>(Expanded.begin(), Expanded.end());
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

bool hasSwitchCaseEdge(const StructuredCFG &Graph, BlockId Pred,
                       BlockId Target) {
  SwitchEdgeKind Kind = Graph.switchEdgeKind(Pred, Target);
  return Kind == SwitchEdgeKind::CaseOnly ||
         Kind == SwitchEdgeKind::DefaultAndCase;
}

bool hasSwitchDefaultEdge(const StructuredCFG &Graph, BlockId Pred,
                          BlockId Target) {
  SwitchEdgeKind Kind = Graph.switchEdgeKind(Pred, Target);
  return Kind == SwitchEdgeKind::DefaultOnly ||
         Kind == SwitchEdgeKind::DefaultAndCase;
}

bool switchCaseEdgeReachesBlock(const CFGBlock &Block, BlockId Target) {
  if (!switchCaseReachesBlock(Block, Target)) {
    return false;
  }
  std::size_t CaseSuccessorStart = Block.Successors.empty() ? 0 : 1;
  return std::find(Block.Successors.begin() + CaseSuccessorStart,
                   Block.Successors.end(), Target) != Block.Successors.end();
}

bool redirectSwitchCases(StructuredCFG &Graph, BlockId OldTarget,
                         BlockId NewTarget,
                         const std::vector<BlockId> &SwitchPreds) {
  for (BlockId Pred : SwitchPreds) {
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (PredBlock == nullptr ||
        PredBlock->Terminator != TerminatorKind::Switch ||
        !switchCaseReachesBlock(*PredBlock, OldTarget)) {
      return false;
    }
  }

  for (BlockId Pred : SwitchPreds) {
    CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (!Graph.redirectDephicationIncomingTarget(Pred, OldTarget, NewTarget)) {
      return false;
    }
    for (std::size_t I = 1; I < PredBlock->Successors.size(); ++I) {
      if (PredBlock->Successors[I] == OldTarget) {
        PredBlock->Successors[I] = NewTarget;
      }
    }
    for (SwitchCase &Case : PredBlock->Cases) {
      if (Case.Target == OldTarget) {
        Case.Target = NewTarget;
      }
    }
  }
  return true;
}

bool nonCaseSuccessorReachesBlock(const CFGBlock &Block, BlockId Target) {
  if (Block.Terminator == TerminatorKind::Switch) {
    return !Block.Successors.empty() && Block.Successors.front() == Target;
  }
  return std::find(Block.Successors.begin(), Block.Successors.end(), Target) !=
         Block.Successors.end();
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

  if (!Graph.redirectDephicationIncomingTarget(SwitchId, OldTarget, NewTarget)) {
    return false;
  }
  Block->Successors.front() = NewTarget;
  return true;
}

std::optional<ConditionCompare>
matchedConditionCompare(const StructuredCFG &Graph, const CFGBlock &Block,
                        PayloadRef ComparedValue) {
  if (Block.Terminator != TerminatorKind::Branch ||
      Block.Successors.size() != 2) {
    return std::nullopt;
  }

  std::optional<ConditionCompare> Compare =
      Graph.conditionCompare(Block.Condition);
  if (!Compare.has_value()) {
    return std::nullopt;
  }
  if (Compare->Kind != ConditionCompareKind::Equal &&
      Compare->Kind != ConditionCompareKind::NotEqual) {
    return std::nullopt;
  }
  if (Compare->TrueTargetIndex >= Block.Successors.size() ||
      Compare->EqualTargetIndex >= Block.Successors.size()) {
    return std::nullopt;
  }
  if (ComparedValue.isValid() &&
      !samePayload(Graph, Compare->ComparedValue, ComparedValue)) {
    return std::nullopt;
  }
  return Compare;
}

std::optional<ConditionCompare>
matchedRangeConditionCompare(const StructuredCFG &Graph, const CFGBlock &Block,
                             PayloadRef ComparedValue) {
  if (Block.Terminator != TerminatorKind::Branch ||
      Block.Successors.size() != 2) {
    return std::nullopt;
  }

  std::optional<ConditionCompare> Compare =
      Graph.conditionCompare(Block.Condition);
  if (!Compare.has_value()) {
    return std::nullopt;
  }
  if (Compare->Kind != ConditionCompareKind::GreaterThan &&
      Compare->Kind != ConditionCompareKind::GreaterEqual &&
      Compare->Kind != ConditionCompareKind::LessThan &&
      Compare->Kind != ConditionCompareKind::LessEqual) {
    return std::nullopt;
  }
  if (Compare->TrueTargetIndex >= Block.Successors.size()) {
    return std::nullopt;
  }
  if (ComparedValue.isValid() &&
      !samePayload(Graph, Compare->ComparedValue, ComparedValue)) {
    return std::nullopt;
  }
  return Compare;
}

bool canConsumeLoweredSwitchIfNode(const StructuredCFG &Graph,
                                   BlockId Node, BlockId ExpectedPred,
                                   PayloadRef ComparedValue) {
  const CFGBlock *Block = Graph.getBlock(Node);
  if (Block == nullptr || !Block->Statements.empty() ||
      !matchedConditionCompare(Graph, *Block, ComparedValue).has_value()) {
    return false;
  }

  std::vector<BlockId> Preds = predecessorsOf(Graph, Node);
  return Preds.size() == 1 && Preds.front() == ExpectedPred;
}

bool hasOnlyPredecessor(const StructuredCFG &Graph, BlockId Node,
                        BlockId ExpectedPred) {
  std::vector<BlockId> Preds = predecessorsOf(Graph, Node);
  return Preds.size() == 1 && Preds.front() == ExpectedPred;
}

bool rangeConditionContainsCaseValue(const ConditionCompare &Range,
                                     const ConditionCompare &Case,
                                     bool TrueOutcome) {
  if (!Range.HasIntegerValue || !Case.HasIntegerValue) {
    return false;
  }

  bool Result = false;
  if (Range.SignedPredicate) {
    switch (Range.Kind) {
    case ConditionCompareKind::GreaterThan:
      Result = Case.SignedIntegerValue > Range.SignedIntegerValue;
      break;
    case ConditionCompareKind::GreaterEqual:
      Result = Case.SignedIntegerValue >= Range.SignedIntegerValue;
      break;
    case ConditionCompareKind::LessThan:
      Result = Case.SignedIntegerValue < Range.SignedIntegerValue;
      break;
    case ConditionCompareKind::LessEqual:
      Result = Case.SignedIntegerValue <= Range.SignedIntegerValue;
      break;
    default:
      return false;
    }
  } else {
    switch (Range.Kind) {
    case ConditionCompareKind::GreaterThan:
      Result = Case.UnsignedIntegerValue > Range.UnsignedIntegerValue;
      break;
    case ConditionCompareKind::GreaterEqual:
      Result = Case.UnsignedIntegerValue >= Range.UnsignedIntegerValue;
      break;
    case ConditionCompareKind::LessThan:
      Result = Case.UnsignedIntegerValue < Range.UnsignedIntegerValue;
      break;
    case ConditionCompareKind::LessEqual:
      Result = Case.UnsignedIntegerValue <= Range.UnsignedIntegerValue;
      break;
    default:
      return false;
    }
  }

  return TrueOutcome ? Result : !Result;
}

bool loweredSwitchCasesFitRangeGuard(const ConditionCompare &Range,
                                     const LoweredSwitchIfChain &Chain,
                                     bool TrueOutcome) {
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    if (!Case.Compare.has_value() ||
        !rangeConditionContainsCaseValue(Range, *Case.Compare, TrueOutcome)) {
      return false;
    }
  }
  return true;
}

bool loweredSwitchChainFitsRangeGuard(const StructuredCFG &Graph,
                                      const CFGBlock &GuardBlock,
                                      const ConditionCompare &Guard,
                                      const LoweredSwitchIfChain &Chain,
                                      std::size_t ChainIndex) {
  if (!samePayload(Graph, Guard.ComparedValue, Chain.ComparedValue)) {
    return false;
  }

  std::size_t DefaultIndex = ChainIndex == 0 ? 1 : 0;
  if (!sameBlockReference(Graph, GuardBlock.Successors[DefaultIndex],
                          Chain.DefaultTarget)) {
    return false;
  }

  bool TrueOutcome = ChainIndex == Guard.TrueTargetIndex;
  return loweredSwitchCasesFitRangeGuard(Guard, Chain, TrueOutcome);
}

bool loweredSwitchCaseValuesAreDisjoint(const StructuredCFG &Graph,
                                        const LoweredSwitchIfChain &Lhs,
                                        const LoweredSwitchIfChain &Rhs) {
  std::set<std::uint64_t> IntegerValues;
  std::set<PayloadId> PayloadValues;

  auto AddCase = [&](const LoweredSwitchIfCase &Case) {
    if (Case.Compare.has_value() && Case.Compare->HasIntegerValue) {
      return IntegerValues.insert(Case.Compare->UnsignedIntegerValue).second;
    }
    return PayloadValues.insert(Graph.payloadOrigin(Case.Value.Id)).second;
  };

  for (const LoweredSwitchIfCase &Case : Lhs.Cases) {
    if (!AddCase(Case)) {
      return false;
    }
  }
  for (const LoweredSwitchIfCase &Case : Rhs.Cases) {
    if (!AddCase(Case)) {
      return false;
    }
  }
  return true;
}

bool sameClosedDefaultBlock(const StructuredCFG &Graph, BlockId Lhs,
                            BlockId Rhs) {
  const CFGBlock *LhsBlock = Graph.getBlock(Lhs);
  const CFGBlock *RhsBlock = Graph.getBlock(Rhs);
  if (LhsBlock == nullptr || RhsBlock == nullptr) {
    return false;
  }
  if (!isClosedTerminal(*LhsBlock) || !isClosedTerminal(*RhsBlock)) {
    return false;
  }
  if (LhsBlock->Statements.empty() || RhsBlock->Statements.empty()) {
    return false;
  }
  return samePayloads(Graph, LhsBlock->Statements, RhsBlock->Statements);
}

bool mergeEquivalentRangeTreeDefaults(const StructuredCFG &Graph,
                                      LoweredSwitchIfChain &First,
                                      LoweredSwitchIfChain &Second) {
  if (First.DefaultTarget == Second.DefaultTarget) {
    return true;
  }
  if (!sameClosedDefaultBlock(Graph, First.DefaultTarget,
                              Second.DefaultTarget)) {
    return false;
  }

  std::set<BlockId> Removed;
  Removed.insert(First.RemovedBlocks.begin(), First.RemovedBlocks.end());
  Removed.insert(Second.RemovedBlocks.begin(), Second.RemovedBlocks.end());
  Removed.insert(First.Head);
  Removed.insert(Second.Head);
  if (!blockOnlyReachedFrom(Graph, First.DefaultTarget, Removed) ||
      !blockOnlyReachedFrom(Graph, Second.DefaultTarget, Removed)) {
    return false;
  }

  Second.RemovedBlocks.push_back(Second.DefaultTarget);
  Second.DefaultTarget = First.DefaultTarget;
  return true;
}

// Conservative subset of Angr's range-tree lowered switch recovery:
// both sides of one range guard must already be valid equality chains and
// contain only cases allowed by the guard side. RetDupPass may split one empty
// default return into two equal blocks before this pass sees the graph, so we
// only merge duplicated defaults when both are closed and chain-private.
bool mergeRangeTreeLoweredSwitchIfChains(const StructuredCFG &Graph,
                                         const CFGBlock &GuardBlock,
                                         const ConditionCompare &Guard,
                                         LoweredSwitchIfChain &First,
                                         LoweredSwitchIfChain &Second) {
  if (!samePayload(Graph, Guard.ComparedValue, First.ComparedValue) ||
      !samePayload(Graph, Guard.ComparedValue, Second.ComparedValue) ||
      !loweredSwitchCaseValuesAreDisjoint(Graph, First, Second)) {
    return false;
  }
  if (!mergeEquivalentRangeTreeDefaults(Graph, First, Second)) {
    return false;
  }

  for (std::size_t I = 0; I < GuardBlock.Successors.size(); ++I) {
    const LoweredSwitchIfChain &Chain = I == 0 ? First : Second;
    bool TrueOutcome = I == Guard.TrueTargetIndex;
    if (!loweredSwitchCasesFitRangeGuard(Guard, Chain, TrueOutcome)) {
      return false;
    }
  }

  First.Cases.insert(First.Cases.end(), Second.Cases.begin(),
                     Second.Cases.end());
  First.RemovedBlocks.insert(First.RemovedBlocks.end(),
                             Second.RemovedBlocks.begin(),
                             Second.RemovedBlocks.end());
  First.RemovedBlocks.push_back(First.Head);
  First.RemovedBlocks.push_back(Second.Head);
  First.Head = GuardBlock.Id;
  return true;
}

bool rangeGuardCoversChainCases(const StructuredCFG &Graph,
                                const CFGBlock &GuardBlock,
                                const LoweredSwitchIfChain &Chain) {
  // This is only a "do not rewrite the inner chain first" check. The real
  // range-guard rewrite still verifies the default target before consuming it.
  std::optional<ConditionCompare> Guard =
      matchedRangeConditionCompare(Graph, GuardBlock, Chain.ComparedValue);
  if (!Guard.has_value()) {
    return false;
  }

  for (std::size_t I = 0; I < GuardBlock.Successors.size(); ++I) {
    if (GuardBlock.Successors[I] != Chain.Head) {
      continue;
    }
    bool TrueOutcome = I == Guard->TrueTargetIndex;
    return loweredSwitchCasesFitRangeGuard(*Guard, Chain, TrueOutcome);
  }
  return false;
}

bool loweredSwitchIfChainHasRangeGuard(const StructuredCFG &Graph,
                                       const LoweredSwitchIfChain &Chain) {
  std::vector<BlockId> Worklist = predecessorsOf(Graph, Chain.Head);
  std::set<BlockId> Seen;
  while (!Worklist.empty()) {
    BlockId PredId = Worklist.back();
    Worklist.pop_back();
    if (!Seen.insert(PredId).second) {
      continue;
    }

    const CFGBlock *Pred = Graph.getBlock(PredId);
    if (Pred == nullptr || Pred->Successors.size() != 2) {
      continue;
    }
    if (rangeGuardCoversChainCases(Graph, *Pred, Chain)) {
      return true;
    }

    std::vector<BlockId> Preds = predecessorsOf(Graph, PredId);
    Worklist.insert(Worklist.end(), Preds.begin(), Preds.end());
  }
  return false;
}

// Angr rejects lowered-switch candidates whose case/default bodies are also
// entered from outside the comparison chain. Keep the same guard here because
// rewriting such a shared target as a switch case would hide real sharing.
bool loweredSwitchTargetOnlyReachedFromChain(const StructuredCFG &Graph,
                                             BlockId Target,
                                             const std::set<BlockId> &Chain) {
  for (BlockId Pred : predecessorsOf(Graph, Target)) {
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    bool SyntheticChainEdge =
        PredBlock != nullptr &&
        (PredBlock->CopyKind == CFGBlockCopyKind::SyntheticForwarder ||
         PredBlock->CopyKind == CFGBlockCopyKind::SyntheticGoto) &&
        PredBlock->SyntheticTarget == Target &&
        Chain.count(PredBlock->SyntheticSource) != 0;
    if (Pred != Target && Chain.count(Pred) == 0 && !SyntheticChainEdge) {
      return false;
    }
  }
  return true;
}

bool blockOnlyReachedFrom(const StructuredCFG &Graph, BlockId Target,
                          const std::set<BlockId> &AllowedPreds) {
  for (BlockId Pred : predecessorsOf(Graph, Target)) {
    if (Pred != Target && AllowedPreds.count(Pred) == 0) {
      return false;
    }
  }
  return true;
}

bool loweredSwitchTargetsOnlyReachedFromChain(
    const StructuredCFG &Graph, const LoweredSwitchIfChain &Chain) {
  std::set<BlockId> ChainBlocks = {Chain.Head};
  ChainBlocks.insert(Chain.RemovedBlocks.begin(), Chain.RemovedBlocks.end());

  if (!loweredSwitchTargetOnlyReachedFromChain(Graph, Chain.DefaultTarget,
                                               ChainBlocks)) {
    return false;
  }
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    if (!loweredSwitchTargetOnlyReachedFromChain(Graph, Case.Target,
                                                 ChainBlocks)) {
      return false;
    }
  }
  return true;
}

bool loweredSwitchDefaultReachesChain(const StructuredCFG &Graph,
                                      const LoweredSwitchIfChain &Chain) {
  std::set<BlockId> ChainBlocks = {Chain.Head};
  ChainBlocks.insert(Chain.RemovedBlocks.begin(), Chain.RemovedBlocks.end());

  std::set<BlockId> Seen;
  std::vector<BlockId> Worklist = {Chain.DefaultTarget};
  while (!Worklist.empty()) {
    BlockId Current = Worklist.back();
    Worklist.pop_back();
    if (!Seen.insert(Current).second) {
      continue;
    }
    if (ChainBlocks.count(Current) != 0) {
      return true;
    }
    for (BlockId Succ : Graph.successorsOf(Current)) {
      Worklist.push_back(Succ);
    }
  }
  return false;
}

bool graphHasSwitchTerminator(const StructuredCFG &Graph) {
  for (const CFGBlock &Block : Graph.blocks()) {
    if (Block.Terminator == TerminatorKind::Switch) {
      return true;
    }
  }
  return false;
}

std::optional<std::int64_t>
signedCaseValue(const LoweredSwitchIfCase &Case) {
  if (!Case.Compare.has_value() || !Case.Compare->HasIntegerValue) {
    return std::nullopt;
  }
  if (Case.Compare->SignedPredicate) {
    return Case.Compare->SignedIntegerValue;
  }
  if (Case.Compare->UnsignedIntegerValue >
      static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max())) {
    return std::nullopt;
  }
  return static_cast<std::int64_t>(Case.Compare->UnsignedIntegerValue);
}

std::optional<std::vector<std::int64_t>>
loweredSwitchIntegerCaseValues(const LoweredSwitchIfChain &Chain) {
  std::vector<std::int64_t> Values;
  Values.reserve(Chain.Cases.size());
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    std::optional<std::int64_t> Value = signedCaseValue(Case);
    if (!Value.has_value()) {
      return std::nullopt;
    }
    Values.push_back(*Value);
  }
  std::sort(Values.begin(), Values.end());
  return Values;
}

bool hasAllOnesSwitchSentinelCase(const LoweredSwitchIfChain &Chain) {
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    if (!Case.Compare.has_value() || !Case.Compare->HasIntegerValue) {
      continue;
    }

    std::uint64_t Value = Case.Compare->UnsignedIntegerValue;
    if (Value == LoweredSwitchI32AllOnesSentinel ||
        Value == LoweredSwitchI64AllOnesSentinel) {
      return true;
    }
  }
  return false;
}

std::size_t maxContinuousCaseRun(const std::vector<std::int64_t> &Values) {
  if (Values.empty()) {
    return 0;
  }

  std::size_t MaxRun = 1;
  std::size_t CurrentRun = 1;
  for (std::size_t I = 1; I < Values.size(); ++I) {
    if (Values[I] == Values[I - 1] + 1) {
      ++CurrentRun;
    } else {
      MaxRun = std::max(MaxRun, CurrentRun);
      CurrentRun = 1;
    }
  }
  return std::max(MaxRun, CurrentRun);
}

bool loweredSwitchChainPassesAngrHeuristics(const LoweredSwitchIfChain &Chain,
                                            bool HadExistingSwitch) {
  // Match Angr's low false-positive filters using only shared CFG metadata.
  // If integer values are unavailable, keep the earlier conservative behavior.
  if (hasAllOnesSwitchSentinelCase(Chain)) {
    return false;
  }

  std::optional<std::vector<std::int64_t>> CaseValues =
      loweredSwitchIntegerCaseValues(Chain);
  if (!CaseValues.has_value()) {
    return true;
  }

  std::size_t MaxRun = maxContinuousCaseRun(*CaseValues);
  if (MaxRun >= MaxLoweredSwitchContinuousCases) {
    return false;
  }

  bool AllContinuous = MaxRun == Chain.Cases.size();
  if (AllContinuous && !HadExistingSwitch) {
    return false;
  }

  std::set<BlockId> DistinctTargets;
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    DistinctTargets.insert(Case.Target);
  }
  if (DistinctTargets.size() < 2 && !HadExistingSwitch) {
    return false;
  }
  return true;
}

std::optional<LoweredSwitchIfChain>
collectLoweredSwitchIfChain(const StructuredCFG &Graph, BlockId HeadId,
                            bool AllowRangeGuardedHead = false) {
  if (!Graph.dephicationVVars().empty() ||
      !Graph.dephicationIncomings().empty()) {
    return std::nullopt;
  }

  const CFGBlock *Head = Graph.getBlock(HeadId);
  if (Head == nullptr) {
    return std::nullopt;
  }

  std::optional<ConditionCompare> FirstCompare =
      matchedConditionCompare(Graph, *Head, {});
  if (!FirstCompare.has_value()) {
    return std::nullopt;
  }

  LoweredSwitchIfChain Chain;
  Chain.Head = HeadId;
  Chain.ComparedValue = FirstCompare->ComparedValue;

  std::set<BlockId> ChainBlocks;
  // Prefer the parsed integer value when it exists. Different payload ids can
  // still render the same case value, and Angr rejects those duplicate cases.
  std::set<std::uint64_t> IntegerCaseValues;
  std::set<PayloadId> CaseValues;
  BlockId CurrentId = HeadId;
  ChainBlocks.insert(CurrentId);

  while (true) {
    const CFGBlock *Current = Graph.getBlock(CurrentId);
    if (Current == nullptr) {
      return std::nullopt;
    }

    std::optional<ConditionCompare> Compare =
        matchedConditionCompare(Graph, *Current, Chain.ComparedValue);
    if (!Compare.has_value()) {
      return std::nullopt;
    }

    std::size_t CaseIndex = Compare->EqualTargetIndex;
    std::size_t NextIndex = CaseIndex == 0 ? 1 : 0;
    BlockId CaseTarget = Current->Successors[CaseIndex];
    BlockId Next = Current->Successors[NextIndex];
    if (ChainBlocks.count(CaseTarget) != 0) {
      return std::nullopt;
    }
    if (Compare->HasIntegerValue) {
      if (!IntegerCaseValues.insert(Compare->UnsignedIntegerValue).second) {
        return std::nullopt;
      }
    } else {
      PayloadId CaseValueOrigin = Graph.payloadOrigin(Compare->ConstantValue.Id);
      if (!CaseValues.insert(CaseValueOrigin).second) {
        return std::nullopt;
      }
    }
    Chain.Cases.push_back({Compare->ConstantValue, Compare, CaseTarget});

    if (ChainBlocks.count(Next) != 0) {
      return std::nullopt;
    }

    if (!canConsumeLoweredSwitchIfNode(Graph, Next, CurrentId,
                                      Chain.ComparedValue)) {
      Chain.DefaultTarget = Next;
      break;
    }

    Chain.RemovedBlocks.push_back(Next);
    ChainBlocks.insert(Next);
    CurrentId = Next;
  }

  if (Chain.Cases.size() < 2 ||
      Chain.DefaultTarget == InvalidBlockId) {
    return std::nullopt;
  }

  std::set<BlockId> Removed(Chain.RemovedBlocks.begin(),
                            Chain.RemovedBlocks.end());
  if (Removed.count(Chain.DefaultTarget) != 0) {
    return std::nullopt;
  }
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    if (Removed.count(Case.Target) != 0) {
      return std::nullopt;
    }
  }
  if (!AllowRangeGuardedHead &&
      loweredSwitchIfChainHasRangeGuard(Graph, Chain)) {
    return std::nullopt;
  }
  if (!AllowRangeGuardedHead &&
      !loweredSwitchTargetsOnlyReachedFromChain(Graph, Chain)) {
    return std::nullopt;
  }
  if (loweredSwitchDefaultReachesChain(Graph, Chain)) {
    return std::nullopt;
  }
  return Chain;
}

std::optional<LoweredSwitchIfChain>
collectRangeGuardedLoweredSwitchIfChainFrom(const StructuredCFG &Graph,
                                            BlockId HeadId,
                                            std::set<BlockId> SeenGuards) {
  const CFGBlock *Head = Graph.getBlock(HeadId);
  if (Head == nullptr || !Head->Statements.empty()) {
    return std::nullopt;
  }
  if (!SeenGuards.insert(HeadId).second) {
    return std::nullopt;
  }

  std::optional<ConditionCompare> Guard =
      matchedRangeConditionCompare(Graph, *Head, {});
  if (!Guard.has_value()) {
    return std::nullopt;
  }

  std::optional<LoweredSwitchIfChain> BranchChains[2];

  for (std::size_t ChainIndex = 0; ChainIndex < Head->Successors.size();
       ++ChainIndex) {
    BlockId ChainHead = Head->Successors[ChainIndex];
    if (!hasOnlyPredecessor(Graph, ChainHead, HeadId)) {
      continue;
    }
    std::optional<LoweredSwitchIfChain> Chain =
        collectLoweredSwitchIfChain(Graph, ChainHead,
                                    /*AllowRangeGuardedHead=*/true);
    if (!Chain.has_value()) {
      // Support a conservative linear range-guard chain:
      //   range guard -> range guard -> equality if-chain
      // All non-chain guard edges must share the final default target.
      Chain = collectRangeGuardedLoweredSwitchIfChainFrom(Graph, ChainHead,
                                                          SeenGuards);
    }
    if (!Chain.has_value()) {
      continue;
    }

    BranchChains[ChainIndex] = Chain;

    if (!loweredSwitchChainFitsRangeGuard(Graph, *Head, *Guard, *Chain,
                                          ChainIndex)) {
      continue;
    }

    std::size_t DefaultIndex = ChainIndex == 0 ? 1 : 0;
    BlockId GuardDefault = Head->Successors[DefaultIndex];
    if (GuardDefault != Chain->DefaultTarget) {
      std::set<BlockId> RemovedBlocks = {HeadId};
      RemovedBlocks.insert(Chain->RemovedBlocks.begin(),
                           Chain->RemovedBlocks.end());
      if (!blockOnlyReachedFrom(Graph, Chain->DefaultTarget, RemovedBlocks)) {
        continue;
      }
      Chain->RemovedBlocks.push_back(Chain->DefaultTarget);
      Chain->DefaultTarget = GuardDefault;
    }
    Chain->Head = HeadId;
    Chain->RemovedBlocks.insert(Chain->RemovedBlocks.begin(), ChainHead);
    return Chain;
  }

  if (BranchChains[0].has_value() && BranchChains[1].has_value() &&
      mergeRangeTreeLoweredSwitchIfChains(Graph, *Head, *Guard,
                                          *BranchChains[0],
                                          *BranchChains[1])) {
    return BranchChains[0];
  }

  return std::nullopt;
}

std::optional<LoweredSwitchIfChain>
collectRangeGuardedLoweredSwitchIfChain(const StructuredCFG &Graph,
                                        BlockId HeadId) {
  if (!Graph.dephicationVVars().empty() ||
      !Graph.dephicationIncomings().empty()) {
    return std::nullopt;
  }

  std::optional<LoweredSwitchIfChain> Chain =
      collectRangeGuardedLoweredSwitchIfChainFrom(Graph, HeadId, {});
  if (!Chain.has_value() ||
      !loweredSwitchTargetsOnlyReachedFromChain(Graph, *Chain) ||
      loweredSwitchDefaultReachesChain(Graph, *Chain)) {
    return std::nullopt;
  }
  return Chain;
}

bool rewriteLoweredSwitchIfChain(StructuredCFG &Graph,
                                 const LoweredSwitchIfChain &Chain) {
  StructuredCFG Candidate = Graph;
  CFGBlock *Head = Candidate.getBlock(Chain.Head);
  if (Head == nullptr) {
    return false;
  }

  Head->Terminator = TerminatorKind::Switch;
  Head->Condition = Chain.ComparedValue;
  Head->Successors.clear();
  Head->Successors.push_back(Chain.DefaultTarget);
  Head->Cases.clear();
  for (const LoweredSwitchIfCase &Case : Chain.Cases) {
    Head->Successors.push_back(Case.Target);
    Head->Cases.push_back({Case.Value, Case.Target});
  }

  if (!Candidate.removeBlocks(Chain.RemovedBlocks)) {
    return false;
  }

  Graph = std::move(Candidate);
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

bool reachesBlockWithin(const StructuredCFG &Graph, BlockId Start,
                        BlockId Target, unsigned MaxDepth,
                        std::set<BlockId> &Seen) {
  if (Start == Target) {
    return true;
  }
  if (MaxDepth == 0 || !Seen.insert(Start).second) {
    return false;
  }

  for (BlockId Succ : Graph.successorsOf(Start)) {
    if (reachesBlockWithin(Graph, Succ, Target, MaxDepth - 1, Seen)) {
      return true;
    }
  }
  Seen.erase(Start);
  return false;
}

bool reachesAnyEndBlockWithin(const StructuredCFG &Graph, BlockId Start,
                              unsigned MaxDepth) {
  for (const CFGBlock &Block : Graph.blocks()) {
    if (!Graph.successorsOf(Block.Id).empty()) {
      continue;
    }

    std::set<BlockId> Seen;
    if (reachesBlockWithin(Graph, Start, Block.Id, MaxDepth, Seen)) {
      return true;
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

bool materializeDuplicatedRegion(StructuredCFG &Graph,
                                 const DuplicatedRegion &CopyRegion,
                                 BlockId RegionHead,
                                 const std::vector<BlockId> &ExternalPreds) {
  for (const auto &[Original, Copy] : CopyRegion.Blocks) {
    BlockId OriginalPred = InvalidBlockId;
    BlockId NewPred = InvalidBlockId;
    std::vector<BlockId> OriginalPreds;
    std::vector<BlockId> NewPreds;

    if (Original == RegionHead && !ExternalPreds.empty() &&
        (ExternalPreds.size() == 1 ||
         Graph.hasGroupedPredecessorRewritePayloadMaterializeHook())) {
      bool AllExternalPredsReachHead = true;
      for (BlockId ExternalPred : ExternalPreds) {
        if (!Graph.hasEdge(ExternalPred, Original)) {
          AllExternalPredsReachHead = false;
          break;
        }
      }
      if (AllExternalPredsReachHead) {
        OriginalPreds = ExternalPreds;
        NewPreds = ExternalPreds;
      }
    } else {
      std::vector<BlockId> InternalPreds;
      for (BlockId Pred : predecessorsOf(Graph, Original)) {
        if (CopyRegion.copyOf(Pred) != InvalidBlockId) {
          InternalPreds.push_back(Pred);
        }
      }

      if (InternalPreds.size() == 1) {
        OriginalPred = InternalPreds.front();
        NewPred = CopyRegion.copyOf(OriginalPred);
        OriginalPreds.push_back(OriginalPred);
        NewPreds.push_back(NewPred);
      } else if (InternalPreds.size() > 1 &&
                 Graph.hasGroupedPredecessorRewritePayloadMaterializeHook()) {
        bool AllCopied = true;
        for (BlockId Pred : InternalPreds) {
          BlockId CopyPred = CopyRegion.copyOf(Pred);
          if (CopyPred == InvalidBlockId) {
            AllCopied = false;
            break;
          }
          OriginalPreds.push_back(Pred);
          NewPreds.push_back(CopyPred);
        }
        if (!AllCopied) {
          return false;
        }
      }
    }

    if (OriginalPreds.empty() && OriginalPred != InvalidBlockId &&
        NewPred != InvalidBlockId) {
      OriginalPreds.push_back(OriginalPred);
      NewPreds.push_back(NewPred);
    }

    if (!Graph.materializeBlockBody(Copy, std::move(OriginalPreds),
                                    std::move(NewPreds))) {
      return false;
    }

    const CFGBlock *OriginalBlock = Graph.getBlock(Original);
    const CFGBlock *CopyBlock = Graph.getBlock(Copy);
    if (OriginalBlock == nullptr || CopyBlock == nullptr) {
      return false;
    }
    if (isClosedTerminalForkLeaf(*OriginalBlock) &&
        CopyBlock->Terminator == TerminatorKind::Fallthrough &&
        CopyBlock->Successors.empty() && CopyBlock->Statements.empty()) {
      CFGBlock *MutableCopy = Graph.getBlock(Copy);
      if (MutableCopy == nullptr) {
        return false;
      }
      MutableCopy->Terminator = TerminatorKind::Unreachable;
      CopyBlock = MutableCopy;
    }
    for (std::size_t I = 0; I < OriginalBlock->Statements.size() &&
                            I < CopyBlock->Statements.size(); ++I) {
      Graph.setPayloadOrigin(CopyBlock->Statements[I].Id,
                             OriginalBlock->Statements[I].Id);
    }
    if (OriginalBlock->Condition.isValid() && CopyBlock->Condition.isValid()) {
      Graph.setPayloadOrigin(CopyBlock->Condition.Id, OriginalBlock->Condition.Id);
    }
    for (std::size_t I = 0; I < OriginalBlock->Cases.size() &&
                            I < CopyBlock->Cases.size(); ++I) {
      if (OriginalBlock->Cases[I].Value.isValid() &&
          CopyBlock->Cases[I].Value.isValid()) {
        Graph.setPayloadOrigin(CopyBlock->Cases[I].Value.Id,
                               OriginalBlock->Cases[I].Value.Id);
      }
    }
  }

  return true;
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
  std::optional<DuplicatedRegion> CopyRegion =
      Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   Preds)) {
    for (const auto &[_, OldCopy] : CopyRegion->Blocks) {
      Graph.removeBlock(OldCopy);
    }
    Graph = std::move(Snapshot);
    return false;
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

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   Preds)) {
    for (const auto &[_, OldCopy] : CopyRegion->Blocks) {
      Graph.removeBlock(OldCopy);
    }
    Graph = std::move(Snapshot);
    return false;
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

bool redirectNonSwitchCaseEdges(StructuredCFG &Graph, BlockId OldTarget,
                                BlockId NewTarget,
                                const std::vector<BlockId> &Preds) {
  for (BlockId Pred : Preds) {
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (PredBlock == nullptr ||
        !nonCaseSuccessorReachesBlock(*PredBlock, OldTarget)) {
      return false;
    }
  }

  for (BlockId Pred : Preds) {
    CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (!Graph.redirectDephicationIncomingTarget(Pred, OldTarget, NewTarget)) {
      return false;
    }
    if (PredBlock->Terminator == TerminatorKind::Switch) {
      if (!PredBlock->Successors.empty() &&
          PredBlock->Successors.front() == OldTarget) {
        PredBlock->Successors.front() = NewTarget;
      }
      continue;
    }
    for (BlockId &Succ : PredBlock->Successors) {
      if (Succ == OldTarget) {
        Succ = NewTarget;
      }
    }
  }
  return true;
}

bool copyLinearRegionForNonCasePredecessors(StructuredCFG &Graph,
                                            const LinearRegion &Region,
                                            const std::vector<BlockId> &Preds,
                                            std::vector<BlockId> &OutCopies) {
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion = Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   Preds)) {
    for (const auto &[_, OldCopy] : CopyRegion->Blocks) {
      Graph.removeBlock(OldCopy);
    }
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !redirectNonSwitchCaseEdges(Graph, Region.Head, CopyHead, Preds)) {
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

bool copyLinearRegionForSwitchCases(StructuredCFG &Graph,
                                    const LinearRegion &Region,
                                    const std::vector<BlockId> &SwitchPreds,
                                    std::vector<BlockId> &OutCopies) {
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion = Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   SwitchPreds)) {
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !redirectSwitchCases(Graph, Region.Head, CopyHead, SwitchPreds)) {
    Graph = std::move(Snapshot);
    return false;
  }

  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    OutCopies.push_back(Copy);
  }
  return true;
}

bool blockUsesSwitchCaseEdge(const StructuredCFG &Graph, BlockId Pred,
                             BlockId Target) {
  return hasSwitchCaseEdge(Graph, Pred, Target);
}

bool blockUsesNonSwitchCaseEdge(const StructuredCFG &Graph, BlockId Pred,
                                BlockId Target) {
  const CFGBlock *Block = Graph.getBlock(Pred);
  if (Block == nullptr) {
    return false;
  }
  if (Block->Terminator == TerminatorKind::Switch) {
    return hasSwitchDefaultEdge(Graph, Pred, Target);
  }
  return std::find(Block->Successors.begin(), Block->Successors.end(),
                   Target) != Block->Successors.end();
}

std::set<StructuredGotoEdgeKind>
gotoEdgeKindsFor(const GotoManager &Gotos, BlockId Source, BlockId Target) {
  std::set<StructuredGotoEdgeKind> Kinds;
  for (const StructuredGoto &Goto : Gotos.gotosInBlock(Source)) {
    if (Goto.Target == Target) {
      Kinds.insert(Goto.EdgeKind);
    }
  }
  return Kinds;
}

bool hasCaseDefaultOverlapPredecessor(const StructuredCFG &Graph,
                                      const std::vector<BlockId> &Preds,
                                      BlockId Target) {
  for (BlockId Pred : Preds) {
    const CFGBlock *PredBlock = Graph.getBlock(Pred);
    if (PredBlock != nullptr &&
        PredBlock->Terminator == TerminatorKind::Switch &&
        Graph.switchEdgeKind(Pred, Target) ==
            SwitchEdgeKind::DefaultAndCase) {
      return true;
    }
  }
  return false;
}

bool hasMultipleLogicalPredecessors(const StructuredCFG &Graph,
                                    const std::vector<BlockId> &Preds,
                                    BlockId Target) {
  // predecessorsOf() de-duplicates by block id. A switch whose case and default
  // both reach the same region is still two logical incoming edges for
  // ReturnDuplicatorLow's edge-splitting path.
  return Preds.size() > 1 ||
         hasCaseDefaultOverlapPredecessor(Graph, Preds, Target);
}

bool copyRegionForNonCasePredecessors(StructuredCFG &Graph,
                                      const ReturnRegion &Region,
                                      const std::vector<BlockId> &Preds,
                                      std::vector<BlockId> &OutCopies) {
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion =
      Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   Preds)) {
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !redirectNonSwitchCaseEdges(Graph, Region.Head, CopyHead, Preds)) {
    Graph = std::move(Snapshot);
    return false;
  }

  for (const auto &[_, Copy] : CopyRegion->Blocks) {
    OutCopies.push_back(Copy);
  }
  return true;
}

bool copyRegionForSwitchCases(StructuredCFG &Graph,
                              const ReturnRegion &Region,
                              const std::vector<BlockId> &SwitchPreds,
                              std::vector<BlockId> &OutCopies) {
  StructuredCFG Snapshot = Graph;
  std::optional<DuplicatedRegion> CopyRegion =
      Graph.duplicateRegion(Region.Blocks);
  if (!CopyRegion.has_value()) {
    Graph = std::move(Snapshot);
    return false;
  }

  if (!materializeDuplicatedRegion(Graph, *CopyRegion, Region.Head,
                                   SwitchPreds)) {
    Graph = std::move(Snapshot);
    return false;
  }

  BlockId CopyHead = CopyRegion->copyOf(Region.Head);
  if (CopyHead == InvalidBlockId ||
      !redirectSwitchCases(Graph, Region.Head, CopyHead, SwitchPreds)) {
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

std::size_t statementCountInRegion(const StructuredCFG &Graph,
                                   const ReturnRegion &Region) {
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

std::size_t callCountInRegion(const StructuredCFG &Graph,
                              const LinearRegion &Region) {
  std::size_t Count = 0;
  for (BlockId Id : Region.Blocks) {
    const CFGBlock *Block = Graph.getBlock(Id);
    if (Block == nullptr) {
      return std::numeric_limits<std::size_t>::max();
    }
    Count += Block->CallCount;
  }
  return Count;
}

std::size_t callCountInRegion(const StructuredCFG &Graph,
                              const ReturnRegion &Region) {
  std::size_t Count = 0;
  for (BlockId Id : Region.Blocks) {
    const CFGBlock *Block = Graph.getBlock(Id);
    if (Block == nullptr) {
      return std::numeric_limits<std::size_t>::max();
    }
    Count += Block->CallCount;
  }
  return Count;
}

} // namespace

bool hasInitialSourceGoto(const StructuringEvaluation &Initial,
                          const StructuredGoto &Goto, BlockId Target);

StructuringOptimizationOptions SwitchReusedEntryRewriter::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireStructurableGraph = false;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool SwitchReusedEntryRewriter::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  StructuredCFG Candidate = Graph;
  std::vector<BlockId> EntryIds;
  EntryIds.reserve(Candidate.blocks().size());
  for (const CFGBlock &Entry : Candidate.blocks()) {
    EntryIds.push_back(Entry.Id);
  }

  std::map<BlockId, std::vector<BlockId>> SwitchPredsByEntry;
  for (BlockId EntryId : EntryIds) {
    const CFGBlock *Entry = Candidate.getBlock(EntryId);
    if (Entry == nullptr) {
      continue;
    }

    std::vector<BlockId> SwitchPreds;
    for (const CFGBlock &PredBlock : Candidate.blocks()) {
      if (switchCaseEdgeReachesBlock(PredBlock, EntryId)) {
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
    const CFGBlock *Entry = Candidate.getBlock(EntryId);
    if (Entry == nullptr) {
      continue;
    }

    bool EntryChanged = false;
    for (std::size_t I = 1; I < SwitchPreds.size(); ++I) {
      BlockId Pred = SwitchPreds[I];
      const CFGBlock *PredBlock = Candidate.getBlock(Pred);
      if (PredBlock == nullptr ||
          !switchCaseEdgeReachesBlock(*PredBlock, EntryId)) {
        continue;
      }

      BlockId Goto = Candidate.createSyntheticGoto(
          Pred, EntryId, CFGBlockCreator::SAILRDeoptimization);
      if (Goto == InvalidBlockId ||
          !redirectSwitchCases(Candidate, EntryId, Goto, {Pred})) {
        continue;
      }

      EntryChanged = true;
    }

    if (EntryChanged) {
      Changed = true;
    }
  }

  if (Changed) {
    Graph = std::move(Candidate);
  }
  return Changed;
}

StructuringOptimizationOptions LoweredSwitchSimplifier::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireStructurableGraph = false;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool LoweredSwitchSimplifier::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  bool Changed = false;
  bool HadExistingSwitch = graphHasSwitchTerminator(Graph);
  std::vector<BlockId> BranchIds;
  BranchIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    if (Block.Terminator == TerminatorKind::Branch) {
      BranchIds.push_back(Block.Id);
    }
  }

  for (BlockId BranchId : BranchIds) {
    std::optional<LoweredSwitchIfChain> Chain =
        collectRangeGuardedLoweredSwitchIfChain(Graph, BranchId);
    if (!Chain.has_value()) {
      Chain = collectLoweredSwitchIfChain(Graph, BranchId);
    }
    if (!Chain.has_value()) {
      continue;
    }
    if (!loweredSwitchChainPassesAngrHeuristics(*Chain, HadExistingSwitch)) {
      continue;
    }
    if (rewriteLoweredSwitchIfChain(Graph, *Chain)) {
      Changed = true;
    }
  }

  std::vector<BlockId> TargetIds;
  TargetIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    TargetIds.push_back(Block.Id);
  }

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

    std::vector<BlockId> AllPreds = predecessorsOf(Graph, TargetId);
    bool HasNonCaseReuse = false;
    for (BlockId Pred : AllPreds) {
      const CFGBlock *PredBlock = Graph.getBlock(Pred);
      if (PredBlock != nullptr &&
          nonCaseSuccessorReachesBlock(*PredBlock, TargetId)) {
        HasNonCaseReuse = true;
        break;
      }
    }
    if (Preds.empty() ||
        (Preds.size() == 1 && AllPreds.size() == 1 && !HasNonCaseReuse)) {
      continue;
    }

    LinearRegion Region = findLinearCopyRegion(Graph, TargetId);
    if (Region.Blocks.size() <= 1) {
      continue;
    }

    StructuredCFG Candidate = Graph;
    std::vector<std::vector<BlockId>> PredComponents =
        materializePredecessorComponents(Graph, Preds);
    std::vector<BlockId> UpdatedPreds;
    std::vector<BlockId> Copies;
    bool Failed = false;
    bool DeleteOriginal = sameBlockSet(AllPreds, Preds);
    for (BlockId Pred : AllPreds) {
      const CFGBlock *PredBlock = Graph.getBlock(Pred);
      if (PredBlock == nullptr ||
          nonCaseSuccessorReachesBlock(*PredBlock, TargetId)) {
        DeleteOriginal = false;
        break;
      }
    }
    if (DeleteOriginal) {
      StructuredCFG DeletionProbe = Candidate;
      if (!DeletionProbe.removeBlocks(Region.Blocks)) {
        continue;
      }
    }
    for (const std::vector<BlockId> &Component : PredComponents) {
      if (!copyLinearRegionForSwitchCases(Candidate, Region, Component,
                                          Copies)) {
        Failed = true;
        break;
      }
      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(), Component.end());
    }

    if (Failed) {
      continue;
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
  Options.RequireStructurableGraph = false;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.MaxOptIters = 2;
  return Options;
}

bool SwitchDefaultCaseDuplicator::runOnGraph(
    StructuredCFG &Graph, const StructuringEvaluation &Current) {
  (void)Current;

  StructuredCFG Candidate = Graph;
  std::map<BlockId, std::vector<BlockId>> SwitchPredsByDefault;
  std::map<BlockId, BlockId> KeepPredForDefault;
  std::map<BlockId, LinearRegion> DefaultRegions;

  for (const CFGBlock &Block : Candidate.blocks()) {
    BlockId DefaultTarget = defaultSwitchSuccessor(Candidate, Block);
    if (DefaultTarget == InvalidBlockId) {
      continue;
    }

    const CFGBlock *DefaultBlock = Candidate.getBlock(DefaultTarget);
    if (DefaultBlock == nullptr) {
      continue;
    }
    SwitchPredsByDefault[DefaultTarget].push_back(Block.Id);
    if (Candidate.successorsOf(DefaultTarget).size() != 1) {
      continue;
    }

    std::vector<BlockId> Preds = predecessorsOf(Candidate, DefaultTarget);
    if (Preds.size() <= 1) {
      continue;
    }

    LinearRegion Region = findLinearRegionFromHead(Candidate, DefaultTarget);
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

    StructuredCFG DefaultCandidate = Candidate;
    bool RewroteDefault = false;
    bool Failed = false;
    for (BlockId SwitchPred : SwitchPreds) {
      if (!DefaultCandidate.hasEdge(SwitchPred, DefaultTarget)) {
        Failed = true;
        break;
      }

      BlockId RewriteBlock =
          SharedDefaultMode == SharedDefaultRewriteMode::SyntheticGoto
              ? DefaultCandidate.createSyntheticGotoEdge(
                    SwitchPred, DefaultTarget,
                    CFGBlockCreator::SAILRDeoptimization)
              : DefaultCandidate.createSyntheticForwarder(
                    SwitchPred, DefaultTarget,
                    CFGBlockCreator::SAILRDeoptimization);
      if (RewriteBlock == InvalidBlockId ||
          !replaceDefaultSwitchSuccessor(DefaultCandidate, SwitchPred, DefaultTarget,
                                         RewriteBlock)) {
        if (RewriteBlock != InvalidBlockId) {
          DefaultCandidate.removeBlock(RewriteBlock);
        }
        Failed = true;
        break;
      }

      RewroteDefault = true;
    }

    if (Failed || !RewroteDefault) {
      continue;
    }

    Candidate = std::move(DefaultCandidate);
    Changed = true;
  }

  for (const auto &[DefaultTarget, KeepPred] : KeepPredForDefault) {
    const CFGBlock *KeepSwitch = Candidate.getBlock(KeepPred);
    std::vector<BlockId> Preds = predecessorsOf(Candidate, DefaultTarget);
    std::vector<BlockId> PredsToUpdate;
    for (BlockId Pred : Preds) {
      const CFGBlock *PredBlock = Candidate.getBlock(Pred);
      if (PredBlock != nullptr &&
          PredBlock->Terminator == TerminatorKind::Switch &&
          switchCaseReachesBlock(*PredBlock, DefaultTarget)) {
        continue;
      }
      if (PredBlock != nullptr &&
          (PredBlock->CopyKind == CFGBlockCopyKind::SyntheticForwarder ||
           PredBlock->CopyKind == CFGBlockCopyKind::SyntheticGoto) &&
          PredBlock->SyntheticTarget == DefaultTarget) {
        continue;
      }
      if (Pred == KeepPred) {
        continue;
      }
      if (KeepSwitch != nullptr &&
          reachesBlockFromNonDefaultSwitchSuccessor(Candidate, *KeepSwitch,
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

    std::vector<BlockId> Copies;
    std::vector<std::vector<BlockId>> PredComponents =
        materializePredecessorComponents(Candidate, PredsToUpdate);
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
      Changed = true;
    }
  }

  if (Changed) {
    Graph = std::move(Candidate);
  }
  return Changed;
}

StructuringOptimizationOptions DuplicationReverter::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 5;
  return Options;
}

bool DuplicationReverter::runOnGraph(StructuredCFG &Graph,
                                     const StructuringEvaluation &Current) {
  if (revertGotoRelatedDuplicateBranchArms(Graph, Current)) {
    return true;
  }
  if (revertGotoRelatedCommonLinearRegionTail(Graph, Current)) {
    return true;
  }
  if (revertGotoRelatedCommonStatementTail(Graph, Current)) {
    return true;
  }

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
          !sameBlockShapeByReference(Graph, *Keep, *Drop)) {
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

GotoManager DuplicationReverter::getNewGotos(
    const StructuredCFG &Cfg, const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  constexpr unsigned MaxEndpointDistance = 5;
  std::vector<StructuredGoto> Filtered;

  for (const StructuredGoto &Goto : Current.Gotos.gotos()) {
    StructuredGoto Normalized = Goto;
    const CFGBlock *Target = Cfg.getBlock(Normalized.Target);
    if (Target != nullptr && Target->Origin == CFGBlockOrigin::Copied &&
        Target->SourceBlock != InvalidBlockId &&
        hasInitialSourceGoto(Initial, Normalized, Target->SourceBlock)) {
      Normalized.Target = Target->SourceBlock;
      Target = Cfg.getBlock(Normalized.Target);
    }
    if (Target == nullptr) {
      Filtered.push_back(Normalized);
      continue;
    }
    if (Cfg.successorsOf(Target->Id).empty() ||
        reachesAnyEndBlockWithin(Cfg, Target->Id, MaxEndpointDistance)) {
      Filtered.push_back(Normalized);
    }
  }

  return GotoManager::fromGotos(Filtered);
}

bool hasInitialSourceGoto(const StructuringEvaluation &Initial,
                          const StructuredGoto &Goto, BlockId Target) {
  for (const StructuredGoto &InitialGoto : Initial.Gotos.gotos()) {
    if (InitialGoto.Source == Goto.Source &&
        InitialGoto.Target == Target &&
        InitialGoto.EdgeKind == Goto.EdgeKind) {
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

GotoManager ReturnDuplicatorLow::getNewGotos(
    const StructuredCFG &Cfg, const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  std::vector<StructuredGoto> Normalized;
  for (StructuredGoto Goto : Current.Gotos.gotos()) {
    const CFGBlock *Target = Cfg.getBlock(Goto.Target);
    if (Target != nullptr && Target->Origin == CFGBlockOrigin::Copied &&
        Target->SourceBlock != InvalidBlockId &&
        hasInitialSourceGoto(Initial, Goto, Target->SourceBlock)) {
      Goto.Target = Target->SourceBlock;
    }
    Normalized.push_back(Goto);
  }
  return GotoManager::fromGotos(Normalized);
}

bool ReturnDuplicatorLow::runOnGraph(StructuredCFG &Graph,
                                     const StructuringEvaluation &Current) {
  if (Graph.blocks().size() > MaxFunctionBlocks) {
    return false;
  }

  std::map<BlockId, ReturnRegion> Regions;
  // Angr does not gate return-region discovery on payload rewrite hooks. The
  // hook only matters when we need predecessor-sensitive payload rewriting.
  bool AllowBranchReturnRegion = true;

  for (const CFGBlock &Block : Graph.blocks()) {
    ReturnRegion Region =
        findLinearReturnRegion(Graph, Block.Id, AllowBranchReturnRegion);
    if (Region.Head == InvalidBlockId) {
      continue;
    }
    std::vector<BlockId> Preds = externalPredecessorsOf(Graph, Region);
    if (!hasMultipleLogicalPredecessors(Graph, Preds, Region.Head)) {
      continue;
    }
    Regions.emplace(Region.Head, std::move(Region));
  }

  bool Changed = false;
  for (auto &Entry : Regions) {
    const ReturnRegion &Region = Entry.second;
    if (statementCountInRegion(Graph, Region) > MaxDuplicatedStatements ||
        callCountInRegion(Graph, Region) > MaxDuplicatedCalls) {
      continue;
    }

    std::vector<BlockId> CurrentPreds = externalPredecessorsOf(Graph, Region);
    if (!hasMultipleLogicalPredecessors(Graph, CurrentPreds, Region.Head)) {
      continue;
    }
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
    } else {
      PredsToUpdate =
          expandToConnectedPredecessorComponents(Graph, CurrentPreds,
                                                 PredsToUpdate);
      PredsToUpdate = expandToSiblingSwitchJumpPredecessors(
          Graph, CurrentPreds, PredsToUpdate, Region);
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
        materializePredecessorComponents(Graph, PredsToUpdate);
    std::vector<BlockId> UpdatedPreds;
    bool Failed = false;
    for (const std::vector<BlockId> &Component : PredComponents) {
      std::vector<BlockId> GenericPreds;
      std::vector<BlockId> CasePreds;
      std::vector<BlockId> NonCasePreds;
      bool ComponentFailed = false;
      for (BlockId Pred : Component) {
        const CFGBlock *PredBlock = Candidate.getBlock(Pred);
        bool UsesCaseEdge =
            blockUsesSwitchCaseEdge(Candidate, Pred, Region.Head);
        bool UsesNonCaseEdge =
            PredBlock != nullptr &&
            nonCaseSuccessorReachesBlock(*PredBlock, Region.Head);
        if (PredBlock != nullptr &&
            PredBlock->Terminator == TerminatorKind::Switch &&
            UsesCaseEdge && UsesNonCaseEdge) {
          std::set<StructuredGotoEdgeKind> Kinds =
              gotoEdgeKindsFor(Current.Gotos, Pred, Region.Head);
          bool HasUnknown =
              Kinds.empty() ||
              Kinds.count(StructuredGotoEdgeKind::Unknown) != 0;
          bool HasCase =
              Kinds.count(StructuredGotoEdgeKind::SwitchCase) != 0;
          bool HasDefault =
              Kinds.count(StructuredGotoEdgeKind::SwitchDefault) != 0;
          if (HasUnknown || (!HasCase && !HasDefault)) {
            ComponentFailed = true;
            break;
          }
          if (HasCase) {
            CasePreds.push_back(Pred);
          }
          if (HasDefault) {
            NonCasePreds.push_back(Pred);
          }
          continue;
        }
        GenericPreds.push_back(Pred);
      }

      if (ComponentFailed) {
        Failed = true;
        break;
      }

      if (!GenericPreds.empty() &&
          !copyRegionForPredecessors(Candidate, Region, GenericPreds,
                                     Copies)) {
        Failed = true;
        break;
      }
      if (!CasePreds.empty() &&
          !copyRegionForSwitchCases(Candidate, Region, CasePreds, Copies)) {
        Failed = true;
        break;
      }
      if (!NonCasePreds.empty() &&
          !copyRegionForNonCasePredecessors(Candidate, Region, NonCasePreds,
                                            Copies)) {
        Failed = true;
        break;
      }
      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(),
                          Component.end());
    }

    if (Failed) {
      continue;
    }

    std::sort(UpdatedPreds.begin(), UpdatedPreds.end());
    UpdatedPreds.erase(std::unique(UpdatedPreds.begin(), UpdatedPreds.end()),
                       UpdatedPreds.end());

    if (DeleteOriginal && predecessorsOf(Candidate, Region.Head).empty()) {
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

StructuringOptimizationOptions ReturnDeduplicator::defaultOptions() {
  StructuringOptimizationOptions Options;
  Options.RequireStructurableGraph = false;
  Options.PreventNewGotos = true;
  Options.RequireGotos = false;
  Options.MustImproveRelativeQuality = false;
  Options.EvaluateInputBeforeRun = false;
  Options.MaxInputBlocks = 500;
  return Options;
}

bool canDeduplicateReturnArm(const StructuredCFG &Graph, BlockId Branch,
                             BlockId Arm) {
  const CFGBlock *Block = Graph.getBlock(Arm);
  if (Block == nullptr || Block->Terminator != TerminatorKind::Return ||
      Block->Successors.empty() == false || Block->Statements.empty()) {
    return false;
  }

  std::vector<BlockId> Preds = predecessorsOf(Graph, Arm);
  return Preds.size() == 1 && Preds.front() == Branch;
}

bool sameTailReturnPayload(const StructuredCFG &Graph, const CFGBlock &Lhs,
                           const CFGBlock &Rhs) {
  if (Lhs.Statements.empty() || Rhs.Statements.empty()) {
    return false;
  }
  return samePayload(Graph, Lhs.Statements.back(), Rhs.Statements.back());
}

bool deduplicateReturnArmGroup(StructuredCFG &Graph, BlockId Parent,
                               const std::vector<BlockId> &ArmIds) {
  if (ArmIds.size() < 2) {
    return false;
  }

  std::set<BlockId> UniqueArms(ArmIds.begin(), ArmIds.end());
  if (UniqueArms.size() != ArmIds.size()) {
    return false;
  }

  const CFGBlock *FirstArm = Graph.getBlock(ArmIds.front());
  if (FirstArm == nullptr || !canDeduplicateReturnArm(Graph, Parent,
                                                      ArmIds.front())) {
    return false;
  }
  for (BlockId ArmId : ArmIds) {
    const CFGBlock *Arm = Graph.getBlock(ArmId);
    if (Arm == nullptr || !canDeduplicateReturnArm(Graph, Parent, ArmId) ||
        !sameTailReturnPayload(Graph, *FirstArm, *Arm)) {
      return false;
    }
  }

  StructuredCFG Candidate = Graph;
  CFGBlock *FirstCandidateArm = Candidate.getBlock(ArmIds.front());
  if (FirstCandidateArm == nullptr) {
    return false;
  }

  PayloadRef SharedReturn = FirstCandidateArm->Statements.back();
  for (BlockId ArmId : ArmIds) {
    CFGBlock *Arm = Candidate.getBlock(ArmId);
    if (Arm == nullptr || Arm->Statements.empty()) {
      return false;
    }
    Arm->Statements.pop_back();
    Arm->Terminator = TerminatorKind::Fallthrough;
  }

  CFGBlock ReturnBlock;
  ReturnBlock.Origin = CFGBlockOrigin::Synthetic;
  ReturnBlock.CopyKind = CFGBlockCopyKind::None;
  ReturnBlock.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  ReturnBlock.Statements = {SharedReturn};
  ReturnBlock.Terminator = TerminatorKind::Return;
  BlockId ReturnId = Candidate.addBlock(std::move(ReturnBlock));
  for (BlockId ArmId : ArmIds) {
    CFGBlock *Arm = Candidate.getBlock(ArmId);
    if (Arm == nullptr) {
      return false;
    }
    Arm->Successors = {ReturnId};
  }

  Graph = std::move(Candidate);
  return true;
}

bool ReturnDeduplicator::runOnGraph(StructuredCFG &Graph,
                                    const StructuringEvaluation &Current) {
  (void)Current;

  std::vector<BlockId> BlockIds;
  BlockIds.reserve(Graph.blocks().size());
  for (const CFGBlock &Block : Graph.blocks()) {
    BlockIds.push_back(Block.Id);
  }
  std::sort(BlockIds.begin(), BlockIds.end());

  for (BlockId BranchId : BlockIds) {
    const CFGBlock *Branch = Graph.getBlock(BranchId);
    if (Branch == nullptr) {
      continue;
    }

    if (Branch->Terminator == TerminatorKind::Branch &&
        Branch->Successors.size() == 2 &&
        Branch->Successors[0] != Branch->Successors[1] &&
        deduplicateReturnArmGroup(
            Graph, BranchId, {Branch->Successors[0], Branch->Successors[1]})) {
      return true;
    }

    if (Branch->Terminator != TerminatorKind::Switch ||
        Branch->Successors.size() < 3) {
      continue;
    }

    // Keep case/default overlap out of this narrow pass. ReturnDuplicatorLow has
    // explicit switch-edge logic for that shape; ReturnDeduplicator only shares
    // distinct private return arms here.
    std::set<BlockId> UniqueSuccessors(Branch->Successors.begin(),
                                       Branch->Successors.end());
    if (UniqueSuccessors.size() != Branch->Successors.size()) {
      continue;
    }

    std::vector<BlockId> ReturnArms;
    for (BlockId Succ : Branch->Successors) {
      if (canDeduplicateReturnArm(Graph, BranchId, Succ)) {
        ReturnArms.push_back(Succ);
      }
    }
    for (unsigned I = 0; I < ReturnArms.size(); ++I) {
      const CFGBlock *Base = Graph.getBlock(ReturnArms[I]);
      if (Base == nullptr) {
        continue;
      }
      std::vector<BlockId> Group = {ReturnArms[I]};
      for (unsigned J = I + 1; J < ReturnArms.size(); ++J) {
        const CFGBlock *Other = Graph.getBlock(ReturnArms[J]);
        if (Other != nullptr && sameTailReturnPayload(Graph, *Base, *Other)) {
          Group.push_back(ReturnArms[J]);
        }
      }
      if (deduplicateReturnArmGroup(Graph, BranchId, Group)) {
        return true;
      }
    }
  }

  return false;
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
  std::map<std::pair<BlockId, BlockId>, std::set<StructuredGotoEdgeKind>>
      GotoEdgeKinds;

  for (const CFGBlock &Block : Graph.blocks()) {
    std::vector<StructuredGoto> Gotos = Current.Gotos.gotosInBlock(Block.Id);
    if (Gotos.empty()) {
      continue;
    }

    BlockId Target = Gotos.front().Target;
    bool SameTarget = std::all_of(
        Gotos.begin(), Gotos.end(),
        [Target](const StructuredGoto &Goto) { return Goto.Target == Target; });
    if (!SameTarget) {
      continue;
    }

    if (!Graph.hasEdge(Block.Id, Target)) {
      continue;
    }

    LinearRegion Region = findLinearRegionFromHead(Graph, Target);
    if (Region.Head == InvalidBlockId || Graph.successorsOf(Target).size() != 1) {
      continue;
    }
    if (statementCountInRegion(Graph, Region) > MaxDuplicatedStatements ||
        callCountInRegion(Graph, Region) > MaxDuplicatedCalls) {
      continue;
    }

    PredsByTarget[Target].push_back(Block.Id);
    std::set<StructuredGotoEdgeKind> &Kinds = GotoEdgeKinds[{Block.Id, Target}];
    for (const StructuredGoto &Goto : Gotos) {
      Kinds.insert(Goto.EdgeKind);
    }
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

    bool DeleteOriginal = sameBlockSet(predecessorsOf(Graph, Target),
                                       PredsToUpdate);
    StructuredCFG Candidate = Graph;
    if (DeleteOriginal) {
      StructuredCFG DeletionProbe = Candidate;
      if (!DeletionProbe.removeBlocks(Region.Blocks)) {
        continue;
      }
    }

    std::vector<BlockId> UpdatedPreds;
    std::vector<std::vector<BlockId>> PredComponents =
        materializePredecessorComponents(Graph, PredsToUpdate);
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
      std::vector<BlockId> CasePreds;
      std::vector<BlockId> NonCasePreds;
      bool HasAmbiguousSwitchPred = false;
      for (BlockId Pred : Component) {
        bool UsesCaseEdge = blockUsesSwitchCaseEdge(Candidate, Pred, Target);
        bool UsesNonCaseEdge =
            blockUsesNonSwitchCaseEdge(Candidate, Pred, Target);
        if (UsesCaseEdge && UsesNonCaseEdge) {
          const std::set<StructuredGotoEdgeKind> *Kinds = nullptr;
          auto KindIt = GotoEdgeKinds.find({Pred, Target});
          if (KindIt != GotoEdgeKinds.end()) {
            Kinds = &KindIt->second;
          }
          bool HasCaseKind =
              Kinds != nullptr &&
              Kinds->count(StructuredGotoEdgeKind::SwitchCase) != 0;
          bool HasDefaultKind =
              Kinds != nullptr &&
              Kinds->count(StructuredGotoEdgeKind::SwitchDefault) != 0;
          bool HasUnknownKind =
              Kinds == nullptr ||
              Kinds->count(StructuredGotoEdgeKind::Unknown) != 0;
          if (HasUnknownKind || (!HasCaseKind && !HasDefaultKind)) {
            HasAmbiguousSwitchPred = true;
            break;
          }
          if (HasCaseKind) {
            CasePreds.push_back(Pred);
          }
          if (HasDefaultKind) {
            NonCasePreds.push_back(Pred);
          }
          continue;
        }
        if (UsesCaseEdge) {
          CasePreds.push_back(Pred);
        }
        if (UsesNonCaseEdge) {
          NonCasePreds.push_back(Pred);
        }
      }
      if (HasAmbiguousSwitchPred) {
        continue;
      }

      if (!CasePreds.empty() &&
          !copyLinearRegionForSwitchCases(Candidate, Region, CasePreds,
                                          RegionCopies)) {
        continue;
      }
      if (!NonCasePreds.empty() &&
          !copyLinearRegionForNonCasePredecessors(Candidate, Region,
                                                  NonCasePreds,
                                                  RegionCopies)) {
        continue;
      }

      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(),
                          Component.end());
    }

    if (DeleteOriginal && predecessorsOf(Candidate, Target).empty()) {
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

SAILRDeoptimizationPipelineOptions &
defaultSAILRDeoptimizationPipelineOptions() {
  static SAILRDeoptimizationPipelineOptions Options;
  return Options;
}

void setDefaultSAILRDeoptimizationPipelineOptions(
    SAILRDeoptimizationPipelineOptions Options) {
  defaultSAILRDeoptimizationPipelineOptions() = Options;
}

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline(
    SAILRDeoptimizationPipelineOptions Options) {
  StructuringOptimizationPipeline Pipeline;
  // Angr runs both jump-table graph-creation passes before the SAILR
  // during-region passes. NotDec folds them into this shared CFG pipeline, but
  // keeps the same stage order.
  Pipeline.addPass(std::make_unique<SwitchDefaultCaseDuplicator>(
      SwitchDefaultCaseDuplicator::defaultOptions(), Options.SharedDefaultMode));
  Pipeline.addPass(std::make_unique<SwitchReusedEntryRewriter>());
  Pipeline.addPass(std::make_unique<DuplicationReverter>());
  Pipeline.addPass(std::make_unique<LoweredSwitchSimplifier>());
  Pipeline.addPass(std::make_unique<ReturnDuplicatorLow>());
  Pipeline.addPass(std::make_unique<ReturnDeduplicator>());
  Pipeline.addPass(std::make_unique<CrossJumpReverter>());
  return Pipeline;
}

} // namespace notdec::backend::structuring
