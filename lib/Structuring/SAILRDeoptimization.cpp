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

constexpr std::size_t MaxLinearRegionMergeBlocks = 12;

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
      LeftPath.size() < 2 || RightPath.size() < 2) {
    return false;
  }

  for (std::size_t I = 0; I + 1 < LeftPath.size(); ++I) {
    for (std::size_t J = 0; J + 1 < RightPath.size(); ++J) {
      if (LeftPath[I] == RightPath[J]) {
        return false;
      }
    }
  }

  BlockId LeftTerminalPred = LeftPath[LeftPath.size() - 2];
  BlockId RightTerminalPred = RightPath[RightPath.size() - 2];
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
      if (Block->Successors.size() != 2 || Side.Blocks.empty()) {
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
  if (Sides[0].Blocks.empty() || Sides[1].Blocks.empty()) {
    return false;
  }
  if (!containsBlock(HeadBlock->Successors, Sides[0].Blocks.back()) ||
      !containsBlock(HeadBlock->Successors, Sides[1].Blocks.back())) {
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

    BlockId BestOther = InvalidBlockId;
    std::size_t BestSuffix = 0;
    for (BlockId OtherId : BlockIds) {
      std::size_t CommonSuffix = 0;
      if (!commonStatementTailCandidate(Graph, Goto.Target, OtherId,
                                        CommonSuffix)) {
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

    StructuredCFG Candidate = Graph;
    if (!extractCommonStatementTail(Candidate, Goto.Target, BestOther,
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

bool switchDefaultAlsoCaseTarget(const CFGBlock &Block, BlockId DefaultTarget) {
  if (DefaultTarget == InvalidBlockId) {
    return false;
  }
  return switchCaseReachesBlock(Block, DefaultTarget);
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
  const CFGBlock *Block = Graph.getBlock(Pred);
  if (Block == nullptr || Block->Terminator != TerminatorKind::Switch) {
    return false;
  }
  return std::any_of(Block->Cases.begin(), Block->Cases.end(),
                     [Target](const SwitchCase &Case) {
                       return Case.Target == Target;
                     });
}

bool blockUsesNonSwitchCaseEdge(const StructuredCFG &Graph, BlockId Pred,
                                BlockId Target) {
  const CFGBlock *Block = Graph.getBlock(Pred);
  if (Block == nullptr) {
    return false;
  }
  return std::find(Block->Successors.begin(), Block->Successors.end(),
                   Target) != Block->Successors.end();
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
          defaultSwitchSuccessor(Candidate, *PredBlock) == DefaultTarget &&
          switchDefaultAlsoCaseTarget(*PredBlock, DefaultTarget)) {
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
    if (InitialGoto.Source == Goto.Source && InitialGoto.Target == Target) {
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
    if (Preds.size() <= 1) {
      continue;
    }
    Regions.emplace(Region.Head, std::move(Region));
  }

  bool Changed = false;
  for (auto &Entry : Regions) {
    const ReturnRegion &Region = Entry.second;
    if (statementCountInRegion(Graph, Region) > MaxDuplicatedStatements) {
      continue;
    }

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
    } else {
      PredsToUpdate =
          expandToConnectedPredecessorComponents(Graph, CurrentPreds,
                                                 PredsToUpdate);
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
      if (!copyRegionForPredecessors(Candidate, Region, Component, Copies)) {
        Failed = true;
        break;
      }
      UpdatedPreds.insert(UpdatedPreds.end(), Component.begin(),
                          Component.end());
    }

    if (Failed) {
      continue;
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
    if (statementCountInRegion(Graph, Region) > MaxDuplicatedStatements) {
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
          if (HasCaseKind && !HasUnknownKind) {
            CasePreds.push_back(Pred);
          } else if (HasDefaultKind && !HasUnknownKind) {
            NonCasePreds.push_back(Pred);
          } else {
            HasAmbiguousSwitchPred = true;
            break;
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
  Pipeline.addPass(std::make_unique<CrossJumpReverter>());
  return Pipeline;
}

} // namespace notdec::backend::structuring
