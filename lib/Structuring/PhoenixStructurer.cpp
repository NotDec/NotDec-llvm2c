#include "notdec-backends/Structuring/PhoenixStructurer.h"

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <map>
#include <set>
#include <vector>

namespace notdec::backend::structuring {

namespace {

StructuredNode makeControlTransfer(BlockId Target, VirtualEdgeKind Kind) {
  StructuredNode Node;
  Node.Target = Target;
  switch (Kind) {
  case VirtualEdgeKind::Goto:
    Node.Kind = StructuredNodeKind::Goto;
    break;
  case VirtualEdgeKind::Break:
    Node.Kind = StructuredNodeKind::Break;
    break;
  case VirtualEdgeKind::Continue:
    Node.Kind = StructuredNodeKind::Continue;
    break;
  }
  return Node;
}

StructuredNode makeBreak() {
  StructuredNode Node;
  Node.Kind = StructuredNodeKind::Break;
  return Node;
}

bool isTerminalBlock(const StructuredCFG &Cfg, BlockId Target);
bool allSuccessorsAreTerminal(const StructuredCFG &Cfg, const Region &R);

void appendBlockLabel(BlockId Id, StructuredNode &Sequence,
                      StructuredTree &Tree) {
  StructuredNode Label;
  Label.Kind = StructuredNodeKind::Label;
  Label.Block = Id;
  Sequence.Children.push_back(Tree.addNode(std::move(Label)));
}

void appendBlockBody(const StructuredCFG &Cfg, BlockId Id,
                     StructuredNode &Sequence, StructuredTree &Tree) {
  const CFGBlock *Block = Cfg.getBlock(Id);
  if (Block == nullptr) {
    return;
  }

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = Block->Id;
  Body.Statements = Block->Statements;
  Sequence.Children.push_back(Tree.addNode(std::move(Body)));
}

NodeId buildLinearNode(const StructuredCFG &Cfg,
                       const std::vector<BlockId> &Blocks,
                       StructuredTree &Tree) {
  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  for (BlockId Block : Blocks) {
    appendBlockLabel(Block, Sequence, Tree);
    appendBlockBody(Cfg, Block, Sequence, Tree);
  }
  return Tree.addNode(std::move(Sequence));
}

void appendRegionNode(const StructuredCFG &Cfg, const MutableRegionNode &Node,
                      StructuredNode &Sequence, StructuredTree &Tree) {
  if (Node.StructuredRoot != InvalidNodeId) {
    Sequence.Children.push_back(Node.StructuredRoot);
    return;
  }
  for (BlockId Block : Node.Blocks) {
    appendBlockLabel(Block, Sequence, Tree);
    appendBlockBody(Cfg, Block, Sequence, Tree);
  }
}

NodeId buildSequenceNode(const StructuredCFG &Cfg,
                         const std::vector<const MutableRegionNode *> &Nodes,
                         StructuredTree &Tree) {
  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  for (const MutableRegionNode *Node : Nodes) {
    if (Node != nullptr) {
      appendRegionNode(Cfg, *Node, Sequence, Tree);
    }
  }
  return Tree.addNode(std::move(Sequence));
}

NodeId buildLoopBody(const StructuredCFG &Cfg, const MutableRegionNode &Node,
                     StructuredTree &Tree) {
  if (Node.StructuredRoot != InvalidNodeId) {
    return Node.StructuredRoot;
  }
  return buildLinearNode(Cfg, Node.Blocks, Tree);
}

bool isFallthroughTo(const StructuredCFG &Cfg, GraphNodeId FromId,
                     GraphNodeId ToId, const MutableRegionGraph &Graph) {
  const MutableRegionNode *From = Graph.getNode(FromId);
  const MutableRegionNode *To = Graph.getNode(ToId);
  if (From == nullptr || From->Blocks.empty() || To == nullptr ||
      To->Blocks.empty()) {
    return false;
  }

  const CFGBlock *Block = Cfg.getBlock(From->Blocks.back());
  return Block != nullptr && Block->Terminator == TerminatorKind::Fallthrough &&
         Block->Successors.size() == 1 && Block->Successors[0] == To->Blocks[0];
}

bool containsBlock(const std::vector<BlockId> &Blocks, BlockId Id) {
  return std::find(Blocks.begin(), Blocks.end(), Id) != Blocks.end();
}

bool nodeIntersectsBlocks(const MutableRegionNode &Node,
                          const std::vector<BlockId> &Blocks) {
  for (BlockId Block : Node.Blocks) {
    if (containsBlock(Blocks, Block)) {
      return true;
    }
  }
  return false;
}

bool nodeContainedByBlocks(const MutableRegionNode &Node,
                           const std::vector<BlockId> &Blocks) {
  for (BlockId Block : Node.Blocks) {
    if (!containsBlock(Blocks, Block)) {
      return false;
    }
  }
  return true;
}

bool collapseCrossesNaturalLoopBoundary(
    const StructuredCFG &Cfg,
    const RegionTree &Regions, const Region &R, const MutableRegionGraph &Graph,
    const std::vector<GraphNodeId> &Members) {
  if (R.Kind != RegionKind::Root) {
    return false;
  }

  for (RegionId ChildId : R.Children) {
    const Region *Child = Regions.getRegion(ChildId);
    if (Child == nullptr || Child->Kind != RegionKind::NaturalLoop) {
      continue;
    }

    bool HasLoopMember = false;
    bool HasLoopExitMember = false;
    for (GraphNodeId Id : Members) {
      const MutableRegionNode *Node = Graph.getNode(Id);
      if (Node == nullptr) {
        continue;
      }
      bool Intersects = nodeIntersectsBlocks(*Node, Child->Blocks);
      bool Contained = nodeContainedByBlocks(*Node, Child->Blocks);
      HasLoopMember |= Intersects;
      if (!Contained) {
        for (BlockId Block : Node->Blocks) {
          if (containsBlock(Child->Successors, Block) ||
              (allSuccessorsAreTerminal(Cfg, *Child) &&
               isTerminalBlock(Cfg, Block))) {
            HasLoopExitMember = true;
          }
        }
      }
    }
    if (HasLoopMember && HasLoopExitMember) {
      return true;
    }
  }
  return false;
}

bool reduceSequenceOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                        const RegionTree &Regions, const Region &R,
                        StructuredTree &Tree) {
  for (GraphNodeId FromId : Graph.activeNodes()) {
    const MutableRegionNode *From = Graph.getNode(FromId);
    if (From == nullptr || From->Succs.size() != 1) {
      continue;
    }

    GraphNodeId ToId = From->Succs[0];
    const MutableRegionNode *To = Graph.getNode(ToId);
    if (To == nullptr || To->Preds.size() != 1 ||
        !isFallthroughTo(Cfg, FromId, ToId, Graph)) {
      continue;
    }
    if (collapseCrossesNaturalLoopBoundary(Cfg, Regions, R, Graph,
                                           {FromId, ToId})) {
      continue;
    }

    std::vector<BlockId> Blocks = From->Blocks;
    Blocks.insert(Blocks.end(), To->Blocks.begin(), To->Blocks.end());
    NodeId Root = buildSequenceNode(Cfg, {From, To}, Tree);
    Graph.collapseNodes({FromId, ToId}, From->Block, Root);
    return true;
  }
  return false;
}

bool reduceIfOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                  const RegionTree &Regions, const Region &R,
                  StructuredTree &Tree) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.empty() ||
        Header->Succs.size() != 2) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Header->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Branch ||
        Tail->Successors.size() != 2) {
      continue;
    }

    GraphNodeId TrueId = Graph.getNodeForBlock(Tail->Successors[0]);
    GraphNodeId FalseId = Graph.getNodeForBlock(Tail->Successors[1]);
    const MutableRegionNode *TrueNode = Graph.getNode(TrueId);
    const MutableRegionNode *FalseNode = Graph.getNode(FalseId);
    if (TrueNode == nullptr || FalseNode == nullptr) {
      continue;
    }

    GraphNodeId FollowId = InvalidGraphNodeId;
    GraphNodeId ThenId = InvalidGraphNodeId;
    GraphNodeId ElseId = InvalidGraphNodeId;
    bool NegateCondition = false;
    bool HasTerminalElse = false;

    auto IsTerminalNode = [&](const MutableRegionNode *Node) {
      if (Node == nullptr || !Node->Succs.empty() || Node->Blocks.empty()) {
        return false;
      }
      const CFGBlock *Block = Cfg.getBlock(Node->Blocks.back());
      return Block != nullptr &&
             (Block->Terminator == TerminatorKind::Return ||
              Block->Terminator == TerminatorKind::Unreachable);
    };

    if (TrueNode->Preds.size() == 1 && TrueNode->Succs.size() == 1 &&
        TrueNode->Succs[0] == FalseId) {
      ThenId = TrueId;
      FollowId = FalseId;
    } else if (FalseNode->Preds.size() == 1 && FalseNode->Succs.size() == 1 &&
               FalseNode->Succs[0] == TrueId) {
      ThenId = FalseId;
      FollowId = TrueId;
      NegateCondition = true;
    } else if (TrueNode->Preds.size() == 1 && FalseNode->Preds.size() == 1 &&
               TrueNode->Succs.size() == 1 && FalseNode->Succs.size() == 1 &&
               TrueNode->Succs[0] == FalseNode->Succs[0]) {
      ThenId = TrueId;
      ElseId = FalseId;
      FollowId = TrueNode->Succs[0];
    } else if (TrueNode->Preds.size() == 1 && FalseNode->Preds.size() == 1 &&
               IsTerminalNode(TrueNode) && FalseNode->Succs.size() == 1 &&
               FalseNode->Succs[0] != HeaderId &&
               !Graph.hasEdge(FalseNode->Succs[0], HeaderId)) {
      ThenId = TrueId;
      FollowId = FalseId;
    } else if (TrueNode->Preds.size() == 1 && FalseNode->Preds.size() == 1 &&
               TrueNode->Succs.size() == 1 && TrueNode->Succs[0] != HeaderId &&
               !Graph.hasEdge(TrueNode->Succs[0], HeaderId) &&
               IsTerminalNode(FalseNode)) {
      ThenId = FalseId;
      FollowId = TrueId;
      NegateCondition = true;
    } else if (TrueNode->Preds.size() == 1 && FalseNode->Preds.size() == 1 &&
               IsTerminalNode(TrueNode) && IsTerminalNode(FalseNode)) {
      ThenId = TrueId;
      ElseId = FalseId;
      HasTerminalElse = true;
    } else {
      continue;
    }

    const MutableRegionNode *Then = Graph.getNode(ThenId);
    const MutableRegionNode *Else =
        ElseId == InvalidGraphNodeId ? nullptr : Graph.getNode(ElseId);
    if (Then == nullptr ||
        (!HasTerminalElse && Graph.getNode(FollowId) == nullptr) ||
        (ElseId != InvalidGraphNodeId && Else == nullptr)) {
      continue;
    }

    std::vector<GraphNodeId> Members = {HeaderId, ThenId};
    if (ElseId != InvalidGraphNodeId) {
      Members.push_back(ElseId);
    }
    if (collapseCrossesNaturalLoopBoundary(Cfg, Regions, R, Graph, Members)) {
      continue;
    }

    StructuredNode Sequence;
    Sequence.Kind = StructuredNodeKind::Sequence;
    for (BlockId Block : Header->Blocks) {
      appendBlockLabel(Block, Sequence, Tree);
      appendBlockBody(Cfg, Block, Sequence, Tree);
    }

    StructuredNode IfNode;
    IfNode.Kind = StructuredNodeKind::If;
    IfNode.Block = Tail->Id;
    IfNode.Condition = Tail->Condition;
    IfNode.ConditionNegated = NegateCondition;
    IfNode.Then = Then->StructuredRoot != InvalidNodeId
                      ? Then->StructuredRoot
                      : buildLinearNode(Cfg, Then->Blocks, Tree);
    if (Else != nullptr) {
      IfNode.Else = Else->StructuredRoot != InvalidNodeId
                        ? Else->StructuredRoot
                        : buildLinearNode(Cfg, Else->Blocks, Tree);
    }
    Sequence.Children.push_back(Tree.addNode(std::move(IfNode)));

    NodeId Root = Tree.addNode(std::move(Sequence));
    Graph.collapseNodes(Members, Header->Block, Root);
    return true;
  }
  return false;
}

NodeId buildSwitchCaseBody(const StructuredCFG &Cfg,
                           const MutableRegionNode *CaseNode,
                           StructuredTree &Tree) {
  StructuredNode Body;
  Body.Kind = StructuredNodeKind::Sequence;
  if (CaseNode != nullptr) {
    if (CaseNode->StructuredRoot != InvalidNodeId) {
      Body.Children.push_back(CaseNode->StructuredRoot);
    } else {
      for (BlockId Block : CaseNode->Blocks) {
        appendBlockLabel(Block, Body, Tree);
        appendBlockBody(Cfg, Block, Body, Tree);
      }
    }
  }
  Body.Children.push_back(Tree.addNode(makeBreak()));
  return Tree.addNode(std::move(Body));
}

bool reduceSwitchOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                      const RegionTree &Regions, const Region &R,
                      StructuredTree &Tree) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.empty()) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Header->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Switch ||
        Tail->Successors.empty()) {
      continue;
    }

    GraphNodeId FollowId = InvalidGraphNodeId;
    bool Valid = true;
    std::vector<GraphNodeId> Targets;
    Targets.reserve(Tail->Successors.size());
    for (BlockId SuccBlock : Tail->Successors) {
      GraphNodeId TargetId = Graph.getNodeForBlock(SuccBlock);
      const MutableRegionNode *Target = Graph.getNode(TargetId);
      if (Target == nullptr) {
        Valid = false;
        break;
      }
      Targets.push_back(TargetId);

      if (Target->Succs.size() > 1) {
        Valid = false;
        break;
      }
      if (Target->Succs.empty()) {
        continue;
      }

      GraphNodeId TargetFollow = Target->Succs[0];
      if (TargetFollow == HeaderId) {
        Valid = false;
        break;
      }
      if (FollowId == InvalidGraphNodeId) {
        FollowId = TargetFollow;
      } else if (TargetId != FollowId && FollowId != TargetFollow) {
        Valid = false;
        break;
      }
    }
    bool HasTerminalCasesOnly = Valid && FollowId == InvalidGraphNodeId;
    if (!Valid || FollowId == InvalidGraphNodeId) {
      std::set<GraphNodeId> UniqueTargets(Targets.begin(), Targets.end());
      if (Valid && UniqueTargets.size() == 1) {
        FollowId = *UniqueTargets.begin();
        HasTerminalCasesOnly = false;
      } else if (HasTerminalCasesOnly) {
        for (GraphNodeId TargetId : Targets) {
          const MutableRegionNode *Target = Graph.getNode(TargetId);
          if (Target == nullptr || !Target->Succs.empty()) {
            Valid = false;
            break;
          }
        }
      } else {
        continue;
      }
    }
    if (!Valid) {
      continue;
    }

    std::set<GraphNodeId> Members;
    Members.insert(HeaderId);
    for (GraphNodeId TargetId : Targets) {
      if (TargetId == FollowId) {
        continue;
      }
      const MutableRegionNode *Target = Graph.getNode(TargetId);
      if (Target == nullptr || Target->Preds.size() != 1 ||
          Target->Preds[0] != HeaderId ||
          (HasTerminalCasesOnly
               ? !Target->Succs.empty()
               : (Target->Succs.size() != 1 || Target->Succs[0] != FollowId))) {
        Valid = false;
        break;
      }
      Members.insert(TargetId);
    }
    if (!Valid) {
      continue;
    }

    std::vector<GraphNodeId> MemberList(Members.begin(), Members.end());
    if (collapseCrossesNaturalLoopBoundary(Cfg, Regions, R, Graph,
                                           MemberList)) {
      continue;
    }

    StructuredNode Sequence;
    Sequence.Kind = StructuredNodeKind::Sequence;
    for (BlockId Block : Header->Blocks) {
      appendBlockLabel(Block, Sequence, Tree);
      appendBlockBody(Cfg, Block, Sequence, Tree);
    }

    StructuredNode SwitchNode;
    SwitchNode.Kind = StructuredNodeKind::Switch;
    SwitchNode.Block = Tail->Id;
    SwitchNode.Condition = Tail->Condition;

    GraphNodeId DefaultId = Targets.front();
    SwitchNode.Default = buildSwitchCaseBody(
        Cfg, DefaultId == FollowId ? nullptr : Graph.getNode(DefaultId), Tree);
    for (const SwitchCase &Case : Tail->Cases) {
      GraphNodeId CaseId = Graph.getNodeForBlock(Case.Target);
      SwitchNode.StructuredCases.push_back(
          {Case.Value, Case.Target,
           buildSwitchCaseBody(
               Cfg, CaseId == FollowId ? nullptr : Graph.getNode(CaseId),
               Tree)});
    }
    Sequence.Children.push_back(Tree.addNode(std::move(SwitchNode)));

    Graph.collapseNodes(MemberList, Header->Block,
                        Tree.addNode(std::move(Sequence)));
    return true;
  }
  return false;
}

bool collectLinearLoopBody(const StructuredCFG &Cfg,
                           const MutableRegionGraph &Graph,
                           GraphNodeId HeaderId, GraphNodeId BodyEntryId,
                           std::vector<GraphNodeId> &BodyIds) {
  const MutableRegionNode *BodyEntry = Graph.getNode(BodyEntryId);
  if (BodyEntry == nullptr || BodyEntry->Preds.size() != 1 ||
      BodyEntry->Preds[0] != HeaderId) {
    return false;
  }

  std::set<GraphNodeId> Seen;
  GraphNodeId CurrentId = BodyEntryId;
  while (true) {
    if (CurrentId == HeaderId || !Seen.insert(CurrentId).second) {
      return false;
    }

    const MutableRegionNode *Current = Graph.getNode(CurrentId);
    if (Current == nullptr || Current->Succs.size() != 1) {
      return false;
    }
    BodyIds.push_back(CurrentId);

    GraphNodeId NextId = Current->Succs[0];
    if (NextId == HeaderId) {
      return true;
    }
    const MutableRegionNode *Next = Graph.getNode(NextId);
    if (Next == nullptr || Next->Preds.size() != 1 ||
        Next->Preds[0] != CurrentId ||
        !isFallthroughTo(Cfg, CurrentId, NextId, Graph)) {
      return false;
    }
    CurrentId = NextId;
  }
}

struct LoopBreakSite {
  GraphNodeId Node = InvalidGraphNodeId;
  GraphNodeId ExitNode = InvalidGraphNodeId;
  bool NegateCondition = false;
};

bool isTerminalGraphNode(const StructuredCFG &Cfg,
                         const MutableRegionGraph &Graph, GraphNodeId Id) {
  const MutableRegionNode *Node = Graph.getNode(Id);
  if (Node == nullptr || !Node->Succs.empty() || Node->Blocks.empty()) {
    return false;
  }

  const CFGBlock *Block = Cfg.getBlock(Node->Blocks.back());
  return Block != nullptr && (Block->Terminator == TerminatorKind::Return ||
                              Block->Terminator == TerminatorKind::Unreachable);
}

bool isLoopBreakExit(const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
                     GraphNodeId CandidateId, GraphNodeId FollowId,
                     GraphNodeId FromId) {
  if (CandidateId == FollowId) {
    return true;
  }

  const MutableRegionNode *Candidate = Graph.getNode(CandidateId);
  if (Candidate == nullptr || Candidate->Preds.size() != 1 ||
      Candidate->Preds[0] != FromId) {
    return false;
  }
  return isTerminalGraphNode(Cfg, Graph, CandidateId) &&
         isTerminalGraphNode(Cfg, Graph, FollowId);
}

NodeId buildBreakIfNode(const CFGBlock &Block, bool NegateCondition,
                        StructuredTree &Tree) {
  StructuredNode BreakNode = makeBreak();

  StructuredNode IfNode;
  IfNode.Kind = StructuredNodeKind::If;
  IfNode.Block = Block.Id;
  IfNode.Condition = Block.Condition;
  IfNode.ConditionNegated = NegateCondition;
  IfNode.Then = Tree.addNode(std::move(BreakNode));
  return Tree.addNode(std::move(IfNode));
}

NodeId buildLoopBodyWithBreak(const StructuredCFG &Cfg,
                              const MutableRegionGraph &Graph,
                              const std::vector<GraphNodeId> &BodyIds,
                              const LoopBreakSite &BreakSite,
                              StructuredTree &Tree) {
  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;

  for (GraphNodeId BodyId : BodyIds) {
    const MutableRegionNode *Node = Graph.getNode(BodyId);
    if (Node == nullptr) {
      continue;
    }
    appendRegionNode(Cfg, *Node, Sequence, Tree);

    if (BodyId != BreakSite.Node || Node->Blocks.empty()) {
      continue;
    }
    const CFGBlock *Tail = Cfg.getBlock(Node->Blocks.back());
    if (Tail != nullptr) {
      Sequence.Children.push_back(
          buildBreakIfNode(*Tail, BreakSite.NegateCondition, Tree));
    }
  }

  return Tree.addNode(std::move(Sequence));
}

bool collectLinearLoopBodyWithBreak(
    const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
    GraphNodeId HeaderId, GraphNodeId BodyEntryId, GraphNodeId FollowId,
    std::vector<GraphNodeId> &BodyIds, LoopBreakSite &BreakSite) {
  // This is a conservative fast path for `while (...) { ... if (...) break; }`.
  // It only accepts a linear loop body and at most one break edge.
  const MutableRegionNode *BodyEntry = Graph.getNode(BodyEntryId);
  if (BodyEntry == nullptr || BodyEntry->Preds.size() != 1 ||
      BodyEntry->Preds[0] != HeaderId) {
    return false;
  }

  bool SawBreak = false;
  std::set<GraphNodeId> Seen;
  GraphNodeId CurrentId = BodyEntryId;
  while (true) {
    if (CurrentId == HeaderId || CurrentId == FollowId ||
        !Seen.insert(CurrentId).second) {
      return false;
    }

    const MutableRegionNode *Current = Graph.getNode(CurrentId);
    if (Current == nullptr || Current->Blocks.empty()) {
      return false;
    }
    BodyIds.push_back(CurrentId);

    if (Current->Succs.size() == 1) {
      GraphNodeId NextId = Current->Succs[0];
      if (NextId == HeaderId) {
        return SawBreak;
      }
      const MutableRegionNode *Next = Graph.getNode(NextId);
      if (Next == nullptr || Next->Preds.size() != 1 ||
          Next->Preds[0] != CurrentId ||
          !isFallthroughTo(Cfg, CurrentId, NextId, Graph)) {
        return false;
      }
      CurrentId = NextId;
      continue;
    }

    if (SawBreak || Current->Succs.size() != 2) {
      return false;
    }

    const CFGBlock *Tail = Cfg.getBlock(Current->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Branch ||
        Tail->Successors.size() != 2) {
      return false;
    }

    GraphNodeId TrueId = Graph.getNodeForBlock(Tail->Successors[0]);
    GraphNodeId FalseId = Graph.getNodeForBlock(Tail->Successors[1]);
    if (isLoopBreakExit(Cfg, Graph, TrueId, FollowId, CurrentId) &&
        FalseId != FollowId) {
      BreakSite = {CurrentId, TrueId, false};
      CurrentId = FalseId;
    } else if (isLoopBreakExit(Cfg, Graph, FalseId, FollowId, CurrentId) &&
               TrueId != FollowId) {
      BreakSite = {CurrentId, FalseId, true};
      CurrentId = TrueId;
    } else {
      return false;
    }

    if (CurrentId == HeaderId) {
      return true;
    }
    const MutableRegionNode *Next = Graph.getNode(CurrentId);
    if (Next == nullptr || Next->Preds.size() != 1 ||
        Next->Preds[0] != Current->Id) {
      return false;
    }
    SawBreak = true;
  }
}

bool reduceLinearWhileOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                           StructuredTree &Tree) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.size() != 1 ||
        Header->Succs.size() != 2) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Header->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Branch ||
        !Tail->Statements.empty() || Tail->Successors.size() != 2) {
      continue;
    }

    GraphNodeId TrueId = Graph.getNodeForBlock(Tail->Successors[0]);
    GraphNodeId FalseId = Graph.getNodeForBlock(Tail->Successors[1]);
    const MutableRegionNode *TrueNode = Graph.getNode(TrueId);
    const MutableRegionNode *FalseNode = Graph.getNode(FalseId);
    if (TrueNode == nullptr || FalseNode == nullptr) {
      continue;
    }

    GraphNodeId BodyEntryId = InvalidGraphNodeId;
    GraphNodeId FollowId = InvalidGraphNodeId;
    bool NegateCondition = false;
    std::vector<GraphNodeId> BodyIds;
    if (collectLinearLoopBody(Cfg, Graph, HeaderId, TrueId, BodyIds)) {
      BodyEntryId = TrueId;
      FollowId = FalseId;
    } else {
      BodyIds.clear();
      if (collectLinearLoopBody(Cfg, Graph, HeaderId, FalseId, BodyIds)) {
        BodyEntryId = FalseId;
        FollowId = TrueId;
        NegateCondition = true;
      }
    }

    if (BodyEntryId == InvalidGraphNodeId ||
        Graph.getNode(FollowId) == nullptr) {
      continue;
    }

    std::vector<const MutableRegionNode *> BodyNodes;
    BodyNodes.reserve(BodyIds.size());
    for (GraphNodeId BodyId : BodyIds) {
      BodyNodes.push_back(Graph.getNode(BodyId));
    }

    StructuredNode WhileNode;
    WhileNode.Kind = StructuredNodeKind::While;
    WhileNode.Block = Tail->Id;
    WhileNode.Condition = Tail->Condition;
    WhileNode.ConditionNegated = NegateCondition;
    WhileNode.Body = buildSequenceNode(Cfg, BodyNodes, Tree);

    std::vector<GraphNodeId> Members;
    Members.push_back(HeaderId);
    Members.insert(Members.end(), BodyIds.begin(), BodyIds.end());
    Graph.collapseNodes(Members, Header->Block,
                        Tree.addNode(std::move(WhileNode)));
    return true;
  }
  return false;
}

bool reduceLinearWhileWithBreakOnce(const StructuredCFG &Cfg,
                                    MutableRegionGraph &Graph,
                                    StructuredTree &Tree) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.size() != 1 ||
        Header->Succs.size() != 2) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Header->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Branch ||
        !Tail->Statements.empty() || Tail->Successors.size() != 2) {
      continue;
    }

    GraphNodeId TrueId = Graph.getNodeForBlock(Tail->Successors[0]);
    GraphNodeId FalseId = Graph.getNodeForBlock(Tail->Successors[1]);
    if (Graph.getNode(TrueId) == nullptr || Graph.getNode(FalseId) == nullptr) {
      continue;
    }

    GraphNodeId BodyEntryId = InvalidGraphNodeId;
    GraphNodeId FollowId = InvalidGraphNodeId;
    bool NegateCondition = false;
    LoopBreakSite BreakSite;
    std::vector<GraphNodeId> BodyIds;
    if (collectLinearLoopBodyWithBreak(Cfg, Graph, HeaderId, TrueId, FalseId,
                                       BodyIds, BreakSite)) {
      BodyEntryId = TrueId;
      FollowId = FalseId;
    } else {
      BodyIds.clear();
      BreakSite = {};
      if (collectLinearLoopBodyWithBreak(Cfg, Graph, HeaderId, FalseId, TrueId,
                                         BodyIds, BreakSite)) {
        BodyEntryId = FalseId;
        FollowId = TrueId;
        NegateCondition = true;
      }
    }

    if (BodyEntryId == InvalidGraphNodeId || FollowId == InvalidGraphNodeId) {
      continue;
    }

    StructuredNode WhileNode;
    WhileNode.Kind = StructuredNodeKind::While;
    WhileNode.Block = Tail->Id;
    WhileNode.Condition = Tail->Condition;
    WhileNode.ConditionNegated = NegateCondition;
    WhileNode.Body =
        buildLoopBodyWithBreak(Cfg, Graph, BodyIds, BreakSite, Tree);

    std::vector<GraphNodeId> Members;
    Members.push_back(HeaderId);
    Members.insert(Members.end(), BodyIds.begin(), BodyIds.end());
    if (BreakSite.ExitNode != FollowId &&
        BreakSite.ExitNode != InvalidGraphNodeId) {
      Members.push_back(BreakSite.ExitNode);
    }
    Graph.collapseNodes(Members, Header->Block,
                        Tree.addNode(std::move(WhileNode)));
    return true;
  }
  return false;
}

bool collectLinearDoWhileBody(const StructuredCFG &Cfg,
                              const MutableRegionGraph &Graph,
                              GraphNodeId EntryId, GraphNodeId LatchId,
                              std::vector<GraphNodeId> &BodyIds) {
  if (EntryId == LatchId) {
    return false;
  }

  std::set<GraphNodeId> Seen;
  GraphNodeId CurrentId = EntryId;
  while (true) {
    if (!Seen.insert(CurrentId).second) {
      return false;
    }

    const MutableRegionNode *Current = Graph.getNode(CurrentId);
    if (Current == nullptr) {
      return false;
    }
    BodyIds.push_back(CurrentId);
    if (CurrentId == LatchId) {
      return true;
    }
    if (Current->Succs.size() != 1) {
      return false;
    }

    GraphNodeId NextId = Current->Succs[0];
    const MutableRegionNode *Next = Graph.getNode(NextId);
    if (Next == nullptr || Next->Preds.size() != 1 ||
        Next->Preds[0] != CurrentId ||
        !isFallthroughTo(Cfg, CurrentId, NextId, Graph)) {
      return false;
    }
    CurrentId = NextId;
  }
}

bool reduceLinearDoWhileOnce(const StructuredCFG &Cfg,
                             MutableRegionGraph &Graph, StructuredTree &Tree) {
  for (GraphNodeId LatchId : Graph.activeNodes()) {
    const MutableRegionNode *Latch = Graph.getNode(LatchId);
    if (Latch == nullptr || Latch->Blocks.empty() || Latch->Succs.size() != 2) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Latch->Blocks.back());
    if (Tail == nullptr || Tail->Terminator != TerminatorKind::Branch ||
        Tail->Successors.size() != 2) {
      continue;
    }

    GraphNodeId TrueId = Graph.getNodeForBlock(Tail->Successors[0]);
    GraphNodeId FalseId = Graph.getNodeForBlock(Tail->Successors[1]);
    if (Graph.getNode(TrueId) == nullptr || Graph.getNode(FalseId) == nullptr) {
      continue;
    }

    GraphNodeId EntryId = InvalidGraphNodeId;
    GraphNodeId FollowId = InvalidGraphNodeId;
    bool NegateCondition = false;
    std::vector<GraphNodeId> BodyIds;
    if (collectLinearDoWhileBody(Cfg, Graph, TrueId, LatchId, BodyIds)) {
      EntryId = TrueId;
      FollowId = FalseId;
    } else {
      BodyIds.clear();
      if (collectLinearDoWhileBody(Cfg, Graph, FalseId, LatchId, BodyIds)) {
        EntryId = FalseId;
        FollowId = TrueId;
        NegateCondition = true;
      }
    }

    const MutableRegionNode *Entry = Graph.getNode(EntryId);
    if (Entry == nullptr || EntryId == FollowId || Entry->Preds.size() < 2 ||
        Graph.getNode(FollowId) == nullptr) {
      continue;
    }

    std::vector<const MutableRegionNode *> BodyNodes;
    BodyNodes.reserve(BodyIds.size());
    bool Valid = true;
    for (GraphNodeId BodyId : BodyIds) {
      const MutableRegionNode *BodyNode = Graph.getNode(BodyId);
      if (BodyNode == nullptr) {
        Valid = false;
        break;
      }
      BodyNodes.push_back(BodyNode);
    }
    if (!Valid) {
      continue;
    }

    StructuredNode DoWhileNode;
    DoWhileNode.Kind = StructuredNodeKind::DoWhile;
    DoWhileNode.Block = Tail->Id;
    DoWhileNode.Condition = Tail->Condition;
    DoWhileNode.ConditionNegated = NegateCondition;
    DoWhileNode.Body = buildSequenceNode(Cfg, BodyNodes, Tree);
    Graph.collapseNodes(BodyIds, Entry->Block,
                        Tree.addNode(std::move(DoWhileNode)));
    return true;
  }
  return false;
}

bool reduceSelfLoopOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                        StructuredTree &Tree) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.empty() ||
        !Graph.hasEdge(HeaderId, HeaderId)) {
      continue;
    }

    const CFGBlock *Tail = Cfg.getBlock(Header->Blocks.back());
    if (Tail == nullptr) {
      continue;
    }

    StructuredNode LoopNode;
    LoopNode.Block = Tail->Id;
    LoopNode.Body = buildLoopBody(Cfg, *Header, Tree);

    if (Header->Succs.size() == 1 && Tail->Successors.size() == 1) {
      LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
    } else if (Header->Succs.size() == 2 &&
               Tail->Terminator == TerminatorKind::Branch &&
               Tail->Successors.size() == 2) {
      bool HeadControlled = Tail->Statements.empty();
      if (Tail->Successors[0] == Header->Blocks.front()) {
        LoopNode.Kind = HeadControlled ? StructuredNodeKind::While
                                       : StructuredNodeKind::DoWhile;
        LoopNode.Condition = Tail->Condition;
      } else if (Tail->Successors[1] == Header->Blocks.front()) {
        LoopNode.Kind = HeadControlled ? StructuredNodeKind::While
                                       : StructuredNodeKind::DoWhile;
        LoopNode.Condition = Tail->Condition;
        LoopNode.ConditionNegated = true;
      } else {
        continue;
      }
    } else {
      continue;
    }

    Graph.collapseNodes({HeaderId}, Header->Block,
                        Tree.addNode(std::move(LoopNode)));
    return true;
  }
  return false;
}

std::vector<VirtualEdge>
collectVirtualizableEdges(const Region &R, const MutableRegionGraph &Graph) {
  std::vector<VirtualEdge> Edges;
  for (const MutableRegionNode &Node : Graph.nodes()) {
    if (!Node.Active) {
      continue;
    }
    for (GraphNodeId Succ : Node.Succs) {
      const MutableRegionNode *SuccNode = Graph.getNode(Succ);
      if (SuccNode == nullptr || !SuccNode->Active) {
        continue;
      }
      BlockId FromBlock =
          Node.Blocks.empty() ? InvalidBlockId : Node.Blocks.back();
      BlockId ToBlock =
          SuccNode->Blocks.empty() ? InvalidBlockId : SuccNode->Blocks.front();
      VirtualEdgeKind Kind = VirtualEdgeKind::Goto;
      if (R.Kind == RegionKind::NaturalLoop && ToBlock == R.Head) {
        Kind = VirtualEdgeKind::Continue;
      }
      Edges.push_back({Node.Id, Succ, FromBlock, ToBlock, Kind});
    }
  }
  return Edges;
}

std::vector<VirtualEdge>
filterByAngrLastResortPriority(const Region &R,
                               const MutableRegionGraphAnalysis &Analysis,
                               const std::vector<VirtualEdge> &Edges) {
  // Match Angr Phoenix's last-resort buckets: prefer edges where neither end
  // dominates the other, then edges whose source does not dominate the target.
  // If the root acyclic view had to drop cycle-closing edges, use them only
  // after the normal buckets are empty. The final per-bucket order is still
  // delegated to Phoenix/SAILR.
  std::vector<VirtualEdge> NoDominanceEdges;
  std::vector<VirtualEdge> SecondaryEdges;

  for (const VirtualEdge &Edge : Edges) {
    bool FromDominatesTo = Analysis.dominates(Edge.From, Edge.To);
    bool ToDominatesFrom = Analysis.dominates(Edge.To, Edge.From);
    if (!FromDominatesTo && !ToDominatesFrom) {
      NoDominanceEdges.push_back(Edge);
    } else if (!FromDominatesTo) {
      SecondaryEdges.push_back(Edge);
    }
  }

  if (!NoDominanceEdges.empty()) {
    return NoDominanceEdges;
  }
  if (!SecondaryEdges.empty()) {
    return SecondaryEdges;
  }
  if (R.Kind == RegionKind::Root) {
    return Analysis.AcyclicDroppedEdges;
  }
  return {};
}

std::map<GraphNodeId, std::vector<VirtualEdge>>
groupVirtualEdgesBySource(const MutableRegionGraph &Graph) {
  std::map<GraphNodeId, std::vector<VirtualEdge>> Result;
  for (const VirtualEdge &Edge : Graph.virtualEdges()) {
    Result[Edge.From].push_back(Edge);
  }
  return Result;
}

void appendControlTransfer(StructuredNode &Root, StructuredTree &Tree,
                           BlockId Target, VirtualEdgeKind Kind) {
  if (Target == InvalidBlockId) {
    return;
  }

  Root.Children.push_back(Tree.addNode(makeControlTransfer(Target, Kind)));
}

bool isTerminalBlock(const StructuredCFG &Cfg, BlockId Target) {
  const CFGBlock *Block = Cfg.getBlock(Target);
  return Block != nullptr && (Block->Terminator == TerminatorKind::Return ||
                              Block->Terminator == TerminatorKind::Unreachable);
}

bool allSuccessorsAreTerminal(const StructuredCFG &Cfg, const Region &R) {
  if (R.Successors.empty()) {
    return false;
  }
  for (BlockId Succ : R.Successors) {
    if (!isTerminalBlock(Cfg, Succ)) {
      return false;
    }
  }
  return true;
}

VirtualEdgeKind classifyNaturalLoopExit(const StructuredCFG &Cfg,
                                        const Region &R, BlockId Target) {
  if (R.Kind == RegionKind::NaturalLoop) {
    if (Target == R.Head) {
      return VirtualEdgeKind::Continue;
    }
    if (R.Successors.size() == 1 && R.Successors.front() == Target) {
      return VirtualEdgeKind::Break;
    }
    if (R.Successors.size() == 1 && isTerminalBlock(Cfg, R.Successors.front()) &&
        isTerminalBlock(Cfg, Target)) {
      return VirtualEdgeKind::Break;
    }
    if (allSuccessorsAreTerminal(Cfg, R) && isTerminalBlock(Cfg, Target)) {
      return VirtualEdgeKind::Break;
    }
  }
  return VirtualEdgeKind::Goto;
}

bool hasSuccessorTarget(const CFGBlock &Block, BlockId Target) {
  return std::find(Block.Successors.begin(), Block.Successors.end(), Target) !=
         Block.Successors.end();
}

bool nodeHasSuccessorTarget(const StructuredCFG &Cfg,
                            const MutableRegionNode &Node, BlockId Target) {
  for (BlockId BlockId : Node.Blocks) {
    const CFGBlock *Block = Cfg.getBlock(BlockId);
    if (Block != nullptr && hasSuccessorTarget(*Block, Target)) {
      return true;
    }
  }
  return false;
}

bool nodeContainsBlock(const MutableRegionNode &Node, BlockId Target) {
  return std::find(Node.Blocks.begin(), Node.Blocks.end(), Target) !=
         Node.Blocks.end();
}

void appendSourceBody(const StructuredCFG &Cfg, const MutableRegionNode &Source,
                      StructuredNode &Sequence, StructuredTree &Tree) {
  if (Source.StructuredRoot != InvalidNodeId) {
    Sequence.Children.push_back(Source.StructuredRoot);
    return;
  }
  for (BlockId Block : Source.Blocks) {
    appendBlockLabel(Block, Sequence, Tree);
    appendBlockBody(Cfg, Block, Sequence, Tree);
  }
}

NodeId buildVirtualizedBranchSource(const CFGBlock &Tail,
                                    const MutableRegionNode &Source,
                                    const VirtualEdge &Edge, const Region &R,
                                    const StructuredCFG &Cfg,
                                    StructuredTree &Tree) {
  bool RemovedTrue = Tail.Successors[0] == Edge.ToBlock;
  bool RemovedFalse = Tail.Successors[1] == Edge.ToBlock;
  if (!RemovedTrue && !RemovedFalse) {
    return InvalidNodeId;
  }

  BlockId KeptTarget = RemovedTrue ? Tail.Successors[1] : Tail.Successors[0];

  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  appendSourceBody(Cfg, Source, Sequence, Tree);

  StructuredNode IfNode;
  IfNode.Kind = StructuredNodeKind::If;
  IfNode.Block = Tail.Id;
  IfNode.Condition = Tail.Condition;
  IfNode.ConditionNegated = RemovedFalse;
  IfNode.Then = Tree.addNode(makeControlTransfer(
      Edge.ToBlock, Edge.Kind == VirtualEdgeKind::Goto
                        ? classifyNaturalLoopExit(Cfg, R, Edge.ToBlock)
                        : Edge.Kind));
  Sequence.Children.push_back(Tree.addNode(std::move(IfNode)));
  Sequence.Children.push_back(Tree.addNode(makeControlTransfer(
      KeptTarget, classifyNaturalLoopExit(Cfg, R, KeptTarget))));
  return Tree.addNode(std::move(Sequence));
}

NodeId buildVirtualizedSwitchSource(const CFGBlock &Tail,
                                    const MutableRegionNode &Source,
                                    const VirtualEdge &Edge, const Region &R,
                                    const StructuredCFG &Cfg,
                                    StructuredTree &Tree) {
  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  appendSourceBody(Cfg, Source, Sequence, Tree);

  StructuredNode SwitchNode;
  SwitchNode.Kind = StructuredNodeKind::Switch;
  SwitchNode.Block = Tail.Id;
  SwitchNode.Condition = Tail.Condition;
  SwitchNode.Cases = Tail.Cases;

  bool Matched = false;
  if (!Tail.Successors.empty() && Tail.Successors[0] == Edge.ToBlock) {
    SwitchNode.Default = Tree.addNode(makeControlTransfer(
        Edge.ToBlock, Edge.Kind == VirtualEdgeKind::Goto
                          ? classifyNaturalLoopExit(Cfg, R, Edge.ToBlock)
                          : Edge.Kind));
    Matched = true;
  }
  for (const SwitchCase &Case : Tail.Cases) {
    if (Case.Target != Edge.ToBlock) {
      continue;
    }
    NodeId CaseBody = Tree.addNode(makeControlTransfer(
        Edge.ToBlock, Edge.Kind == VirtualEdgeKind::Goto
                          ? classifyNaturalLoopExit(Cfg, R, Edge.ToBlock)
                          : Edge.Kind));
    SwitchNode.StructuredCases.push_back({Case.Value, Case.Target, CaseBody});
    Matched = true;
  }
  if (!Matched) {
    return InvalidNodeId;
  }

  Sequence.Children.push_back(Tree.addNode(std::move(SwitchNode)));
  for (BlockId Succ : Tail.Successors) {
    if (Succ != Edge.ToBlock) {
      Sequence.Children.push_back(Tree.addNode(
          makeControlTransfer(Succ, classifyNaturalLoopExit(Cfg, R, Succ))));
    }
  }
  return Tree.addNode(std::move(Sequence));
}

NodeId buildVirtualizedFallthroughSource(const CFGBlock &Tail,
                                         const MutableRegionNode &Source,
                                         const VirtualEdge &Edge,
                                         const Region &R,
                                         const StructuredCFG &Cfg,
                                         StructuredTree &Tree) {
  if (!hasSuccessorTarget(Tail, Edge.ToBlock)) {
    return InvalidNodeId;
  }

  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  appendSourceBody(Cfg, Source, Sequence, Tree);
  Sequence.Children.push_back(Tree.addNode(makeControlTransfer(
      Edge.ToBlock, Edge.Kind == VirtualEdgeKind::Goto
                        ? classifyNaturalLoopExit(Cfg, R, Edge.ToBlock)
                        : Edge.Kind)));
  return Tree.addNode(std::move(Sequence));
}

NodeId buildVirtualizedSource(const StructuredCFG &Cfg,
                              const MutableRegionNode &Source,
                              const VirtualEdge &Edge, const Region &R,
                              StructuredTree &Tree) {
  if (Source.Blocks.empty()) {
    return InvalidNodeId;
  }

  const CFGBlock *Tail = Cfg.getBlock(Source.TailBlock);
  if (Tail == nullptr) {
    return InvalidNodeId;
  }
  switch (Tail->Terminator) {
  case TerminatorKind::Branch:
    if (Tail->Successors.size() != 2) {
      return InvalidNodeId;
    }
    return buildVirtualizedBranchSource(*Tail, Source, Edge, R, Cfg, Tree);
  case TerminatorKind::Switch:
    return buildVirtualizedSwitchSource(*Tail, Source, Edge, R, Cfg, Tree);
  case TerminatorKind::Fallthrough:
    return buildVirtualizedFallthroughSource(*Tail, Source, Edge, R, Cfg, Tree);
  case TerminatorKind::Return:
  case TerminatorKind::Unreachable:
    return InvalidNodeId;
  }
  return InvalidNodeId;
}

bool nodeTreeContainsKind(const StructuredTree &Tree, NodeId Id,
                          StructuredNodeKind Kind) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == Kind) {
    return true;
  }
  for (NodeId Child : Node->Children) {
    if (nodeTreeContainsKind(Tree, Child, Kind)) {
      return true;
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    if (nodeTreeContainsKind(Tree, Case.Body, Kind)) {
      return true;
    }
  }
  return nodeTreeContainsKind(Tree, Node->Then, Kind) ||
         nodeTreeContainsKind(Tree, Node->Else, Kind) ||
         nodeTreeContainsKind(Tree, Node->Body, Kind) ||
         nodeTreeContainsKind(Tree, Node->Default, Kind);
}

bool nodeTreeContainsStructuredControl(const StructuredTree &Tree, NodeId Id) {
  return nodeTreeContainsKind(Tree, Id, StructuredNodeKind::Switch) ||
         nodeTreeContainsKind(Tree, Id, StructuredNodeKind::While) ||
         nodeTreeContainsKind(Tree, Id, StructuredNodeKind::DoWhile) ||
         nodeTreeContainsKind(Tree, Id, StructuredNodeKind::InfiniteLoop);
}

void appendFallbackNode(const StructuredCFG &Cfg, const MutableRegionNode &Node,
                        const std::vector<VirtualEdge> &VirtualEdges,
                        const Region &R, StructuredNode &Root,
                        StructuredTree &Tree) {
  if (Node.StructuredRoot != InvalidNodeId) {
    Root.Children.push_back(Node.StructuredRoot);
    if (Node.Blocks.empty()) {
      return;
    }
    const CFGBlock *Tail = Cfg.getBlock(Node.TailBlock);
    if (Tail == nullptr) {
      return;
    }
    if (nodeTreeContainsStructuredControl(Tree, Node.StructuredRoot)) {
      for (const VirtualEdge &Edge : VirtualEdges) {
        if (!nodeContainsBlock(Node, Edge.ToBlock) &&
            !nodeHasSuccessorTarget(Cfg, Node, Edge.ToBlock)) {
          appendControlTransfer(Root, Tree, Edge.ToBlock,
                                Edge.Kind == VirtualEdgeKind::Goto
                                    ? classifyNaturalLoopExit(Cfg, R,
                                                              Edge.ToBlock)
                                    : Edge.Kind);
        }
      }
      return;
    }
  } else {
    for (BlockId Block : Node.Blocks) {
      appendBlockLabel(Block, Root, Tree);
      appendBlockBody(Cfg, Block, Root, Tree);
    }
  }

  if (Node.Blocks.empty()) {
    return;
  }
  const CFGBlock *Tail = Cfg.getBlock(Node.TailBlock);
  if (Tail == nullptr) {
    return;
  }

  switch (Tail->Terminator) {
  case TerminatorKind::Branch: {
    StructuredNode IfNode;
    IfNode.Kind = StructuredNodeKind::If;
    IfNode.Block = Tail->Id;
    IfNode.Condition = Tail->Condition;
    if (!Tail->Successors.empty()) {
      IfNode.Then = Tree.addNode(
          makeControlTransfer(Tail->Successors[0],
                              classifyNaturalLoopExit(
                                  Cfg, R, Tail->Successors[0])));
    }
    if (Tail->Successors.size() > 1) {
      IfNode.Else = Tree.addNode(
          makeControlTransfer(Tail->Successors[1],
                              classifyNaturalLoopExit(
                                  Cfg, R, Tail->Successors[1])));
    }
    Root.Children.push_back(Tree.addNode(std::move(IfNode)));
    break;
  }
  case TerminatorKind::Switch: {
    StructuredNode SwitchNode;
    SwitchNode.Kind = StructuredNodeKind::Switch;
    SwitchNode.Block = Tail->Id;
    SwitchNode.Condition = Tail->Condition;
    SwitchNode.Cases = Tail->Cases;
    if (!Tail->Successors.empty()) {
      SwitchNode.Default = Tree.addNode(
          makeControlTransfer(Tail->Successors[0],
                              classifyNaturalLoopExit(
                                  Cfg, R, Tail->Successors[0])));
    }
    for (const auto &Case : Tail->Cases) {
      NodeId CaseBody = Tree.addNode(makeControlTransfer(
          Case.Target, classifyNaturalLoopExit(Cfg, R, Case.Target)));
      SwitchNode.StructuredCases.push_back({Case.Value, Case.Target, CaseBody});
    }
    Root.Children.push_back(Tree.addNode(std::move(SwitchNode)));
    break;
  }
  case TerminatorKind::Fallthrough:
    for (BlockId Succ : Tail->Successors) {
      Root.Children.push_back(Tree.addNode(
          makeControlTransfer(Succ, classifyNaturalLoopExit(Cfg, R, Succ))));
    }
    break;
  case TerminatorKind::Return: {
    StructuredNode Ret;
    Ret.Kind = StructuredNodeKind::Return;
    Root.Children.push_back(Tree.addNode(std::move(Ret)));
    break;
  }
  case TerminatorKind::Unreachable: {
    StructuredNode Unreachable;
    Unreachable.Kind = StructuredNodeKind::Unreachable;
    Root.Children.push_back(Tree.addNode(std::move(Unreachable)));
    break;
  }
  }

  for (const VirtualEdge &Edge : VirtualEdges) {
    if (!nodeContainsBlock(Node, Edge.ToBlock) &&
        !hasSuccessorTarget(*Tail, Edge.ToBlock)) {
      appendControlTransfer(Root, Tree, Edge.ToBlock,
                            Edge.Kind == VirtualEdgeKind::Goto
                                ? classifyNaturalLoopExit(Cfg, R, Edge.ToBlock)
                                : Edge.Kind);
    }
  }
  if (R.Kind == RegionKind::NaturalLoop) {
    for (BlockId Succ : Node.ExternalSuccs) {
      appendControlTransfer(Root, Tree, Succ,
                            classifyNaturalLoopExit(Cfg, R, Succ));
    }
  }
}

bool hasDirectLoopControlTransfer(const StructuredTree &Tree, NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == StructuredNodeKind::Break ||
      Node->Kind == StructuredNodeKind::Continue) {
    return true;
  }
  for (NodeId Child : Node->Children) {
    if (hasDirectLoopControlTransfer(Tree, Child)) {
      return true;
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    if (hasDirectLoopControlTransfer(Tree, Case.Body)) {
      return true;
    }
  }
  return hasDirectLoopControlTransfer(Tree, Node->Then) ||
         hasDirectLoopControlTransfer(Tree, Node->Else) ||
         hasDirectLoopControlTransfer(Tree, Node->Body) ||
         hasDirectLoopControlTransfer(Tree, Node->Default);
}

NodeId wrapNaturalLoopFallback(const Region &R, NodeId Body,
                               StructuredTree &Tree) {
  if (R.Kind != RegionKind::NaturalLoop ||
      !hasDirectLoopControlTransfer(Tree, Body)) {
    return Body;
  }

  StructuredNode LoopNode;
  LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
  LoopNode.Block = R.Head;
  LoopNode.Body = Body;
  return Tree.addNode(std::move(LoopNode));
}

void dropGotoIntoFollowingInfiniteLoop(StructuredNode &Node,
                                       const StructuredTree &Tree) {
  if (Node.Kind != StructuredNodeKind::Sequence || Node.Children.size() < 2) {
    return;
  }

  std::vector<NodeId> Filtered;
  Filtered.reserve(Node.Children.size());
  for (unsigned Index = 0; Index < Node.Children.size(); ++Index) {
    const StructuredNode *Current = Tree.getNode(Node.Children[Index]);
    const StructuredNode *Next = nullptr;
    if (Index + 1 < Node.Children.size()) {
      Next = Tree.getNode(Node.Children[Index + 1]);
    }
    if (Current != nullptr && Next != nullptr &&
        Current->Kind == StructuredNodeKind::Goto &&
        Next->Kind == StructuredNodeKind::InfiniteLoop &&
        Current->Target == Next->Block) {
      continue;
    }
    Filtered.push_back(Node.Children[Index]);
  }
  Node.Children = std::move(Filtered);
}

bool nodeIntersectsRegion(const MutableRegionNode &Node, const Region &R) {
  for (BlockId Block : Node.Blocks) {
    if (std::find(R.Blocks.begin(), R.Blocks.end(), Block) != R.Blocks.end()) {
      return true;
    }
  }
  return false;
}

bool nodeContainedByRegion(const MutableRegionNode &Node, const Region &R) {
  for (BlockId Block : Node.Blocks) {
    if (std::find(R.Blocks.begin(), R.Blocks.end(), Block) == R.Blocks.end()) {
      return false;
    }
  }
  return true;
}

bool regionAlreadyStructured(const MutableRegionGraph &Graph,
                             const Region &Loop, const StructuredTree &Tree) {
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node == nullptr || !nodeIntersectsRegion(*Node, Loop) ||
        Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    if (nodeTreeContainsKind(Tree, Node->StructuredRoot,
                             StructuredNodeKind::While) ||
        nodeTreeContainsKind(Tree, Node->StructuredRoot,
                             StructuredNodeKind::DoWhile) ||
        nodeTreeContainsKind(Tree, Node->StructuredRoot,
                             StructuredNodeKind::InfiniteLoop)) {
      return true;
    }
  }
  return false;
}

bool collectNaturalLoopMembers(const MutableRegionGraph &Graph,
                               const MutableRegionGraphAnalysis &Analysis,
                               GraphNodeId HeadId, GraphNodeId LatchId,
                               std::set<GraphNodeId> &Members) {
  if (!Analysis.dominates(HeadId, LatchId)) {
    return false;
  }

  Members.clear();
  Members.insert(HeadId);
  std::vector<GraphNodeId> Worklist = {LatchId};
  while (!Worklist.empty()) {
    GraphNodeId CurrentId = Worklist.back();
    Worklist.pop_back();
    if (!Members.insert(CurrentId).second) {
      continue;
    }

    const MutableRegionNode *Current = Graph.getNode(CurrentId);
    if (Current == nullptr || !Analysis.dominates(HeadId, CurrentId)) {
      return false;
    }
    for (GraphNodeId PredId : Current->Preds) {
      if (PredId != HeadId) {
        Worklist.push_back(PredId);
      }
    }
  }
  return Members.size() > 1;
}

std::vector<BlockId>
collectNaturalLoopSuccessors(const MutableRegionGraph &Graph,
                             const std::set<GraphNodeId> &Members) {
  std::vector<BlockId> Successors;
  for (GraphNodeId Id : Members) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node == nullptr) {
      continue;
    }
    for (GraphNodeId SuccId : Node->Succs) {
      if (Members.count(SuccId)) {
        continue;
      }
      const MutableRegionNode *Succ = Graph.getNode(SuccId);
      if (Succ != nullptr && !Succ->Blocks.empty() &&
          !containsBlock(Successors, Succ->Blocks.front())) {
        Successors.push_back(Succ->Blocks.front());
      }
    }
    for (BlockId Succ : Node->ExternalSuccs) {
      if (!containsBlock(Successors, Succ)) {
        Successors.push_back(Succ);
      }
    }
  }
  return Successors;
}

bool reduceGraphNaturalLoopOnce(const StructuredCFG &Cfg,
                                MutableRegionGraph &Graph,
                                StructuredTree &Tree) {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  for (GraphNodeId LatchId : Graph.activeNodes()) {
    const MutableRegionNode *Latch = Graph.getNode(LatchId);
    if (Latch == nullptr) {
      continue;
    }
    for (GraphNodeId HeadId : Latch->Succs) {
      const MutableRegionNode *Head = Graph.getNode(HeadId);
      if (Head == nullptr || Head->Blocks.empty()) {
        continue;
      }

      std::set<GraphNodeId> MemberSet;
      if (!collectNaturalLoopMembers(Graph, Analysis, HeadId, LatchId,
                                     MemberSet)) {
        continue;
      }

      std::vector<BlockId> Successors =
          collectNaturalLoopSuccessors(Graph, MemberSet);
      if (Successors.size() > 1) {
        continue;
      }

      Region LoopRegion;
      LoopRegion.Kind = RegionKind::NaturalLoop;
      LoopRegion.Head = Head->Blocks.front();
      LoopRegion.Latch = Latch->Blocks.empty() ? InvalidBlockId
                                               : Latch->Blocks.back();
      LoopRegion.Successors = Successors;

      std::vector<GraphNodeId> Members(MemberSet.begin(), MemberSet.end());
      std::sort(Members.begin(), Members.end(),
                [&](GraphNodeId A, GraphNodeId B) {
                  const MutableRegionNode *ANode = Graph.getNode(A);
                  const MutableRegionNode *BNode = Graph.getNode(B);
                  BlockId ABlock = (ANode == nullptr || ANode->Blocks.empty())
                                       ? InvalidBlockId
                                       : ANode->Blocks.front();
                  BlockId BBlock = (BNode == nullptr || BNode->Blocks.empty())
                                       ? InvalidBlockId
                                       : BNode->Blocks.front();
                  return ABlock < BBlock;
                });
      for (GraphNodeId Id : Members) {
        const MutableRegionNode *Node = Graph.getNode(Id);
        if (Node != nullptr) {
          LoopRegion.Blocks.insert(LoopRegion.Blocks.end(),
                                   Node->Blocks.begin(), Node->Blocks.end());
        }
      }

      StructuredNode Body;
      Body.Kind = StructuredNodeKind::Sequence;
      static const std::vector<VirtualEdge> EmptyVirtualEdges;
      for (GraphNodeId Id : Members) {
        const MutableRegionNode *Node = Graph.getNode(Id);
        if (Node != nullptr) {
          appendFallbackNode(Cfg, *Node, EmptyVirtualEdges, LoopRegion, Body,
                             Tree);
        }
      }

      StructuredNode LoopNode;
      LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
      LoopNode.Block = LoopRegion.Head;
      LoopNode.Body = Tree.addNode(std::move(Body));
      Graph.collapseNodes(Members, LoopRegion.Head,
                          Tree.addNode(std::move(LoopNode)));
      return true;
    }
  }
  return false;
}

bool reduceNaturalLoopFallbackOnce(const StructuredCFG &Cfg,
                                   const RegionTree &Regions,
                                   const Region &Parent,
                                   MutableRegionGraph &Graph,
                                   StructuredTree &Tree) {
  if (Parent.Kind != RegionKind::Root) {
    return false;
  }

  for (RegionId ChildId : Parent.Children) {
    const Region *Loop = Regions.getRegion(ChildId);
    if (Loop == nullptr || Loop->Kind != RegionKind::NaturalLoop ||
        regionAlreadyStructured(Graph, *Loop, Tree)) {
      continue;
    }

    std::vector<GraphNodeId> Members;
    for (GraphNodeId Id : Graph.activeNodes()) {
      const MutableRegionNode *Node = Graph.getNode(Id);
      if (Node == nullptr || !nodeIntersectsRegion(*Node, *Loop)) {
        continue;
      }
      if (!nodeContainedByRegion(*Node, *Loop)) {
        continue;
      }
      Members.push_back(Id);
    }
    if (Members.empty()) {
      continue;
    }

    std::sort(Members.begin(), Members.end(),
              [&](GraphNodeId A, GraphNodeId B) {
                const MutableRegionNode *ANode = Graph.getNode(A);
                const MutableRegionNode *BNode = Graph.getNode(B);
                BlockId ABlock = (ANode == nullptr || ANode->Blocks.empty())
                                     ? InvalidBlockId
                                     : ANode->Blocks.front();
                BlockId BBlock = (BNode == nullptr || BNode->Blocks.empty())
                                     ? InvalidBlockId
                                     : BNode->Blocks.front();
                return ABlock < BBlock;
              });

    StructuredNode Body;
    Body.Kind = StructuredNodeKind::Sequence;
    static const std::vector<VirtualEdge> EmptyVirtualEdges;
    for (GraphNodeId Id : Members) {
      const MutableRegionNode *Node = Graph.getNode(Id);
      if (Node != nullptr) {
        appendFallbackNode(Cfg, *Node, EmptyVirtualEdges, *Loop, Body, Tree);
      }
    }

    StructuredNode LoopNode;
    LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
    LoopNode.Block = Loop->Head;
    LoopNode.Body = Tree.addNode(std::move(Body));
    Graph.collapseNodes(Members, Loop->Head, Tree.addNode(std::move(LoopNode)));
    return true;
  }
  return false;
}

} // namespace

std::vector<VirtualEdge> PhoenixStructurer::orderVirtualizableEdges(
    const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
    const MutableRegionGraphAnalysis &Analysis,
    std::vector<VirtualEdge> Edges) const {
  (void)Cfg;
  std::sort(
      Edges.begin(), Edges.end(),
      [&](const VirtualEdge &A, const VirtualEdge &B) {
        auto OrderOf = [&](GraphNodeId Id) {
          auto It = Analysis.NodeOrder.find(Id);
          return It == Analysis.NodeOrder.end() ? 0U : It->second;
        };
        auto InDegreeOf = [&](GraphNodeId Id) {
          const MutableRegionNode *Node = Graph.getNode(Id);
          return Node == nullptr ? 0U
                                 : static_cast<unsigned>(Node->Preds.size());
        };
        auto OutDegreeOf = [&](GraphNodeId Id) {
          const MutableRegionNode *Node = Graph.getNode(Id);
          return Node == nullptr ? 0U
                                 : static_cast<unsigned>(Node->Succs.size());
        };

        return std::make_tuple(OrderOf(A.To), InDegreeOf(A.To),
                               OutDegreeOf(A.From), A.FromBlock, A.ToBlock) <
               std::make_tuple(OrderOf(B.To), InDegreeOf(B.To),
                               OutDegreeOf(B.From), B.FromBlock, B.ToBlock);
      });
  return Edges;
}

std::vector<VirtualEdge> PhoenixStructurer::edgeVirtualizationHints(
    const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
    const MutableRegionGraphAnalysis &Analysis) const {
  (void)Cfg;
  (void)Graph;
  (void)Analysis;
  return {};
}

bool PhoenixStructurer::analyzeAcyclic(const StructuredCFG &Cfg,
                                       const RegionTree &Regions,
                                       const Region &R,
                                       MutableRegionGraph &Graph,
                                       StructuredTree &Tree) const {
  if (reduceSwitchOnce(Cfg, Graph, Regions, R, Tree)) {
    return true;
  }
  if (reduceSequenceOnce(Cfg, Graph, Regions, R, Tree)) {
    return true;
  }
  return reduceIfOnce(Cfg, Graph, Regions, R, Tree);
}

bool PhoenixStructurer::analyzeCyclic(const StructuredCFG &Cfg,
                                      MutableRegionGraph &Graph,
                                      StructuredTree &Tree) const {
  if (reduceLinearWhileOnce(Cfg, Graph, Tree)) {
    return true;
  }
  if (useImprovedCyclicSchemas() &&
      reduceLinearWhileWithBreakOnce(Cfg, Graph, Tree)) {
    return true;
  }
  if (reduceLinearDoWhileOnce(Cfg, Graph, Tree)) {
    return true;
  }
  return reduceSelfLoopOnce(Cfg, Graph, Tree);
}

bool PhoenixStructurer::refineCyclic(const StructuredCFG &Cfg,
                                     const RegionTree &Regions, const Region &R,
                                     MutableRegionGraph &Graph,
                                     StructuredTree &Tree) const {
  if (reduceGraphNaturalLoopOnce(Cfg, Graph, Tree)) {
    return true;
  }
  return reduceNaturalLoopFallbackOnce(Cfg, Regions, R, Graph, Tree);
}

bool PhoenixStructurer::lastResortRefinement(const StructuredCFG &Cfg,
                                             const Region &R,
                                             MutableRegionGraph &Graph,
                                             StructuredTree &Tree) const {
  if (Graph.activeNodes().size() <= 1) {
    return false;
  }
  return virtualizeOneEdge(Cfg, R, Graph, Tree);
}

bool PhoenixStructurer::virtualizeOneEdge(const StructuredCFG &Cfg,
                                          const Region &R,
                                          MutableRegionGraph &Graph,
                                          StructuredTree &Tree) const {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  for (const VirtualEdge &Hint :
       edgeVirtualizationHints(Cfg, Graph, Analysis)) {
    if (Graph.hasEdge(Hint.From, Hint.To)) {
      const MutableRegionNode *Source = Graph.getNode(Hint.From);
      if (Source != nullptr) {
        NodeId Replacement =
            buildVirtualizedSource(Cfg, *Source, Hint, R, Tree);
        if (Replacement != InvalidNodeId) {
          Graph.setStructuredRoot(Hint.From, Replacement);
        }
      }
      Graph.virtualizeEdge(Hint.From, Hint.To, Hint.Kind);
      return true;
    }
  }

  std::vector<VirtualEdge> Candidates = filterByAngrLastResortPriority(
      R, Analysis, collectVirtualizableEdges(R, Graph));
  std::vector<VirtualEdge> Edges =
      orderVirtualizableEdges(Cfg, Graph, Analysis, std::move(Candidates));
  if (Edges.empty()) {
    return false;
  }

  const VirtualEdge &Edge = Edges.front();
  const MutableRegionNode *Source = Graph.getNode(Edge.From);
  if (Source != nullptr) {
    NodeId Replacement = buildVirtualizedSource(Cfg, *Source, Edge, R, Tree);
    if (Replacement != InvalidNodeId) {
      Graph.setStructuredRoot(Edge.From, Replacement);
    }
  }
  Graph.virtualizeEdge(Edge.From, Edge.To, Edge.Kind);
  return true;
}

StructuredTree PhoenixStructurer::structure(const StructuredCFG &Cfg) {
  RegionTree Regions = RegionIdentifier::identifyRoot(Cfg);
  return RecursiveStructurer().structure(Cfg, Regions, *this);
}

NodeId PhoenixStructurer::structureRegion(const StructuredCFG &Cfg,
                                          const Region &R,
                                          StructuredTree &Tree) {
  static const RegionTree EmptyRegions;
  static const std::map<RegionId, NodeId> EmptyChildren;
  return structureRegion(Cfg, EmptyRegions, R, EmptyChildren, Tree);
}

NodeId PhoenixStructurer::structureRegion(
    const StructuredCFG &Cfg, const RegionTree &Regions, const Region &R,
    const std::map<RegionId, NodeId> &StructuredChildren,
    StructuredTree &Tree) {
  MutableRegionGraph Graph =
      StructuredChildren.empty()
          ? MutableRegionGraph::build(Cfg, R)
          : MutableRegionGraph::build(Cfg, Regions, R, StructuredChildren);

  unsigned Iterations = 0;
  bool Changed = false;
  Changed = preprocessRegionGraph(Cfg, R, Graph);
  do {
    Changed = false;
    if (analyzeAcyclic(Cfg, Regions, R, Graph, Tree)) {
      Changed = true;
    } else if (analyzeCyclic(Cfg, Graph, Tree)) {
      Changed = true;
    } else if (refineCyclic(Cfg, Regions, R, Graph, Tree)) {
      Changed = true;
    } else if (lastResortRefinement(Cfg, R, Graph, Tree)) {
      Changed = true;
    }
    ++Iterations;
  } while (Changed && Iterations < 1000);

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;
  std::map<GraphNodeId, std::vector<VirtualEdge>> VirtualEdgesBySource =
      groupVirtualEdgesBySource(Graph);

  std::vector<GraphNodeId> Active = Graph.activeNodes();
  std::sort(Active.begin(), Active.end(), [&](GraphNodeId A, GraphNodeId B) {
    const MutableRegionNode *ANode = Graph.getNode(A);
    const MutableRegionNode *BNode = Graph.getNode(B);
    BlockId ABlock = (ANode == nullptr || ANode->Blocks.empty())
                         ? InvalidBlockId
                         : ANode->Blocks.front();
    BlockId BBlock = (BNode == nullptr || BNode->Blocks.empty())
                         ? InvalidBlockId
                         : BNode->Blocks.front();
    return ABlock < BBlock;
  });

  for (GraphNodeId Id : Active) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node != nullptr) {
      auto It = VirtualEdgesBySource.find(Id);
      static const std::vector<VirtualEdge> EmptyVirtualEdges;
      appendFallbackNode(Cfg, *Node,
                         It == VirtualEdgesBySource.end() ? EmptyVirtualEdges
                                                          : It->second,
                         R, Root, Tree);
    }
  }

  dropGotoIntoFollowingInfiniteLoop(Root, Tree);
  NodeId RootId = Tree.addNode(std::move(Root));
  return wrapNaturalLoopFallback(R, RootId, Tree);
}

} // namespace notdec::backend::structuring
