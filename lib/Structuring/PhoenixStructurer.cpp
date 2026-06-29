#include "notdec-backends/Structuring/PhoenixStructurer.h"

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <limits>
#include <map>
#include <optional>
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

bool isNodeKind(const StructuredTree &Tree, NodeId Id, StructuredNodeKind Kind) {
  const StructuredNode *Node = Tree.getNode(Id);
  return Node != nullptr && Node->Kind == Kind;
}

void dropGotoIntoFollowingNode(StructuredNode &Node,
                               const StructuredTree &Tree);
void cleanupStructuredGotos(const StructuredCFG &Cfg, StructuredTree &Tree,
                            NodeId Id);
void syncVirtualEdgesToOverlay(const MutableRegionGraph &Graph,
                               RegionOverlay &Overlay);
bool nodeTreeContainsKind(const StructuredTree &Tree, NodeId Id,
                          StructuredNodeKind Kind);
bool sourceContainsStructuredSwitch(const MutableRegionNode &Source,
                                    const StructuredTree &Tree);
bool isStructuredLoopKind(StructuredNodeKind Kind);
NodeId rewriteStructuredSourceTargetTransfer(StructuredTree &Tree,
                                             NodeId SourceRoot,
                                             BlockId Target,
                                             VirtualEdgeKind Kind);
GraphNodeId collapseNodesAndSyncOverlay(MutableRegionGraph &Graph,
                                        RegionOverlay *Overlay,
                                        const std::vector<GraphNodeId> &Members,
                                        BlockId RepresentativeBlock,
                                        NodeId StructuredRoot,
                                        bool SelfLoop = true,
                                        bool DropRefinementMarks = false,
                                        std::optional<OverlayNodeKey>
                                            AbsorbedSuccessor = std::nullopt);

bool foldTrailingContinueBreakIfToDoWhile(StructuredNode &LoopNode,
                                          StructuredTree &Tree) {
  if (LoopNode.Kind != StructuredNodeKind::InfiniteLoop ||
      LoopNode.Body == InvalidNodeId) {
    return false;
  }
  const StructuredNode *Body = Tree.getNode(LoopNode.Body);
  if (Body == nullptr || Body->Kind != StructuredNodeKind::Sequence ||
      Body->Children.empty()) {
    return false;
  }

  NodeId TailId = Body->Children.back();
  bool HasTrailingBreak = false;
  if (isNodeKind(Tree, TailId, StructuredNodeKind::Break) &&
      Body->Children.size() >= 2) {
    HasTrailingBreak = true;
    TailId = Body->Children[Body->Children.size() - 2];
  }
  const StructuredNode *Tail = Tree.getNode(TailId);
  if (Tail == nullptr || Tail->Kind != StructuredNodeKind::If ||
      Tail->Then == InvalidNodeId) {
    return false;
  }

  bool ThenContinue = isNodeKind(Tree, Tail->Then, StructuredNodeKind::Continue);
  bool ElseContinue = Tail->Else != InvalidNodeId &&
                      isNodeKind(Tree, Tail->Else, StructuredNodeKind::Continue);
  bool ThenBreak = isNodeKind(Tree, Tail->Then, StructuredNodeKind::Break);
  bool ElseBreak = Tail->Else != InvalidNodeId &&
                   isNodeKind(Tree, Tail->Else, StructuredNodeKind::Break);
  if (Tail->Else == InvalidNodeId && HasTrailingBreak) {
    ElseBreak = true;
  }
  if (!((ThenContinue && ElseBreak) || (ElseContinue && ThenBreak))) {
    return false;
  }

  StructuredNode NewBody = *Body;
  NewBody.Children.pop_back();
  if (HasTrailingBreak) {
    NewBody.Children.pop_back();
  }
  dropGotoIntoFollowingNode(NewBody, Tree);
  LoopNode.Kind = StructuredNodeKind::DoWhile;
  LoopNode.Block = Tail->Block;
  LoopNode.Condition = Tail->Condition;
  LoopNode.ConditionNegated = Tail->ConditionNegated ^ ElseContinue;
  LoopNode.Body = Tree.addNode(std::move(NewBody));
  return true;
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
  const CFGBlock *BodyBlock = Cfg.getBodyBlock(Id);
  if (BodyBlock == nullptr) {
    BodyBlock = Block;
  }

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = Block->Id;
  Body.Statements = BodyBlock->Statements;
  Sequence.Children.push_back(Tree.addNode(std::move(Body)));

  const CFGBlock *SyntheticBlock = Cfg.getBlock(Id);
  if (SyntheticBlock != nullptr &&
      SyntheticBlock->Origin == CFGBlockOrigin::Synthetic &&
      SyntheticBlock->CopyKind == CFGBlockCopyKind::SyntheticGoto &&
      SyntheticBlock->SyntheticTarget != InvalidBlockId) {
    StructuredNode Goto;
    Goto.Kind = StructuredNodeKind::Goto;
    Goto.Target = SyntheticBlock->SyntheticTarget;
    Sequence.Children.push_back(Tree.addNode(std::move(Goto)));
  }
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
                        StructuredTree &Tree, RegionOverlay *Overlay) {
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
    if (R.Kind == RegionKind::NaturalLoop && !To->Blocks.empty() &&
        To->Blocks.front() == R.Head) {
      continue;
    }
    if (collapseCrossesNaturalLoopBoundary(Cfg, Regions, R, Graph,
                                           {FromId, ToId})) {
      continue;
    }

    std::vector<BlockId> Blocks = From->Blocks;
    Blocks.insert(Blocks.end(), To->Blocks.begin(), To->Blocks.end());
    NodeId Root = buildSequenceNode(Cfg, {From, To}, Tree);
    collapseNodesAndSyncOverlay(Graph, Overlay, {FromId, ToId}, From->Block,
                                Root);
    return true;
  }
  return false;
}

bool reduceIfOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                  const RegionTree &Regions, const Region &R,
                  StructuredTree &Tree, RegionOverlay *Overlay) {
  for (GraphNodeId HeaderId : Graph.activeNodes()) {
    const MutableRegionNode *Header = Graph.getNode(HeaderId);
    if (Header == nullptr || Header->Blocks.empty() ||
        Header->Succs.size() != 2) {
      continue;
    }
    if (R.Kind == RegionKind::NaturalLoop && Header->Blocks.front() == R.Head) {
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
    if (TrueNode->ExternalPlaceholder || FalseNode->ExternalPlaceholder) {
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
    collapseNodesAndSyncOverlay(Graph, Overlay, Members, Header->Block, Root);
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
                      StructuredTree &Tree, RegionOverlay *Overlay) {
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
    SwitchNode.DefaultTarget = Tail->Successors.front();
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

    collapseNodesAndSyncOverlay(Graph, Overlay, MemberList, Header->Block,
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
                           StructuredTree &Tree, RegionOverlay *Overlay) {
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
    collapseNodesAndSyncOverlay(Graph, Overlay, Members, Header->Block,
                                Tree.addNode(std::move(WhileNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
    return true;
  }
  return false;
}

bool reduceLinearWhileWithBreakOnce(const StructuredCFG &Cfg,
                                    MutableRegionGraph &Graph,
                                    StructuredTree &Tree,
                                    RegionOverlay *Overlay) {
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
    collapseNodesAndSyncOverlay(Graph, Overlay, Members, Header->Block,
                                Tree.addNode(std::move(WhileNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
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
                             MutableRegionGraph &Graph, StructuredTree &Tree,
                             RegionOverlay *Overlay) {
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
    collapseNodesAndSyncOverlay(Graph, Overlay, BodyIds, Entry->Block,
                                Tree.addNode(std::move(DoWhileNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
    return true;
  }
  return false;
}

bool reduceSelfLoopOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
                        StructuredTree &Tree, RegionOverlay *Overlay) {
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
    } else if ((Header->Succs.size() == 2 ||
                (Header->Succs.size() == 1 &&
                 Header->ExternalSuccs.size() == 1)) &&
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

    foldTrailingContinueBreakIfToDoWhile(LoopNode, Tree);
    collapseNodesAndSyncOverlay(Graph, Overlay, {HeaderId}, Header->Block,
                                Tree.addNode(std::move(LoopNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
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

std::vector<VirtualEdge> collectOverlayVirtualizableEdges(
    const Region &R, const MutableRegionGraph &Graph,
    const RegionOverlay *Overlay) {
  if (Overlay == nullptr || Overlay->manager() == nullptr) {
    return collectVirtualizableEdges(R, Graph);
  }

  std::map<OverlayNodeKey, GraphNodeId> NodeBySource;
  for (const MutableRegionNode &Node : Graph.nodes()) {
    if (!Node.Active) {
      continue;
    }
    for (const OverlayNodeKey &Source : Node.SourceNodes) {
      NodeBySource.emplace(Source, Node.Id);
    }
  }

  std::vector<VirtualEdge> Edges;
  std::map<OverlayNodeKey, unsigned> NodeOrder =
      Overlay->manager()->quasiTopologicalNodeOrder(Overlay->id());
  for (const OverlayViewEdge &Edge :
       Overlay->manager()->quotientEdgesAcyclic(
           Overlay->id(), /*IncludeSuccessors=*/true, NodeOrder)) {
    if (!Edge.sourcesMember()) {
      continue;
    }
    OverlayNodeKey FromNode = Overlay->manager()->nodeKey(Edge.From);
    auto FromIt = NodeBySource.find(FromNode);
    if (FromIt == NodeBySource.end()) {
      continue;
    }

    OverlayNodeKey ToNode =
        Edge.targetsMember() ? Overlay->manager()->nodeKey(Edge.To)
                             : Edge.targetNode();
    auto ToIt = NodeBySource.find(ToNode);
    if (ToIt == NodeBySource.end()) {
      continue;
    }

    const MutableRegionNode *FromGraphNode = Graph.getNode(FromIt->second);
    const MutableRegionNode *ToGraphNode = Graph.getNode(ToIt->second);
    if (FromGraphNode == nullptr || ToGraphNode == nullptr ||
        FromGraphNode->ExternalPlaceholder || !FromGraphNode->Active ||
        !ToGraphNode->Active) {
      continue;
    }
    if (!Graph.hasEdge(FromGraphNode->Id, ToGraphNode->Id)) {
      continue;
    }

    BlockId FromBlock = FromGraphNode->Blocks.empty()
                            ? InvalidBlockId
                            : FromGraphNode->Blocks.back();
    BlockId ToBlock = ToGraphNode->Blocks.empty()
                          ? InvalidBlockId
                          : ToGraphNode->Blocks.front();
    VirtualEdgeKind Kind = VirtualEdgeKind::Goto;
    if (R.Kind == RegionKind::NaturalLoop && ToBlock == R.Head) {
      Kind = VirtualEdgeKind::Continue;
    }
    Edges.push_back(
        {FromGraphNode->Id, ToGraphNode->Id, FromBlock, ToBlock, Kind});
  }

  return Edges;
}

std::vector<VirtualEdge>
filterByAngrLastResortPriority(const Region &R,
                               const MutableRegionGraphAnalysis &Analysis,
                               const std::vector<VirtualEdge> &Edges,
                               bool IncludeAcyclicDroppedEdges = true) {
  // Match Angr Phoenix's last-resort buckets: prefer edges where neither end
  // dominates the other, then edges whose source does not dominate the target,
  // then the remaining edges. If the root acyclic view had to drop
  // cycle-closing edges, use them only after the Angr buckets are empty. The
  // final per-bucket order is still delegated to Phoenix/SAILR.
  std::vector<VirtualEdge> NoDominanceEdges;
  std::vector<VirtualEdge> SecondaryEdges;
  std::vector<VirtualEdge> OtherEdges;

  for (const VirtualEdge &Edge : Edges) {
    bool FromDominatesTo = Analysis.dominates(Edge.From, Edge.To);
    bool ToDominatesFrom = Analysis.dominates(Edge.To, Edge.From);
    if (!FromDominatesTo && !ToDominatesFrom) {
      NoDominanceEdges.push_back(Edge);
    } else if (!FromDominatesTo) {
      SecondaryEdges.push_back(Edge);
    } else {
      OtherEdges.push_back(Edge);
    }
  }

  if (!NoDominanceEdges.empty()) {
    return NoDominanceEdges;
  }
  if (!SecondaryEdges.empty()) {
    return SecondaryEdges;
  }
  if (!OtherEdges.empty()) {
    return OtherEdges;
  }
  if (IncludeAcyclicDroppedEdges && R.Kind == RegionKind::Root) {
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

void syncVirtualEdgesToOverlay(const MutableRegionGraph &Graph,
                               RegionOverlay &Overlay) {
  OverlayManager *Manager = Overlay.manager();
  if (Manager == nullptr) {
    return;
  }
  for (const VirtualEdge &Edge : Graph.virtualEdges()) {
    if (Edge.FromBlock == InvalidBlockId || Edge.ToBlock == InvalidBlockId) {
      continue;
    }
    Manager->detachNodeEdge(OverlayNodeKey::block(Edge.FromBlock),
                            OverlayNodeKey::block(Edge.ToBlock));
  }
}

void applyOverlayNodeOrder(const MutableRegionGraph &Graph,
                           const RegionOverlay *Overlay,
                           MutableRegionGraphAnalysis &Analysis) {
  if (Overlay == nullptr || Overlay->manager() == nullptr) {
    return;
  }

  std::map<OverlayNodeKey, unsigned> OverlayOrder =
      Overlay->manager()->quasiTopologicalNodeOrder(Overlay->id());
  if (OverlayOrder.empty()) {
    return;
  }

  std::map<GraphNodeId, unsigned> MappedOrder = Analysis.NodeOrder;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node == nullptr || Node->SourceNodes.empty()) {
      continue;
    }

    unsigned Best = std::numeric_limits<unsigned>::max();
    bool Found = false;
    for (const OverlayNodeKey &Source : Node->SourceNodes) {
      auto It = OverlayOrder.find(Source);
      if (It == OverlayOrder.end()) {
        continue;
      }
      Best = std::min(Best, It->second);
      Found = true;
    }
    if (Found) {
      MappedOrder[Id] = Best;
    }
  }
  Analysis.NodeOrder = std::move(MappedOrder);
}

std::vector<OverlayNodeKey>
collectSourceNodesForCollapse(const MutableRegionGraph &Graph,
                              const std::vector<GraphNodeId> &Members) {
  std::vector<OverlayNodeKey> SourceNodes;
  auto AppendUnique = [&](const OverlayNodeKey &Key) {
    if (std::find(SourceNodes.begin(), SourceNodes.end(), Key) ==
        SourceNodes.end()) {
      SourceNodes.push_back(Key);
    }
  };
  for (GraphNodeId Id : Members) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node == nullptr || !Node->Active) {
      continue;
    }
    for (const OverlayNodeKey &Key : Node->SourceNodes) {
      AppendUnique(Key);
    }
  }
  return SourceNodes;
}

bool isSameStructuredSource(const std::vector<OverlayNodeKey> &SourceNodes,
                            RegionId Region, NodeId Root) {
  return SourceNodes.size() == 1 &&
         SourceNodes.front() == OverlayNodeKey::structured(Root, Region);
}

GraphNodeId collapseNodesAndSyncOverlay(MutableRegionGraph &Graph,
                                        RegionOverlay *Overlay,
                                        const std::vector<GraphNodeId> &Members,
                                        BlockId RepresentativeBlock,
                                        NodeId StructuredRoot,
                                        bool SelfLoop,
                                        bool DropRefinementMarks,
                                        std::optional<OverlayNodeKey>
                                            AbsorbedSuccessor) {
  std::vector<OverlayNodeKey> SourceNodes =
      Overlay == nullptr ? std::vector<OverlayNodeKey>{}
                         : collectSourceNodesForCollapse(Graph, Members);
  GraphNodeId Collapsed =
      Graph.collapseNodes(Members, RepresentativeBlock, StructuredRoot);
  if (Overlay == nullptr || Collapsed == InvalidGraphNodeId ||
      StructuredRoot == InvalidNodeId || SourceNodes.empty()) {
    return Collapsed;
  }

  syncVirtualEdgesToOverlay(Graph, *Overlay);
  if (!isSameStructuredSource(SourceNodes, Overlay->id(), StructuredRoot)) {
    Overlay->replaceNodes(SourceNodes, StructuredRoot, AbsorbedSuccessor,
                          SelfLoop);
  } else if (AbsorbedSuccessor) {
    Overlay->absorbSuccessorInto(
        OverlayEdgeEndpoint::external(*AbsorbedSuccessor),
        OverlayEdgeEndpoint::member(
            OverlayMember::structured(StructuredRoot, Overlay->id())));
  }
  if (DropRefinementMarks) {
    Overlay->dropEdgeMarksFrom(
        OverlayNodeKey::structured(StructuredRoot, Overlay->id()),
        "cyclic_refinement_outgoing");
  }

  // After the shared graph has been replaced, the next reducer step must refer
  // to the new structured overlay node, not to blocks that no longer exist.
  MutableRegionNode *CollapsedNode = Graph.getNode(Collapsed);
  if (CollapsedNode != nullptr) {
    CollapsedNode->SourceNodes = {
        OverlayNodeKey::structured(StructuredRoot, Overlay->id())};
  }
  return Collapsed;
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

NodeId buildTerminalBlockBody(const StructuredCFG &Cfg, BlockId Target,
                              StructuredTree &Tree) {
  const CFGBlock *TargetBlock = Cfg.getBlock(Target);
  if (TargetBlock == nullptr ||
      (TargetBlock->Terminator != TerminatorKind::Return &&
       TargetBlock->Terminator != TerminatorKind::Unreachable)) {
    return InvalidNodeId;
  }

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::Sequence;
  appendBlockBody(Cfg, Target, Body, Tree);

  StructuredNode Terminal;
  Terminal.Kind = TargetBlock->Terminator == TerminatorKind::Return
                      ? StructuredNodeKind::Return
                      : StructuredNodeKind::Unreachable;
  Body.Children.push_back(Tree.addNode(std::move(Terminal)));
  return Tree.addNode(std::move(Body));
}

NodeId buildSwitchTargetBody(const StructuredCFG &Cfg, const Region &R,
                             BlockId Target, VirtualEdgeKind Kind,
                             StructuredTree &Tree) {
  if (NodeId TerminalBody = buildTerminalBlockBody(Cfg, Target, Tree);
      TerminalBody != InvalidNodeId) {
    return TerminalBody;
  }
  return Tree.addNode(makeControlTransfer(
      Target, Kind == VirtualEdgeKind::Goto
                  ? classifyNaturalLoopExit(Cfg, R, Target)
                  : Kind));
}

NodeId buildVirtualizedSwitchSource(const CFGBlock &Tail,
                                    const MutableRegionNode &Source,
                                    const VirtualEdge &Edge, const Region &R,
                                    const StructuredCFG &Cfg,
                                    StructuredTree &Tree) {
  if (Source.StructuredRoot != InvalidNodeId &&
      (nodeTreeContainsKind(Tree, Source.StructuredRoot,
                            StructuredNodeKind::While) ||
       nodeTreeContainsKind(Tree, Source.StructuredRoot,
                            StructuredNodeKind::DoWhile) ||
       nodeTreeContainsKind(Tree, Source.StructuredRoot,
                            StructuredNodeKind::InfiniteLoop))) {
    return InvalidNodeId;
  }

  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  appendSourceBody(Cfg, Source, Sequence, Tree);

  StructuredNode SwitchNode;
  SwitchNode.Kind = StructuredNodeKind::Switch;
  SwitchNode.Block = Tail.Id;
  SwitchNode.Condition = Tail.Condition;

  if (R.Kind == RegionKind::NaturalLoop) {
    if (Source.StructuredRoot != InvalidNodeId &&
        sourceContainsStructuredSwitch(Source, Tree)) {
      return InvalidNodeId;
    }

    auto TransferKindFor = [&](BlockId Target) {
      if (Target == Edge.ToBlock && Edge.Kind != VirtualEdgeKind::Goto) {
        return Edge.Kind;
      }
      return classifyNaturalLoopExit(Cfg, R, Target);
    };

    bool Matched = false;
    if (!Tail.Successors.empty()) {
      SwitchNode.DefaultTarget = Tail.Successors[0];
      SwitchNode.Default =
          buildSwitchTargetBody(Cfg, R, Tail.Successors[0],
                                TransferKindFor(Tail.Successors[0]), Tree);
      Matched = Tail.Successors[0] == Edge.ToBlock;
    }
    for (const SwitchCase &Case : Tail.Cases) {
      NodeId CaseBody = buildSwitchTargetBody(
          Cfg, R, Case.Target, TransferKindFor(Case.Target), Tree);
      SwitchNode.StructuredCases.push_back({Case.Value, Case.Target, CaseBody});
      Matched |= Case.Target == Edge.ToBlock;
    }
    if (!Matched) {
      return InvalidNodeId;
    }

    Sequence.Children.push_back(Tree.addNode(std::move(SwitchNode)));
    return Tree.addNode(std::move(Sequence));
  }

  SwitchNode.Cases = Tail.Cases;

  bool Matched = false;
  if (!Tail.Successors.empty() && Tail.Successors[0] == Edge.ToBlock) {
    SwitchNode.DefaultTarget = Tail.Successors[0];
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

std::vector<OverlayNodeKey>
overlayNodesForGraphNode(const MutableRegionNode *Node, BlockId FallbackBlock) {
  if (Node != nullptr && !Node->SourceNodes.empty()) {
    return Node->SourceNodes;
  }
  if (FallbackBlock != InvalidBlockId) {
    return {OverlayNodeKey::block(FallbackBlock)};
  }
  return {};
}

void detachOverlayVirtualEdge(RegionOverlay *Overlay,
                              const std::vector<OverlayNodeKey> &FromNodes,
                              const std::vector<OverlayNodeKey> &ToNodes) {
  if (Overlay == nullptr || Overlay->manager() == nullptr) {
    return;
  }
  for (const OverlayNodeKey &From : FromNodes) {
    for (const OverlayNodeKey &To : ToNodes) {
      Overlay->manager()->detachNodeEdge(From, To);
    }
  }
}

void markOverlayRefinementEdge(RegionOverlay *Overlay,
                               const std::vector<OverlayNodeKey> &FromNodes,
                               const std::vector<OverlayNodeKey> &ToNodes) {
  if (Overlay == nullptr || Overlay->manager() == nullptr) {
    return;
  }
  for (const OverlayNodeKey &From : FromNodes) {
    for (const OverlayNodeKey &To : ToNodes) {
      Overlay->manager()->markNodeEdge(Overlay->id(), From, To,
                                       "cyclic_refinement_outgoing");
    }
  }
}

bool installVirtualizedEdge(const StructuredCFG &Cfg, const Region &R,
                            MutableRegionGraph &Graph, StructuredTree &Tree,
                            RegionOverlay *Overlay,
                            const VirtualEdge &Edge,
                            bool RewriteSource = true) {
  if (!Graph.hasEdge(Edge.From, Edge.To)) {
    return false;
  }

  const MutableRegionNode *Source = Graph.getNode(Edge.From);
  const MutableRegionNode *Target = Graph.getNode(Edge.To);
  std::vector<OverlayNodeKey> FromNodes =
      overlayNodesForGraphNode(Source, Edge.FromBlock);
  std::vector<OverlayNodeKey> ToNodes =
      overlayNodesForGraphNode(Target, Edge.ToBlock);

  NodeId Replacement = InvalidNodeId;
  if (RewriteSource && Source != nullptr) {
    Replacement = buildVirtualizedSource(Cfg, *Source, Edge, R, Tree);
    if (Replacement != InvalidNodeId) {
      Graph.setStructuredRoot(Edge.From, Replacement);
    }
  }

  // Angr detaches the graph edge before replace_nodes_both(). This lets the
  // replacement inherit only the remaining real outgoing edges while the
  // rewritten tree keeps the virtualized control transfer explicitly.
  Graph.virtualizeEdge(Edge.From, Edge.To, Edge.Kind);
  detachOverlayVirtualEdge(Overlay, FromNodes, ToNodes);

  if (Overlay != nullptr && Replacement != InvalidNodeId &&
      !FromNodes.empty()) {
    Overlay->replaceNodes(FromNodes, Replacement);
    Graph.setSourceNodes(
        Edge.From, {OverlayNodeKey::structured(Replacement, Overlay->id())});
  }
  return true;
}

bool rewriteLoopSuccessorExits(const StructuredCFG &Cfg,
                               const Region &LoopRegion,
                               const std::set<GraphNodeId> &Members,
                               BlockId FollowBlock, GraphNodeId HeadId,
                               MutableRegionGraph &Graph,
                               StructuredTree &Tree, RegionOverlay *Overlay) {
  if (Overlay == nullptr || FollowBlock == InvalidBlockId) {
    return false;
  }

  bool Changed = false;
  for (GraphNodeId SourceId : Members) {
    const MutableRegionNode *Source = Graph.getNode(SourceId);
    if (Source == nullptr || SourceId == HeadId ||
        Source->TailBlock == LoopRegion.Latch) {
      continue;
    }

    for (GraphNodeId SuccId : Source->Succs) {
      if (Members.count(SuccId)) {
        continue;
      }
      const MutableRegionNode *Succ = Graph.getNode(SuccId);
      if (Succ == nullptr || Succ->Blocks.empty() ||
          Succ->Blocks.front() != FollowBlock) {
        continue;
      }

      std::vector<OverlayNodeKey> FromNodes =
          overlayNodesForGraphNode(Source, Source->TailBlock);
      std::vector<OverlayNodeKey> ToNodes =
          overlayNodesForGraphNode(Succ, FollowBlock);
      markOverlayRefinementEdge(Overlay, FromNodes, ToNodes);

      NodeId Replacement = InvalidNodeId;
      if (Source->StructuredRoot != InvalidNodeId) {
        Replacement = rewriteStructuredSourceTargetTransfer(
            Tree, Source->StructuredRoot, FollowBlock, VirtualEdgeKind::Break);
      }

      if (Replacement == InvalidNodeId &&
          !sourceContainsStructuredSwitch(*Source, Tree)) {
        VirtualEdge Edge{SourceId, SuccId, Source->TailBlock, FollowBlock,
                         VirtualEdgeKind::Break};
        Replacement = buildVirtualizedSource(Cfg, *Source, Edge, LoopRegion,
                                             Tree);
      }

      if (Replacement != InvalidNodeId && !FromNodes.empty()) {
        Graph.setStructuredRoot(SourceId, Replacement);
        Overlay->replaceNodes(FromNodes, Replacement);
        Graph.setSourceNodes(
            SourceId,
            {OverlayNodeKey::structured(Replacement, Overlay->id())});
      }
      Changed = true;
    }
  }
  return Changed;
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

bool nodeTreeContainsControlTransfer(const StructuredTree &Tree, NodeId Id) {
  return nodeTreeContainsKind(Tree, Id, StructuredNodeKind::Goto) ||
         nodeTreeContainsKind(Tree, Id, StructuredNodeKind::Break) ||
         nodeTreeContainsKind(Tree, Id, StructuredNodeKind::Continue);
}

bool isTargetedControlTransfer(const StructuredNode &Node, BlockId Target) {
  return (Node.Kind == StructuredNodeKind::Goto ||
          Node.Kind == StructuredNodeKind::Break ||
          Node.Kind == StructuredNodeKind::Continue) &&
         Node.Target == Target;
}

NodeId copyReplacingTargetTransfer(StructuredTree &Tree, NodeId Id,
                                   BlockId Target, VirtualEdgeKind Kind,
                                   bool EnterLoops, bool &Changed) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return InvalidNodeId;
  }

  if (isTargetedControlTransfer(*Node, Target)) {
    Changed = true;
    return Tree.addNode(makeControlTransfer(Target, Kind));
  }

  if (Node->Kind == StructuredNodeKind::Switch ||
      (!EnterLoops && isStructuredLoopKind(Node->Kind))) {
    return Id;
  }

  StructuredNode Copy = *Node;
  bool LocalChanged = false;
  for (NodeId &Child : Copy.Children) {
    bool ChildChanged = false;
    NodeId NewChild = copyReplacingTargetTransfer(Tree, Child, Target, Kind,
                                                  EnterLoops, ChildChanged);
    if (ChildChanged) {
      Child = NewChild;
      LocalChanged = true;
    }
  }
  for (StructuredSwitchCase &Case : Copy.StructuredCases) {
    bool CaseChanged = false;
    NodeId NewBody = copyReplacingTargetTransfer(Tree, Case.Body, Target, Kind,
                                                 EnterLoops, CaseChanged);
    if (CaseChanged) {
      Case.Body = NewBody;
      LocalChanged = true;
    }
  }

  auto RewriteChild = [&](NodeId &Child) {
    bool ChildChanged = false;
    NodeId NewChild = copyReplacingTargetTransfer(Tree, Child, Target, Kind,
                                                  EnterLoops, ChildChanged);
    if (ChildChanged) {
      Child = NewChild;
      LocalChanged = true;
    }
  };
  RewriteChild(Copy.Then);
  RewriteChild(Copy.Else);
  RewriteChild(Copy.Body);
  RewriteChild(Copy.Default);

  if (!LocalChanged) {
    return Id;
  }
  Changed = true;
  return Tree.addNode(std::move(Copy));
}

NodeId rewriteStructuredSourceTargetTransfer(StructuredTree &Tree,
                                             NodeId SourceRoot,
                                             BlockId Target,
                                             VirtualEdgeKind Kind) {
  bool Changed = false;
  NodeId Replacement = copyReplacingTargetTransfer(
      Tree, SourceRoot, Target, Kind, /*EnterLoops=*/false, Changed);
  return Changed ? Replacement : InvalidNodeId;
}

bool sourceContainsStructuredSwitch(const MutableRegionNode &Source,
                                    const StructuredTree &Tree) {
  return Source.StructuredRoot != InvalidNodeId &&
         nodeTreeContainsKind(Tree, Source.StructuredRoot,
                              StructuredNodeKind::Switch);
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
    if (nodeTreeContainsStructuredControl(Tree, Node.StructuredRoot) ||
        nodeTreeContainsControlTransfer(Tree, Node.StructuredRoot)) {
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
      SwitchNode.DefaultTarget = Tail->Successors[0];
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

bool isStructuredLoopKind(StructuredNodeKind Kind) {
  return Kind == StructuredNodeKind::While ||
         Kind == StructuredNodeKind::DoWhile ||
         Kind == StructuredNodeKind::InfiniteLoop;
}

bool startsWithStructuredLoop(const StructuredTree &Tree, NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (isStructuredLoopKind(Node->Kind)) {
    return true;
  }
  if (Node->Kind == StructuredNodeKind::Sequence && !Node->Children.empty()) {
    return startsWithStructuredLoop(Tree, Node->Children.front());
  }
  return false;
}

NodeId wrapNaturalLoopFallback(const Region &R, NodeId Body,
                               StructuredTree &Tree) {
  if (R.Kind != RegionKind::NaturalLoop ||
      startsWithStructuredLoop(Tree, Body) ||
      !hasDirectLoopControlTransfer(Tree, Body)) {
    return Body;
  }

  StructuredNode LoopNode;
  LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
  LoopNode.Block = R.Head;
  LoopNode.Body = Body;
  return Tree.addNode(std::move(LoopNode));
}

BlockId firstRenderedBlock(const StructuredTree &Tree, NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return InvalidBlockId;
  }
  if ((Node->Kind == StructuredNodeKind::Label ||
       Node->Kind == StructuredNodeKind::BasicBlock) &&
      Node->Block != InvalidBlockId) {
    return Node->Block;
  }
  for (NodeId Child : Node->Children) {
    BlockId Block = firstRenderedBlock(Tree, Child);
    if (Block != InvalidBlockId) {
      return Block;
    }
  }
  return firstRenderedBlock(Tree, Node->Body);
}

BlockId structuredLoopEntryBlock(const StructuredTree &Tree, NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return InvalidBlockId;
  }
  if (Node->Kind == StructuredNodeKind::DoWhile) {
    return firstRenderedBlock(Tree, Node->Body);
  }
  if (isStructuredLoopKind(Node->Kind)) {
    return Node->Block;
  }
  if (Node->Kind == StructuredNodeKind::Sequence && !Node->Children.empty()) {
    return structuredLoopEntryBlock(Tree, Node->Children.front());
  }
  return InvalidBlockId;
}

void dropGotoIntoFollowingNode(StructuredNode &Node,
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
        (Current->Target ==
             structuredLoopEntryBlock(Tree, Node.Children[Index + 1]) ||
         (Next->Kind == StructuredNodeKind::Label &&
          Current->Target == Next->Block))) {
      continue;
    }
    Filtered.push_back(Node.Children[Index]);
  }
  Node.Children = std::move(Filtered);
}

bool labelBlock(const StructuredTree &Tree, NodeId Id, BlockId &Block) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr || Node->Kind != StructuredNodeKind::Label) {
    return false;
  }
  Block = Node->Block;
  return true;
}

bool gotoTarget(const StructuredTree &Tree, NodeId Id, BlockId &Target) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr || Node->Kind != StructuredNodeKind::Goto) {
    return false;
  }
  Target = Node->Target;
  return true;
}

bool gotoTargetDeep(const StructuredTree &Tree, NodeId Id, BlockId &Target) {
  if (gotoTarget(Tree, Id, Target)) {
    return true;
  }
  const StructuredNode *Node = Tree.getNode(Id);
  return Node != nullptr && Node->Kind == StructuredNodeKind::Sequence &&
         Node->Children.size() == 1 &&
         gotoTarget(Tree, Node->Children.front(), Target);
}

NodeId buildSequenceFromRange(StructuredTree &Tree,
                              const std::vector<NodeId> &Children,
                              unsigned Begin, unsigned End) {
  StructuredNode Sequence;
  Sequence.Kind = StructuredNodeKind::Sequence;
  for (unsigned I = Begin; I < End; ++I) {
    Sequence.Children.push_back(Children[I]);
  }
  return Tree.addNode(std::move(Sequence));
}

bool reachesJoinBlock(const StructuredCFG &Cfg, const StructuredTree &Tree,
                      NodeId Id, BlockId Join) {
  BlockId Target = InvalidBlockId;
  if (gotoTargetDeep(Tree, Id, Target)) {
    return Target == Join;
  }
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == StructuredNodeKind::BasicBlock) {
    const CFGBlock *Block = Cfg.getBlock(Node->Block);
    return Block != nullptr && hasSuccessorTarget(*Block, Join);
  }
  if (Node->Kind == StructuredNodeKind::Sequence && !Node->Children.empty()) {
    return reachesJoinBlock(Cfg, Tree, Node->Children.back(), Join);
  }
  return false;
}

bool foldGotoDiamond(const StructuredCFG &Cfg, StructuredTree &Tree,
                     NodeId SequenceId) {
  const StructuredNode *OldSequence = Tree.getNode(SequenceId);
  if (OldSequence == nullptr ||
      OldSequence->Kind != StructuredNodeKind::Sequence ||
      OldSequence->Children.size() < 6) {
    return false;
  }

  for (unsigned IfIndex = 0; IfIndex < OldSequence->Children.size(); ++IfIndex) {
    const StructuredNode *IfNode = Tree.getNode(OldSequence->Children[IfIndex]);
    if (IfNode == nullptr || IfNode->Kind != StructuredNodeKind::If ||
        IfNode->Then == InvalidNodeId || IfNode->Else == InvalidNodeId) {
      continue;
    }

    BlockId ThenTarget = InvalidBlockId;
    BlockId ElseTarget = InvalidBlockId;
    if (!gotoTargetDeep(Tree, IfNode->Then, ThenTarget) ||
        !gotoTargetDeep(Tree, IfNode->Else, ElseTarget)) {
      continue;
    }

    unsigned ThenLabel = OldSequence->Children.size();
    unsigned ElseLabel = OldSequence->Children.size();
    for (unsigned I = IfIndex + 1; I < OldSequence->Children.size(); ++I) {
      BlockId Label = InvalidBlockId;
      if (!labelBlock(Tree, OldSequence->Children[I], Label)) {
        continue;
      }
      if (Label == ThenTarget && ThenLabel == OldSequence->Children.size()) {
        ThenLabel = I;
      }
      if (Label == ElseTarget && ElseLabel == OldSequence->Children.size()) {
        ElseLabel = I;
      }
    }
    if (ThenLabel == OldSequence->Children.size() ||
        ElseLabel == OldSequence->Children.size() || ThenLabel == ElseLabel) {
      continue;
    }

    unsigned FirstLabel = std::min(ThenLabel, ElseLabel);
    unsigned SecondLabel = std::max(ThenLabel, ElseLabel);
    if (SecondLabel + 1 >= OldSequence->Children.size()) {
      continue;
    }

    unsigned JoinLabel = OldSequence->Children.size();
    for (unsigned I = SecondLabel + 1; I < OldSequence->Children.size(); ++I) {
      BlockId Label = InvalidBlockId;
      if (!labelBlock(Tree, OldSequence->Children[I], Label)) {
        continue;
      }
      if (reachesJoinBlock(Cfg, Tree, OldSequence->Children[SecondLabel - 1],
                           Label) &&
          reachesJoinBlock(Cfg, Tree, OldSequence->Children[I - 1], Label)) {
        JoinLabel = I;
        break;
      }
    }
    if (JoinLabel == OldSequence->Children.size()) {
      continue;
    }

    StructuredNode FoldedIf = *IfNode;
    unsigned ThenBegin = ThenLabel + 1;
    unsigned ThenEnd = ThenLabel == FirstLabel ? SecondLabel : JoinLabel;
    unsigned ElseBegin = ElseLabel + 1;
    unsigned ElseEnd = ElseLabel == FirstLabel ? SecondLabel : JoinLabel;
    BlockId JoinBlock = InvalidBlockId;
    if (!labelBlock(Tree, OldSequence->Children[JoinLabel], JoinBlock)) {
      continue;
    }
    BlockId TailTarget = InvalidBlockId;
    if (ThenEnd > ThenBegin &&
        gotoTarget(Tree, OldSequence->Children[ThenEnd - 1], TailTarget) &&
        TailTarget == JoinBlock) {
      --ThenEnd;
    }
    TailTarget = InvalidBlockId;
    if (ElseEnd > ElseBegin &&
        gotoTarget(Tree, OldSequence->Children[ElseEnd - 1], TailTarget) &&
        TailTarget == JoinBlock) {
      --ElseEnd;
    }
    if (ThenBegin >= ThenEnd || ElseBegin >= ElseEnd) {
      continue;
    }
    FoldedIf.Then =
        buildSequenceFromRange(Tree, OldSequence->Children, ThenBegin, ThenEnd);
    FoldedIf.Else =
        buildSequenceFromRange(Tree, OldSequence->Children, ElseBegin, ElseEnd);
    NodeId FoldedIfId = Tree.addNode(std::move(FoldedIf));

    std::vector<NodeId> NewChildren;
    NewChildren.insert(NewChildren.end(), OldSequence->Children.begin(),
                       OldSequence->Children.begin() + IfIndex);
    NewChildren.push_back(FoldedIfId);
    NewChildren.insert(NewChildren.end(),
                       OldSequence->Children.begin() + JoinLabel,
                       OldSequence->Children.end());
    StructuredNode &Sequence = Tree.nodes()[SequenceId];
    Sequence.Children = std::move(NewChildren);
    return true;
  }
  return false;
}

void cleanupStructuredGotos(const StructuredCFG &Cfg, StructuredTree &Tree,
                            NodeId Id) {
  if (Id == InvalidNodeId || Id >= Tree.nodes().size()) {
    return;
  }
  StructuredNode NodeCopy = Tree.nodes()[Id];
  for (NodeId Child : NodeCopy.Children) {
    cleanupStructuredGotos(Cfg, Tree, Child);
  }
  for (const StructuredSwitchCase &Case : NodeCopy.StructuredCases) {
    cleanupStructuredGotos(Cfg, Tree, Case.Body);
  }
  cleanupStructuredGotos(Cfg, Tree, NodeCopy.Then);
  cleanupStructuredGotos(Cfg, Tree, NodeCopy.Else);
  cleanupStructuredGotos(Cfg, Tree, NodeCopy.Body);
  cleanupStructuredGotos(Cfg, Tree, NodeCopy.Default);
  Tree.nodes()[Id] = std::move(NodeCopy);
  StructuredNode &Node = Tree.nodes()[Id];
  dropGotoIntoFollowingNode(Node, Tree);
  while (foldGotoDiamond(Cfg, Tree, Id)) {
    StructuredNode &Updated = Tree.nodes()[Id];
    dropGotoIntoFollowingNode(Updated, Tree);
  }
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

BlockId chooseNaturalLoopSuccessor(const MutableRegionGraph &Graph,
                                   const std::set<GraphNodeId> &Members,
                                   const std::vector<BlockId> &Successors) {
  if (Successors.empty()) {
    return InvalidBlockId;
  }

  std::map<BlockId, unsigned> EdgeCounts;
  for (BlockId Successor : Successors) {
    EdgeCounts[Successor] = 0;
  }
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
      if (Succ != nullptr && !Succ->Blocks.empty()) {
        ++EdgeCounts[Succ->Blocks.front()];
      }
    }
    for (BlockId Succ : Node->ExternalSuccs) {
      ++EdgeCounts[Succ];
    }
  }

  return *std::min_element(
      Successors.begin(), Successors.end(), [&](BlockId A, BlockId B) {
        unsigned ACount = EdgeCounts[A];
        unsigned BCount = EdgeCounts[B];
        if (ACount != BCount) {
          return ACount > BCount;
        }
        return A < B;
      });
}

BlockId findGraphWhileSuccessor(const StructuredCFG &Cfg,
                                const MutableRegionGraph &Graph,
                                const std::set<GraphNodeId> &MemberSet,
                                GraphNodeId HeadId) {
  const MutableRegionNode *Head = Graph.getNode(HeadId);
  if (Head == nullptr || Head->Blocks.empty()) {
    return InvalidBlockId;
  }

  const CFGBlock *HeadBlock = Cfg.getBlock(Head->Blocks.back());
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Branch ||
      !HeadBlock->Statements.empty() || HeadBlock->Successors.size() != 2) {
    return InvalidBlockId;
  }
  for (BlockId PrefixBlock : std::vector<BlockId>(Head->Blocks.begin(),
                                                  Head->Blocks.end() - 1)) {
    const CFGBlock *Block = Cfg.getBlock(PrefixBlock);
    if (Block == nullptr || !Block->Statements.empty()) {
      return InvalidBlockId;
    }
  }

  GraphNodeId TrueId = Graph.getNodeForBlock(HeadBlock->Successors[0]);
  GraphNodeId FalseId = Graph.getNodeForBlock(HeadBlock->Successors[1]);
  bool TrueInLoop = MemberSet.count(TrueId) != 0;
  bool FalseInLoop = MemberSet.count(FalseId) != 0;
  if (TrueInLoop == FalseInLoop) {
    return InvalidBlockId;
  }
  return TrueInLoop ? HeadBlock->Successors[1] : HeadBlock->Successors[0];
}

BlockId findGraphDoWhileSuccessor(const StructuredCFG &Cfg,
                                  const MutableRegionGraph &Graph,
                                  BlockId LatchBlockId,
                                  GraphNodeId HeadId) {
  if (LatchBlockId == InvalidBlockId) {
    return InvalidBlockId;
  }

  const MutableRegionNode *Head = Graph.getNode(HeadId);
  const CFGBlock *LatchBlock = Cfg.getBlock(LatchBlockId);
  if (Head == nullptr || Head->Blocks.empty() || LatchBlock == nullptr ||
      LatchBlock->Terminator != TerminatorKind::Branch ||
      LatchBlock->Successors.size() != 2) {
    return InvalidBlockId;
  }

  bool TrueIsHead = LatchBlock->Successors[0] == Head->Blocks.front();
  bool FalseIsHead = LatchBlock->Successors[1] == Head->Blocks.front();
  if (TrueIsHead == FalseIsHead) {
    return InvalidBlockId;
  }
  return TrueIsHead ? LatchBlock->Successors[1] : LatchBlock->Successors[0];
}

bool virtualizeNonFollowLoopExits(const StructuredCFG &Cfg,
                                  const Region &LoopRegion,
                                  const std::set<GraphNodeId> &Members,
                                  BlockId FollowBlock,
                                  MutableRegionGraph &Graph,
                                  StructuredTree &Tree,
                                  RegionOverlay *Overlay) {
  bool Changed = false;
  std::vector<VirtualEdge> Edges;
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
      if (Succ == nullptr || Succ->Blocks.empty() ||
          Succ->Blocks.front() == FollowBlock) {
        continue;
      }
      Edges.push_back({Id, SuccId, Node->TailBlock, Succ->Blocks.front(),
                       VirtualEdgeKind::Goto});
    }
  }

  for (const VirtualEdge &Edge : Edges) {
    Changed |= installVirtualizedEdge(Cfg, LoopRegion, Graph, Tree, Overlay,
                                      Edge);
  }
  return Changed;
}

bool virtualizeTerminalSwitchLoopExits(const StructuredCFG &Cfg,
                                       const Region &LoopRegion,
                                       const std::set<GraphNodeId> &Members,
                                       MutableRegionGraph &Graph,
                                       StructuredTree &Tree,
                                       RegionOverlay *Overlay) {
  std::vector<VirtualEdge> Edges;
  for (GraphNodeId Id : Members) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node == nullptr ||
        !nodeTreeContainsKind(Tree, Node->StructuredRoot,
                              StructuredNodeKind::Switch)) {
      continue;
    }
    for (GraphNodeId SuccId : Node->Succs) {
      if (Members.count(SuccId)) {
        continue;
      }
      const MutableRegionNode *Succ = Graph.getNode(SuccId);
      if (Succ == nullptr || Succ->Blocks.empty() ||
          !isTerminalBlock(Cfg, Succ->Blocks.front())) {
        continue;
      }
      Edges.push_back({Id, SuccId, Node->TailBlock, Succ->Blocks.front(),
                       VirtualEdgeKind::Break});
    }
  }

  bool Changed = false;
  for (const VirtualEdge &Edge : Edges) {
    Changed |= installVirtualizedEdge(Cfg, LoopRegion, Graph, Tree, Overlay,
                                      Edge, /*RewriteSource=*/false);
  }
  return Changed;
}

bool wouldDetachAllPredecessorsOfNonFollowSuccessor(
    const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
    const std::set<GraphNodeId> &Members, BlockId FollowBlock) {
  std::map<GraphNodeId, unsigned> RemovedPredsByTarget;
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
      if (Succ == nullptr || Succ->Blocks.empty() ||
          Succ->Blocks.front() == FollowBlock) {
        continue;
      }
      ++RemovedPredsByTarget[SuccId];
    }
  }

  for (const auto &Entry : RemovedPredsByTarget) {
    const MutableRegionNode *Target = Graph.getNode(Entry.first);
    if (Target != nullptr && !Target->Blocks.empty() &&
        isTerminalBlock(Cfg, Target->Blocks.front())) {
      continue;
    }
    if (Target != nullptr && Target->Preds.size() == Entry.second) {
      return true;
    }
  }
  return false;
}

bool virtualizeExtraContinueEdges(const StructuredCFG &Cfg,
                                  const Region &LoopRegion,
                                  const std::set<GraphNodeId> &Members,
                                  GraphNodeId HeadId,
                                  const MutableRegionGraphAnalysis &Analysis,
                                  MutableRegionGraph &Graph,
                                  StructuredTree &Tree,
                                  RegionOverlay *Overlay) {
  std::vector<GraphNodeId> ContinueSources;
  for (GraphNodeId Id : Members) {
    if (Id != HeadId && Graph.hasEdge(Id, HeadId)) {
      ContinueSources.push_back(Id);
    }
  }
  if (ContinueSources.size() <= 1) {
    return false;
  }

  std::sort(ContinueSources.begin(), ContinueSources.end(),
            [&](GraphNodeId A, GraphNodeId B) {
              unsigned AOrder = Analysis.NodeOrder.count(A) == 0
                                    ? 0
                                    : Analysis.NodeOrder.at(A);
              unsigned BOrder = Analysis.NodeOrder.count(B) == 0
                                    ? 0
                                    : Analysis.NodeOrder.at(B);
              if (AOrder != BOrder) {
                return AOrder < BOrder;
              }
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
  ContinueSources.pop_back();

  bool Changed = false;
  for (GraphNodeId SourceId : ContinueSources) {
    const MutableRegionNode *Source = Graph.getNode(SourceId);
    const MutableRegionNode *Head = Graph.getNode(HeadId);
    if (Source == nullptr || Head == nullptr || Head->Blocks.empty()) {
      continue;
    }
    VirtualEdge Edge{SourceId, HeadId, Source->TailBlock, Head->Blocks.front(),
                     VirtualEdgeKind::Continue};
    Changed |= installVirtualizedEdge(
        Cfg, LoopRegion, Graph, Tree, Overlay, Edge,
        /*RewriteSource=*/!sourceContainsStructuredSwitch(*Source, Tree));
  }
  return Changed;
}

bool makeGraphWhileLoop(const StructuredCFG &Cfg,
                        const MutableRegionGraph &Graph,
                        const std::vector<GraphNodeId> &Members,
                        const std::set<GraphNodeId> &MemberSet,
                        const Region &LoopRegion, GraphNodeId HeadId,
                        StructuredTree &Tree, StructuredNode &LoopNode) {
  const MutableRegionNode *Head = Graph.getNode(HeadId);
  if (Head == nullptr || Head->Blocks.empty() ||
      LoopRegion.Successors.size() != 1) {
    return false;
  }

  const CFGBlock *HeadBlock = Cfg.getBlock(Head->Blocks.back());
  if (HeadBlock == nullptr || HeadBlock->Terminator != TerminatorKind::Branch ||
      !HeadBlock->Statements.empty() || HeadBlock->Successors.size() != 2) {
    return false;
  }
  for (BlockId PrefixBlock : std::vector<BlockId>(Head->Blocks.begin(),
                                                  Head->Blocks.end() - 1)) {
    const CFGBlock *Block = Cfg.getBlock(PrefixBlock);
    if (Block == nullptr || !Block->Statements.empty()) {
      return false;
    }
  }

  GraphNodeId TrueId = Graph.getNodeForBlock(HeadBlock->Successors[0]);
  GraphNodeId FalseId = Graph.getNodeForBlock(HeadBlock->Successors[1]);
  bool TrueInLoop = MemberSet.count(TrueId) != 0;
  bool FalseInLoop = MemberSet.count(FalseId) != 0;
  if (TrueInLoop == FalseInLoop) {
    return false;
  }

  BlockId ExitBlock = TrueInLoop ? HeadBlock->Successors[1]
                                 : HeadBlock->Successors[0];
  if (ExitBlock != LoopRegion.Successors.front()) {
    return false;
  }

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::Sequence;
  static const std::vector<VirtualEdge> EmptyVirtualEdges;
  for (GraphNodeId Id : Members) {
    if (Id == HeadId) {
      continue;
    }
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node != nullptr) {
      appendFallbackNode(Cfg, *Node, EmptyVirtualEdges, LoopRegion, Body,
                         Tree);
    }
  }

  LoopNode.Kind = StructuredNodeKind::While;
  LoopNode.Block = HeadBlock->Id;
  LoopNode.Condition = HeadBlock->Condition;
  LoopNode.ConditionNegated = !TrueInLoop;
  LoopNode.Body = Tree.addNode(std::move(Body));
  return true;
}

bool makeGraphDoWhileLoop(const StructuredCFG &Cfg,
                          const MutableRegionGraph &Graph,
                          const std::vector<GraphNodeId> &Members,
                          const Region &LoopRegion, GraphNodeId HeadId,
                          StructuredTree &Tree, StructuredNode &LoopNode) {
  if (LoopRegion.Latch == InvalidBlockId ||
      LoopRegion.Successors.size() != 1) {
    return false;
  }

  GraphNodeId LatchId = Graph.getNodeForBlock(LoopRegion.Latch);
  const MutableRegionNode *Latch = Graph.getNode(LatchId);
  const MutableRegionNode *Head = Graph.getNode(HeadId);
  if (Latch == nullptr || Head == nullptr || Head->Blocks.empty()) {
    return false;
  }

  const CFGBlock *LatchBlock = Cfg.getBlock(Latch->Blocks.back());
  if (LatchBlock == nullptr ||
      LatchBlock->Terminator != TerminatorKind::Branch ||
      LatchBlock->Successors.size() != 2) {
    return false;
  }

  bool TrueIsHead = LatchBlock->Successors[0] == Head->Blocks.front();
  bool FalseIsHead = LatchBlock->Successors[1] == Head->Blocks.front();
  if (TrueIsHead == FalseIsHead) {
    return false;
  }
  BlockId ExitBlock =
      TrueIsHead ? LatchBlock->Successors[1] : LatchBlock->Successors[0];
  if (ExitBlock != LoopRegion.Successors.front()) {
    return false;
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

  LoopNode.Kind = StructuredNodeKind::DoWhile;
  LoopNode.Block = LatchBlock->Id;
  LoopNode.Condition = LatchBlock->Condition;
  LoopNode.ConditionNegated = !TrueIsHead;
  dropGotoIntoFollowingNode(Body, Tree);
  LoopNode.Body = Tree.addNode(std::move(Body));
  return true;
}

StructuredNode makeGraphInfiniteLoop(const StructuredCFG &Cfg,
                                     const MutableRegionGraph &Graph,
                                     const std::vector<GraphNodeId> &Members,
                                     const Region &LoopRegion,
                                     StructuredTree &Tree) {
  StructuredNode Body;
  Body.Kind = StructuredNodeKind::Sequence;
  static const std::vector<VirtualEdge> EmptyVirtualEdges;
  for (GraphNodeId Id : Members) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    if (Node != nullptr) {
      appendFallbackNode(Cfg, *Node, EmptyVirtualEdges, LoopRegion, Body, Tree);
    }
  }

  StructuredNode LoopNode;
  LoopNode.Kind = StructuredNodeKind::InfiniteLoop;
  LoopNode.Block = LoopRegion.Head;
  LoopNode.Body = Tree.addNode(std::move(Body));
  return LoopNode;
}

bool reduceGraphNaturalLoopOnce(const StructuredCFG &Cfg, const Region &R,
                                MutableRegionGraph &Graph,
                                StructuredTree &Tree, RegionOverlay *Overlay) {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  for (GraphNodeId HeadId : Graph.activeNodes()) {
    const MutableRegionNode *Head = Graph.getNode(HeadId);
    if (Head == nullptr || Head->Blocks.empty()) {
      continue;
    }

    std::set<GraphNodeId> MemberSet;
    BlockId LatchBlock = InvalidBlockId;
    unsigned BackEdgeCount = 0;
    for (GraphNodeId LatchId : Head->Preds) {
      if (!Graph.hasEdge(LatchId, HeadId)) {
        continue;
      }

      std::set<GraphNodeId> BackEdgeMembers;
      if (!collectNaturalLoopMembers(Graph, Analysis, HeadId, LatchId,
                                     BackEdgeMembers)) {
        continue;
      }
      MemberSet.insert(BackEdgeMembers.begin(), BackEdgeMembers.end());
      ++BackEdgeCount;

      const MutableRegionNode *Latch = Graph.getNode(LatchId);
      if (Latch != nullptr && !Latch->Blocks.empty()) {
        LatchBlock = Latch->Blocks.back();
      }
    }
    if (MemberSet.size() <= 1) {
      continue;
    }

    std::vector<BlockId> Successors =
        collectNaturalLoopSuccessors(Graph, MemberSet);
    BlockId WhileFollowBlock =
        findGraphWhileSuccessor(Cfg, Graph, MemberSet, HeadId);
    BlockId DoWhileFollowBlock =
        BackEdgeCount == 1
            ? findGraphDoWhileSuccessor(Cfg, Graph, LatchBlock, HeadId)
            : InvalidBlockId;
    bool PreferDoWhile = DoWhileFollowBlock != InvalidBlockId &&
                         WhileFollowBlock != InvalidBlockId &&
                         DoWhileFollowBlock != WhileFollowBlock;

    BlockId FollowBlock = PreferDoWhile ? DoWhileFollowBlock : WhileFollowBlock;
    if (FollowBlock == InvalidBlockId) {
      FollowBlock = chooseNaturalLoopSuccessor(Graph, MemberSet, Successors);
    }

    Region LoopRegion;
    LoopRegion.Kind = RegionKind::NaturalLoop;
    LoopRegion.Head = Head->Blocks.front();
    LoopRegion.Latch = LatchBlock;
    if (FollowBlock != InvalidBlockId) {
      LoopRegion.Follow = FollowBlock;
      LoopRegion.Successors = {FollowBlock};
    }

    std::vector<GraphNodeId> Members(MemberSet.begin(), MemberSet.end());
    std::sort(Members.begin(), Members.end(), [&](GraphNodeId A,
                                                  GraphNodeId B) {
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

    if (Successors.size() > 1 &&
        wouldDetachAllPredecessorsOfNonFollowSuccessor(Cfg, Graph, MemberSet,
                                                       FollowBlock)) {
      continue;
    }

    rewriteLoopSuccessorExits(Cfg, LoopRegion, MemberSet, FollowBlock, HeadId,
                              Graph, Tree, Overlay);

    if (Successors.size() > 1) {
      virtualizeNonFollowLoopExits(Cfg, LoopRegion, MemberSet, FollowBlock,
                                   Graph, Tree, Overlay);
    }
    virtualizeTerminalSwitchLoopExits(Cfg, LoopRegion, MemberSet, Graph, Tree,
                                      Overlay);
    virtualizeExtraContinueEdges(Cfg, LoopRegion, MemberSet, HeadId, Analysis,
                                 Graph, Tree, Overlay);

    StructuredNode LoopNode;
    bool CanTryDoWhile = PreferDoWhile || Successors.size() <= 1;
    if ((!PreferDoWhile &&
         makeGraphWhileLoop(Cfg, Graph, Members, MemberSet, LoopRegion, HeadId,
                            Tree, LoopNode)) ||
        (CanTryDoWhile &&
         makeGraphDoWhileLoop(Cfg, Graph, Members, LoopRegion, HeadId, Tree,
                              LoopNode))) {
      // LoopNode was filled by one of the schema builders.
    } else {
      LoopNode = makeGraphInfiniteLoop(Cfg, Graph, Members, LoopRegion, Tree);
    }
    collapseNodesAndSyncOverlay(Graph, Overlay, Members, LoopRegion.Head,
                                Tree.addNode(std::move(LoopNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
    return true;
  }
  return false;
}

bool reduceNaturalLoopFallbackOnce(const StructuredCFG &Cfg,
                                   const RegionTree &Regions,
                                   const Region &Parent,
                                   MutableRegionGraph &Graph,
                                   StructuredTree &Tree,
                                   RegionOverlay *Overlay) {
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
    collapseNodesAndSyncOverlay(Graph, Overlay, Members, Loop->Head,
                                Tree.addNode(std::move(LoopNode)),
                                /*SelfLoop=*/true,
                                /*DropRefinementMarks=*/true);
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
                                       StructuredTree &Tree,
                                       RegionOverlay *Overlay) const {
  if (reduceSwitchOnce(Cfg, Graph, Regions, R, Tree, Overlay)) {
    return true;
  }
  if (reduceSequenceOnce(Cfg, Graph, Regions, R, Tree, Overlay)) {
    return true;
  }
  return reduceIfOnce(Cfg, Graph, Regions, R, Tree, Overlay);
}

bool PhoenixStructurer::analyzeCyclic(const StructuredCFG &Cfg,
                                      MutableRegionGraph &Graph,
                                      StructuredTree &Tree,
                                      RegionOverlay *Overlay) const {
  if (reduceLinearWhileOnce(Cfg, Graph, Tree, Overlay)) {
    return true;
  }
  if (useImprovedCyclicSchemas() &&
      reduceLinearWhileWithBreakOnce(Cfg, Graph, Tree, Overlay)) {
    return true;
  }
  if (reduceLinearDoWhileOnce(Cfg, Graph, Tree, Overlay)) {
    return true;
  }
  return reduceSelfLoopOnce(Cfg, Graph, Tree, Overlay);
}

bool PhoenixStructurer::refineCyclic(const StructuredCFG &Cfg,
                                     const RegionTree &Regions, const Region &R,
                                     MutableRegionGraph &Graph,
                                     StructuredTree &Tree,
                                     RegionOverlay *Overlay) const {
  std::size_t Checkpoint = Graph.checkpoint();
  std::size_t OverlayCheckpoint =
      Overlay == nullptr ? 0 : Overlay->manager()->checkpoint();
  if (reduceGraphNaturalLoopOnce(Cfg, R, Graph, Tree, Overlay)) {
    Graph.commit(Checkpoint);
    if (Overlay != nullptr) {
      Overlay->manager()->commit(OverlayCheckpoint);
    }
    return true;
  }
  if (reduceNaturalLoopFallbackOnce(Cfg, Regions, R, Graph, Tree, Overlay)) {
    Graph.commit(Checkpoint);
    if (Overlay != nullptr) {
      Overlay->manager()->commit(OverlayCheckpoint);
    }
    return true;
  }
  Graph.rollback(Checkpoint);
  if (Overlay != nullptr) {
    Overlay->manager()->rollback(OverlayCheckpoint);
    Overlay->manager()->commit(OverlayCheckpoint);
  }
  Graph.commit(Checkpoint);
  return false;
}

bool PhoenixStructurer::lastResortRefinement(const StructuredCFG &Cfg,
                                             const Region &R,
                                             MutableRegionGraph &Graph,
                                             StructuredTree &Tree,
                                             RegionOverlay *Overlay) const {
  if (Graph.activeNodes().size() <= 1) {
    return false;
  }
  std::size_t OverlayCheckpoint =
      Overlay == nullptr ? 0 : Overlay->manager()->checkpoint();
  if (virtualizeOneEdge(Cfg, R, Graph, Tree, Overlay)) {
    if (Overlay != nullptr) {
      syncVirtualEdgesToOverlay(Graph, *Overlay);
      Overlay->manager()->commit(OverlayCheckpoint);
    }
    return true;
  }
  if (Overlay != nullptr) {
    Overlay->manager()->rollback(OverlayCheckpoint);
    Overlay->manager()->commit(OverlayCheckpoint);
  }
  return false;
}

bool PhoenixStructurer::virtualizeOneEdge(const StructuredCFG &Cfg,
                                          const Region &R,
                                          MutableRegionGraph &Graph,
                                          StructuredTree &Tree,
                                          RegionOverlay *Overlay) const {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  applyOverlayNodeOrder(Graph, Overlay, Analysis);
  for (const VirtualEdge &Hint :
       edgeVirtualizationHints(Cfg, Graph, Analysis)) {
    if (Graph.hasEdge(Hint.From, Hint.To)) {
      return installVirtualizedEdge(Cfg, R, Graph, Tree, Overlay, Hint);
    }
  }

  std::vector<VirtualEdge> CandidateEdges =
      Overlay == nullptr ? collectVirtualizableEdges(R, Graph)
                         : collectOverlayVirtualizableEdges(R, Graph, Overlay);
  std::vector<VirtualEdge> Candidates =
      filterByAngrLastResortPriority(
          R, Analysis, CandidateEdges,
          /*IncludeAcyclicDroppedEdges=*/Overlay == nullptr);
  std::vector<VirtualEdge> Edges =
      orderVirtualizableEdges(Cfg, Graph, Analysis, std::move(Candidates));
  if (Edges.empty()) {
    return false;
  }

  const VirtualEdge &Edge = Edges.front();
  return installVirtualizedEdge(Cfg, R, Graph, Tree, Overlay, Edge);
}

StructuredTree PhoenixStructurer::structure(const StructuredCFG &Cfg) {
  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  return RecursiveStructurer().structure(Cfg, Manager, *this);
}

NodeId PhoenixStructurer::structureRegion(const StructuredCFG &Cfg,
                                          const Region &R,
                                          StructuredTree &Tree) {
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, R);

  unsigned Iterations = 0;
  bool Changed = false;
  Changed = preprocessRegionGraph(Cfg, R, Graph);
  do {
    Changed = false;
    bool HasCycle = Graph.hasCycle();
    static const RegionTree EmptyRegions;
    if (analyzeAcyclic(Cfg, EmptyRegions, R, Graph, Tree)) {
      Changed = true;
    }
    if (HasCycle && analyzeCyclic(Cfg, Graph, Tree)) {
      Changed = true;
    }
    if (!Changed && HasCycle &&
        refineCyclic(Cfg, EmptyRegions, R, Graph, Tree)) {
      Changed = true;
    }
    if (!Changed && lastResortRefinement(Cfg, R, Graph, Tree)) {
      Changed = true;
    }
    ++Iterations;
  } while (Changed && Iterations < 1000);

  std::vector<GraphNodeId> FinalActive = Graph.activeNodes();
  if (FinalActive.size() == 1) {
    const MutableRegionNode *Node = Graph.getNode(FinalActive.front());
    if (Node != nullptr && Node->StructuredRoot != InvalidNodeId) {
      NodeId RootId = wrapNaturalLoopFallback(R, Node->StructuredRoot, Tree);
      cleanupStructuredGotos(Cfg, Tree, RootId);
      return RootId;
    }
  }

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
    if (Node != nullptr && !Node->ExternalPlaceholder) {
      auto It = VirtualEdgesBySource.find(Id);
      static const std::vector<VirtualEdge> EmptyVirtualEdges;
      appendFallbackNode(Cfg, *Node,
                         It == VirtualEdgesBySource.end() ? EmptyVirtualEdges
                                                          : It->second,
                         R, Root, Tree);
    }
  }

  dropGotoIntoFollowingNode(Root, Tree);
  NodeId RootId = Tree.addNode(std::move(Root));
  RootId = wrapNaturalLoopFallback(R, RootId, Tree);
  cleanupStructuredGotos(Cfg, Tree, RootId);
  return RootId;
}

NodeId PhoenixStructurer::structureRegion(const StructuredCFG &Cfg,
                                          RegionOverlay &Overlay,
                                          StructuredTree &Tree) {
  const Region *R = Overlay.region();
  if (R == nullptr) {
    return InvalidNodeId;
  }
  RegionTree VisibleRegions = Overlay.manager()->visibleRegionTree();

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Overlay);

  unsigned Iterations = 0;
  bool Changed = false;
  Changed = preprocessRegionGraph(Cfg, *R, Graph);
  do {
    Changed = false;
    bool HasCycle = Graph.hasCycle();
    if (analyzeAcyclic(Cfg, VisibleRegions, *R, Graph, Tree, &Overlay)) {
      Changed = true;
    }
    if (HasCycle && analyzeCyclic(Cfg, Graph, Tree, &Overlay)) {
      Changed = true;
    }
    if (!Changed && HasCycle &&
        refineCyclic(Cfg, VisibleRegions, *R, Graph, Tree, &Overlay)) {
      Changed = true;
    }
    if (!Changed && lastResortRefinement(Cfg, *R, Graph, Tree, &Overlay)) {
      Changed = true;
    }
    ++Iterations;
  } while (Changed && Iterations < 1000);

  std::vector<GraphNodeId> FinalActive = Graph.activeNodes();
  if (FinalActive.size() == 1) {
    const MutableRegionNode *Node = Graph.getNode(FinalActive.front());
    if (Node != nullptr && Node->StructuredRoot != InvalidNodeId) {
      NodeId RootId = wrapNaturalLoopFallback(*R, Node->StructuredRoot, Tree);
      if (!Node->SourceNodes.empty()) {
        syncVirtualEdgesToOverlay(Graph, Overlay);
        if (!isSameStructuredSource(Node->SourceNodes, Overlay.id(), RootId)) {
          Overlay.replaceNodes(Node->SourceNodes, RootId);
        }
      }
      cleanupStructuredGotos(Cfg, Tree, RootId);
      return RootId;
    }
  }

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
    if (Node != nullptr && !Node->ExternalPlaceholder) {
      auto It = VirtualEdgesBySource.find(Id);
      static const std::vector<VirtualEdge> EmptyVirtualEdges;
      appendFallbackNode(Cfg, *Node,
                         It == VirtualEdgesBySource.end() ? EmptyVirtualEdges
                                                          : It->second,
                         *R, Root, Tree);
    }
  }

  dropGotoIntoFollowingNode(Root, Tree);
  NodeId RootId = Tree.addNode(std::move(Root));
  RootId = wrapNaturalLoopFallback(*R, RootId, Tree);
  cleanupStructuredGotos(Cfg, Tree, RootId);
  return RootId;
}

} // namespace notdec::backend::structuring
