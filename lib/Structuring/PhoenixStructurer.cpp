#include "notdec-backends/Structuring/PhoenixStructurer.h"

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <map>
#include <set>

namespace notdec::backend::structuring {

namespace {

StructuredNode makeGoto(BlockId Target) {
  StructuredNode Node;
  Node.Kind = StructuredNodeKind::Goto;
  Node.Target = Target;
  return Node;
}

StructuredNode makeBreak() {
  StructuredNode Node;
  Node.Kind = StructuredNodeKind::Break;
  return Node;
}

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

bool reduceSequenceOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
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

    std::vector<BlockId> Blocks = From->Blocks;
    Blocks.insert(Blocks.end(), To->Blocks.begin(), To->Blocks.end());
    NodeId Root = buildSequenceNode(Cfg, {From, To}, Tree);
    Graph.collapseNodes({FromId, ToId}, From->Block, Root);
    return true;
  }
  return false;
}

bool reduceIfOnce(const StructuredCFG &Cfg, MutableRegionGraph &Graph,
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
    if (ElseId != InvalidGraphNodeId) {
      Graph.collapseNodes({HeaderId, ThenId, ElseId}, Header->Block, Root);
    } else {
      Graph.collapseNodes({HeaderId, ThenId}, Header->Block, Root);
    }
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
    if (!Valid || FollowId == InvalidGraphNodeId) {
      std::set<GraphNodeId> UniqueTargets(Targets.begin(), Targets.end());
      if (Valid && UniqueTargets.size() == 1) {
        FollowId = *UniqueTargets.begin();
      } else {
        continue;
      }
    }

    std::set<GraphNodeId> Members;
    Members.insert(HeaderId);
    for (GraphNodeId TargetId : Targets) {
      if (TargetId == FollowId) {
        continue;
      }
      const MutableRegionNode *Target = Graph.getNode(TargetId);
      if (Target == nullptr || Target->Preds.size() != 1 ||
          Target->Preds[0] != HeaderId || Target->Succs.size() != 1 ||
          Target->Succs[0] != FollowId) {
        Valid = false;
        break;
      }
      Members.insert(TargetId);
    }
    if (!Valid) {
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

    std::vector<GraphNodeId> MemberList(Members.begin(), Members.end());
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

bool collectLinearLoopBodyWithBreak(const StructuredCFG &Cfg,
                                    const MutableRegionGraph &Graph,
                                    GraphNodeId HeaderId,
                                    GraphNodeId BodyEntryId,
                                    GraphNodeId FollowId,
                                    std::vector<GraphNodeId> &BodyIds,
                                    LoopBreakSite &BreakSite) {
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

    if (BodyEntryId == InvalidGraphNodeId || Graph.getNode(FollowId) == nullptr) {
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
      if (Tail->Successors[0] == Header->Blocks.front()) {
        LoopNode.Kind = StructuredNodeKind::DoWhile;
        LoopNode.Condition = Tail->Condition;
      } else if (Tail->Successors[1] == Header->Blocks.front()) {
        LoopNode.Kind = StructuredNodeKind::DoWhile;
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
collectVirtualizableEdges(const MutableRegionGraph &Graph) {
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
      Edges.push_back(
          {Node.Id, Succ, FromBlock, ToBlock, VirtualEdgeKind::Goto});
    }
  }
  return Edges;
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
  Root.Children.push_back(Tree.addNode(std::move(Node)));
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
                        StructuredNode &Root, StructuredTree &Tree) {
  if (Node.StructuredRoot != InvalidNodeId) {
    Root.Children.push_back(Node.StructuredRoot);
    if (Node.Blocks.empty()) {
      return;
    }
    const CFGBlock *Tail = Cfg.getBlock(Node.Blocks.back());
    if (Tail == nullptr) {
      return;
    }
    if (nodeTreeContainsStructuredControl(Tree, Node.StructuredRoot)) {
      for (const VirtualEdge &Edge : VirtualEdges) {
        if (!nodeHasSuccessorTarget(Cfg, Node, Edge.ToBlock)) {
          appendControlTransfer(Root, Tree, Edge.ToBlock, Edge.Kind);
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
  const CFGBlock *Tail = Cfg.getBlock(Node.Blocks.back());
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
      IfNode.Then = Tree.addNode(makeGoto(Tail->Successors[0]));
      IfNode.Children.push_back(IfNode.Then);
    }
    if (Tail->Successors.size() > 1) {
      IfNode.Else = Tree.addNode(makeGoto(Tail->Successors[1]));
      IfNode.Children.push_back(IfNode.Else);
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
      SwitchNode.Default = Tree.addNode(makeGoto(Tail->Successors[0]));
    }
    for (const auto &Case : Tail->Cases) {
      NodeId CaseBody = Tree.addNode(makeGoto(Case.Target));
      SwitchNode.StructuredCases.push_back({Case.Value, Case.Target, CaseBody});
      SwitchNode.Children.push_back(CaseBody);
    }
    for (BlockId Succ : Tail->Successors) {
      SwitchNode.Children.push_back(Tree.addNode(makeGoto(Succ)));
    }
    Root.Children.push_back(Tree.addNode(std::move(SwitchNode)));
    break;
  }
  case TerminatorKind::Fallthrough:
    for (BlockId Succ : Tail->Successors) {
      Root.Children.push_back(Tree.addNode(makeGoto(Succ)));
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
    if (!hasSuccessorTarget(*Tail, Edge.ToBlock)) {
      appendControlTransfer(Root, Tree, Edge.ToBlock, Edge.Kind);
    }
  }
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

bool PhoenixStructurer::virtualizeOneEdge(const StructuredCFG &Cfg,
                                          MutableRegionGraph &Graph) const {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  std::vector<VirtualEdge> Edges = orderVirtualizableEdges(
      Cfg, Graph, Analysis, collectVirtualizableEdges(Graph));
  if (Edges.empty()) {
    return false;
  }

  const VirtualEdge &Edge = Edges.front();
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
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, R);

  unsigned Iterations = 0;
  bool Changed = false;
  do {
    Changed = false;
    Changed = reduceLinearWhileOnce(Cfg, Graph, Tree);
    if (!Changed) {
      Changed = reduceLinearWhileWithBreakOnce(Cfg, Graph, Tree);
    }
    if (!Changed) {
      Changed = reduceSelfLoopOnce(Cfg, Graph, Tree);
    }
    if (!Changed) {
      Changed = reduceSequenceOnce(Cfg, Graph, Tree);
    }
    if (!Changed) {
      Changed = reduceIfOnce(Cfg, Graph, Tree);
    }
    if (!Changed) {
      Changed = reduceSwitchOnce(Cfg, Graph, Tree);
    }
    if (!Changed && Graph.activeNodes().size() > 1) {
      Changed = virtualizeOneEdge(Cfg, Graph);
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
                         It == VirtualEdgesBySource.end()
                             ? EmptyVirtualEdges
                             : It->second,
                         Root, Tree);
    }
  }

  return Tree.addNode(std::move(Root));
}

} // namespace notdec::backend::structuring
