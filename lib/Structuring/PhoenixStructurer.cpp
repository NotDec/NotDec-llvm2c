#include "notdec-backends/Structuring/PhoenixStructurer.h"

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <map>

namespace notdec::backend::structuring {

namespace {

StructuredNode makeGoto(BlockId Target) {
  StructuredNode Node;
  Node.Kind = StructuredNodeKind::Goto;
  Node.Target = Target;
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
    NodeId Root = buildLinearNode(Cfg, Blocks, Tree);
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
    } else {
      continue;
    }

    const MutableRegionNode *Then = Graph.getNode(ThenId);
    const MutableRegionNode *Else =
        ElseId == InvalidGraphNodeId ? nullptr : Graph.getNode(ElseId);
    if (Then == nullptr || Graph.getNode(FollowId) == nullptr ||
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

void appendFallbackNode(const StructuredCFG &Cfg, const MutableRegionNode &Node,
                        const std::vector<VirtualEdge> &VirtualEdges,
                        StructuredNode &Root, StructuredTree &Tree) {
  if (Node.StructuredRoot != InvalidNodeId) {
    Root.Children.push_back(Node.StructuredRoot);
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
    const MutableRegionGraph &Graph, const MutableRegionGraphAnalysis &Analysis,
    std::vector<VirtualEdge> Edges) const {
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

bool PhoenixStructurer::virtualizeOneEdge(MutableRegionGraph &Graph) const {
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  std::vector<VirtualEdge> Edges =
      orderVirtualizableEdges(Graph, Analysis, collectVirtualizableEdges(Graph));
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
    Changed |= reduceSequenceOnce(Cfg, Graph, Tree);
    Changed |= reduceIfOnce(Cfg, Graph, Tree);
    if (!Changed && Graph.activeNodes().size() > 1) {
      Changed = virtualizeOneEdge(Graph);
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
