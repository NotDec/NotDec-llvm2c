#include "notdec-backends/Structuring/PhoenixStructurer.h"

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>

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
  if (From == nullptr || To == nullptr || To->Blocks.empty()) {
    return false;
  }

  const CFGBlock *Block = Cfg.getBlock(From->Block);
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

void appendFallbackNode(const StructuredCFG &Cfg, const MutableRegionNode &Node,
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
    for (BlockId Succ : Tail->Successors) {
      IfNode.Children.push_back(Tree.addNode(makeGoto(Succ)));
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
    for (const auto &Case : Tail->Cases) {
      SwitchNode.Children.push_back(Tree.addNode(makeGoto(Case.Target)));
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
}

} // namespace

StructuredTree PhoenixStructurer::structure(const StructuredCFG &Cfg) {
  RegionTree Regions = RegionIdentifier::identifyRoot(Cfg);
  return RecursiveStructurer().structure(Cfg, Regions, *this);
}

NodeId PhoenixStructurer::structureRegion(const StructuredCFG &Cfg,
                                          const Region &R,
                                          StructuredTree &Tree) {
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, R);

  while (reduceSequenceOnce(Cfg, Graph, Tree)) {
  }

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

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
      appendFallbackNode(Cfg, *Node, Root, Tree);
    }
  }

  return Tree.addNode(std::move(Root));
}

} // namespace notdec::backend::structuring
