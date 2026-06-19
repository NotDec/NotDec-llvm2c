#include "notdec-backends/Structuring/MutableRegionGraph.h"

#include <algorithm>
#include <map>
#include <set>

namespace notdec::backend::structuring {

static bool contains(const std::vector<GraphNodeId> &Values, GraphNodeId Id) {
  return std::find(Values.begin(), Values.end(), Id) != Values.end();
}

GraphNodeId MutableRegionGraph::addNode(BlockId Block, NodeId StructuredRoot) {
  MutableRegionNode Node;
  Node.Id = static_cast<GraphNodeId>(Nodes.size());
  Node.Block = Block;
  if (Block != InvalidBlockId) {
    Node.Blocks.push_back(Block);
  }
  Node.StructuredRoot = StructuredRoot;
  Nodes.push_back(std::move(Node));
  return Nodes.back().Id;
}

MutableRegionGraph MutableRegionGraph::build(const StructuredCFG &Cfg,
                                             const Region &R) {
  MutableRegionGraph Graph;
  std::map<BlockId, GraphNodeId> BlockToNode;

  for (BlockId Block : R.Blocks) {
    BlockToNode[Block] = Graph.addNode(Block);
  }

  for (BlockId Block : R.Blocks) {
    const CFGBlock *CfgBlock = Cfg.getBlock(Block);
    if (CfgBlock == nullptr) {
      continue;
    }

    GraphNodeId From = BlockToNode.at(Block);
    for (BlockId Succ : CfgBlock->Successors) {
      auto It = BlockToNode.find(Succ);
      if (It != BlockToNode.end()) {
        Graph.addEdge(From, It->second);
      }
    }
  }

  return Graph;
}

const MutableRegionNode *MutableRegionGraph::getNode(GraphNodeId Id) const {
  if (Id >= Nodes.size()) {
    return nullptr;
  }
  return &Nodes[Id];
}

MutableRegionNode *MutableRegionGraph::getNode(GraphNodeId Id) {
  if (Id >= Nodes.size()) {
    return nullptr;
  }
  return &Nodes[Id];
}

std::vector<GraphNodeId> MutableRegionGraph::activeNodes() const {
  std::vector<GraphNodeId> Result;
  for (const MutableRegionNode &Node : Nodes) {
    if (Node.Active) {
      Result.push_back(Node.Id);
    }
  }
  return Result;
}

bool MutableRegionGraph::isActive(GraphNodeId Id) const {
  const MutableRegionNode *Node = getNode(Id);
  return Node != nullptr && Node->Active;
}

bool MutableRegionGraph::hasEdge(GraphNodeId From, GraphNodeId To) const {
  const MutableRegionNode *FromNode = getNode(From);
  if (FromNode == nullptr || !FromNode->Active) {
    return false;
  }
  return contains(FromNode->Succs, To);
}

void MutableRegionGraph::addEdge(GraphNodeId From, GraphNodeId To) {
  MutableRegionNode *FromNode = getNode(From);
  MutableRegionNode *ToNode = getNode(To);
  if (FromNode == nullptr || ToNode == nullptr || !FromNode->Active ||
      !ToNode->Active || From == To) {
    return;
  }

  if (!contains(FromNode->Succs, To)) {
    FromNode->Succs.push_back(To);
  }
  if (!contains(ToNode->Preds, From)) {
    ToNode->Preds.push_back(From);
  }
}

void MutableRegionGraph::removeEdge(GraphNodeId From, GraphNodeId To) {
  MutableRegionNode *FromNode = getNode(From);
  MutableRegionNode *ToNode = getNode(To);
  if (FromNode == nullptr || ToNode == nullptr) {
    return;
  }

  FromNode->Succs.erase(
      std::remove(FromNode->Succs.begin(), FromNode->Succs.end(), To),
      FromNode->Succs.end());
  ToNode->Preds.erase(
      std::remove(ToNode->Preds.begin(), ToNode->Preds.end(), From),
      ToNode->Preds.end());
}

void MutableRegionGraph::virtualizeEdge(GraphNodeId From, GraphNodeId To,
                                        VirtualEdgeKind Kind) {
  if (!hasEdge(From, To)) {
    return;
  }
  const MutableRegionNode *FromNode = getNode(From);
  const MutableRegionNode *ToNode = getNode(To);
  BlockId FromBlock = (FromNode == nullptr || FromNode->Blocks.empty())
                          ? InvalidBlockId
                          : FromNode->Blocks.back();
  BlockId ToBlock = (ToNode == nullptr || ToNode->Blocks.empty())
                        ? InvalidBlockId
                        : ToNode->Blocks.front();
  removeEdge(From, To);
  VirtualizedEdges.push_back({From, To, FromBlock, ToBlock, Kind});
}

GraphNodeId
MutableRegionGraph::collapseNodes(const std::vector<GraphNodeId> &Members,
                                  BlockId RepresentativeBlock,
                                  NodeId StructuredRoot) {
  std::set<GraphNodeId> MemberSet;
  for (GraphNodeId Id : Members) {
    if (isActive(Id)) {
      MemberSet.insert(Id);
    }
  }
  if (MemberSet.empty()) {
    return InvalidGraphNodeId;
  }

  std::set<GraphNodeId> Preds;
  std::set<GraphNodeId> Succs;
  std::vector<BlockId> Blocks;
  for (GraphNodeId Id : MemberSet) {
    const MutableRegionNode *Node = getNode(Id);
    Blocks.insert(Blocks.end(), Node->Blocks.begin(), Node->Blocks.end());
    for (GraphNodeId Pred : Node->Preds) {
      if (!MemberSet.count(Pred) && isActive(Pred)) {
        Preds.insert(Pred);
      }
    }
    for (GraphNodeId Succ : Node->Succs) {
      if (!MemberSet.count(Succ) && isActive(Succ)) {
        Succs.insert(Succ);
      }
    }
  }

  for (GraphNodeId Id : MemberSet) {
    MutableRegionNode *Node = getNode(Id);
    Node->Active = false;
    Node->Preds.clear();
    Node->Succs.clear();
  }

  GraphNodeId Collapsed = addNode(RepresentativeBlock, StructuredRoot);
  MutableRegionNode *CollapsedNode = getNode(Collapsed);
  CollapsedNode->Blocks = std::move(Blocks);
  for (GraphNodeId Pred : Preds) {
    MutableRegionNode *PredNode = getNode(Pred);
    PredNode->Succs.erase(
        std::remove_if(PredNode->Succs.begin(), PredNode->Succs.end(),
                       [&](GraphNodeId Succ) { return MemberSet.count(Succ); }),
        PredNode->Succs.end());
    addEdge(Pred, Collapsed);
  }
  for (GraphNodeId Succ : Succs) {
    MutableRegionNode *SuccNode = getNode(Succ);
    SuccNode->Preds.erase(
        std::remove_if(SuccNode->Preds.begin(), SuccNode->Preds.end(),
                       [&](GraphNodeId Pred) { return MemberSet.count(Pred); }),
        SuccNode->Preds.end());
    addEdge(Collapsed, Succ);
  }

  return Collapsed;
}

GraphNodeId MutableRegionGraph::getNodeForBlock(BlockId Block) const {
  for (const MutableRegionNode &Node : Nodes) {
    if (Node.Active && contains(Node.Blocks, Block)) {
      return Node.Id;
    }
  }
  return InvalidGraphNodeId;
}

} // namespace notdec::backend::structuring
