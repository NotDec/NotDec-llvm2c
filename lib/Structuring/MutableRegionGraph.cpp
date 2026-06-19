#include "notdec-backends/Structuring/MutableRegionGraph.h"

#include <algorithm>
#include <map>
#include <set>

namespace notdec::backend::structuring {

static bool contains(const std::vector<GraphNodeId> &Values, GraphNodeId Id) {
  return std::find(Values.begin(), Values.end(), Id) != Values.end();
}

static bool contains(const std::set<GraphNodeId> &Values, GraphNodeId Id) {
  return Values.find(Id) != Values.end();
}

static std::set<GraphNodeId> intersectSets(const std::set<GraphNodeId> &A,
                                           const std::set<GraphNodeId> &B) {
  std::set<GraphNodeId> Result;
  std::set_intersection(A.begin(), A.end(), B.begin(), B.end(),
                        std::inserter(Result, Result.begin()));
  return Result;
}

static std::set<GraphNodeId> toSet(const std::vector<GraphNodeId> &Values) {
  return {Values.begin(), Values.end()};
}

static void appendUniqueBlock(std::vector<BlockId> &Values, BlockId Id) {
  if (std::find(Values.begin(), Values.end(), Id) == Values.end()) {
    Values.push_back(Id);
  }
}

static GraphNodeId firstWithoutActivePred(const MutableRegionGraph &Graph,
                                          const std::vector<GraphNodeId> &Ids) {
  for (GraphNodeId Id : Ids) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    bool HasActivePred = false;
    for (GraphNodeId Pred : Node->Preds) {
      if (contains(Ids, Pred)) {
        HasActivePred = true;
        break;
      }
    }
    if (!HasActivePred) {
      return Id;
    }
  }
  return Ids.empty() ? InvalidGraphNodeId : Ids.front();
}

static GraphNodeId firstWithoutActiveSucc(const MutableRegionGraph &Graph,
                                          const std::vector<GraphNodeId> &Ids) {
  for (GraphNodeId Id : Ids) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    bool HasActiveSucc = false;
    for (GraphNodeId Succ : Node->Succs) {
      if (contains(Ids, Succ)) {
        HasActiveSucc = true;
        break;
      }
    }
    if (!HasActiveSucc) {
      return Id;
    }
  }
  return Ids.empty() ? InvalidGraphNodeId : Ids.back();
}

static void dfsOrder(const MutableRegionGraph &Graph,
                     const std::set<GraphNodeId> &Active, GraphNodeId Id,
                     std::set<GraphNodeId> &Visited,
                     std::vector<GraphNodeId> &Order) {
  if (!contains(Active, Id) || !Visited.insert(Id).second) {
    return;
  }
  const MutableRegionNode *Node = Graph.getNode(Id);
  std::vector<GraphNodeId> Succs = Node->Succs;
  std::sort(Succs.begin(), Succs.end());
  for (GraphNodeId Succ : Succs) {
    dfsOrder(Graph, Active, Succ, Visited, Order);
  }
  Order.push_back(Id);
}

static std::map<GraphNodeId, std::set<GraphNodeId>>
computeDominators(const MutableRegionGraph &Graph,
                  const std::vector<GraphNodeId> &Ids, GraphNodeId Entry) {
  std::set<GraphNodeId> All = toSet(Ids);
  std::map<GraphNodeId, std::set<GraphNodeId>> Dom;
  for (GraphNodeId Id : Ids) {
    Dom[Id] = Id == Entry ? std::set<GraphNodeId>{Id} : All;
  }

  bool Changed = false;
  do {
    Changed = false;
    for (GraphNodeId Id : Ids) {
      if (Id == Entry) {
        continue;
      }
      const MutableRegionNode *Node = Graph.getNode(Id);
      std::set<GraphNodeId> NewDom = All;
      bool HasPred = false;
      for (GraphNodeId Pred : Node->Preds) {
        if (!contains(All, Pred)) {
          continue;
        }
        NewDom = HasPred ? intersectSets(NewDom, Dom[Pred]) : Dom[Pred];
        HasPred = true;
      }
      if (!HasPred) {
        NewDom.clear();
      }
      NewDom.insert(Id);
      if (NewDom != Dom[Id]) {
        Dom[Id] = std::move(NewDom);
        Changed = true;
      }
    }
  } while (Changed);
  return Dom;
}

static std::map<GraphNodeId, std::set<GraphNodeId>>
computePostDominators(const MutableRegionGraph &Graph,
                      const std::vector<GraphNodeId> &Ids, GraphNodeId Exit) {
  std::set<GraphNodeId> All = toSet(Ids);
  std::map<GraphNodeId, std::set<GraphNodeId>> PostDom;
  for (GraphNodeId Id : Ids) {
    PostDom[Id] = Id == Exit ? std::set<GraphNodeId>{Id} : All;
  }

  bool Changed = false;
  do {
    Changed = false;
    for (GraphNodeId Id : Ids) {
      if (Id == Exit) {
        continue;
      }
      const MutableRegionNode *Node = Graph.getNode(Id);
      std::set<GraphNodeId> NewPostDom = All;
      bool HasSucc = false;
      for (GraphNodeId Succ : Node->Succs) {
        if (!contains(All, Succ)) {
          continue;
        }
        NewPostDom =
            HasSucc ? intersectSets(NewPostDom, PostDom[Succ]) : PostDom[Succ];
        HasSucc = true;
      }
      if (!HasSucc) {
        NewPostDom.clear();
      }
      NewPostDom.insert(Id);
      if (NewPostDom != PostDom[Id]) {
        PostDom[Id] = std::move(NewPostDom);
        Changed = true;
      }
    }
  } while (Changed);
  return PostDom;
}

bool MutableRegionGraphAnalysis::dominates(GraphNodeId Dominator,
                                           GraphNodeId Node) const {
  auto It = Dominators.find(Node);
  return It != Dominators.end() && contains(It->second, Dominator);
}

bool MutableRegionGraphAnalysis::postDominates(GraphNodeId Dominator,
                                               GraphNodeId Node) const {
  auto It = PostDominators.find(Node);
  return It != PostDominators.end() && contains(It->second, Dominator);
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
      } else {
        MutableRegionNode *FromNode = Graph.getNode(From);
        if (FromNode != nullptr) {
          appendUniqueBlock(FromNode->ExternalSuccs, Succ);
        }
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
      !ToNode->Active) {
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
  std::vector<BlockId> ExternalSuccs;
  std::vector<BlockId> Blocks;
  for (GraphNodeId Id : MemberSet) {
    const MutableRegionNode *Node = getNode(Id);
    Blocks.insert(Blocks.end(), Node->Blocks.begin(), Node->Blocks.end());
    for (BlockId ExternalSucc : Node->ExternalSuccs) {
      appendUniqueBlock(ExternalSuccs, ExternalSucc);
    }
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
    Node->ExternalSuccs.clear();
  }

  GraphNodeId Collapsed = addNode(RepresentativeBlock, StructuredRoot);
  MutableRegionNode *CollapsedNode = getNode(Collapsed);
  CollapsedNode->Blocks = std::move(Blocks);
  CollapsedNode->ExternalSuccs = std::move(ExternalSuccs);
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

MutableRegionGraphAnalysis MutableRegionGraph::analyze() const {
  MutableRegionGraphAnalysis Result;
  std::vector<GraphNodeId> Ids = activeNodes();
  std::sort(Ids.begin(), Ids.end());
  if (Ids.empty()) {
    return Result;
  }

  Result.Entry = firstWithoutActivePred(*this, Ids);
  Result.Exit = firstWithoutActiveSucc(*this, Ids);
  Result.Dominators = computeDominators(*this, Ids, Result.Entry);
  Result.PostDominators = computePostDominators(*this, Ids, Result.Exit);

  std::set<GraphNodeId> Active = toSet(Ids);
  std::set<GraphNodeId> Visited;
  std::vector<GraphNodeId> PostOrder;
  dfsOrder(*this, Active, Result.Entry, Visited, PostOrder);
  for (GraphNodeId Id : Ids) {
    dfsOrder(*this, Active, Id, Visited, PostOrder);
  }
  std::reverse(PostOrder.begin(), PostOrder.end());
  for (unsigned Index = 0; Index < PostOrder.size(); ++Index) {
    Result.NodeOrder[PostOrder[Index]] = Index;
  }
  return Result;
}

} // namespace notdec::backend::structuring
