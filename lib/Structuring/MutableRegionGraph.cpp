#include "notdec-backends/Structuring/MutableRegionGraph.h"

#include <llvm/ADT/GraphTraits.h>
#include <llvm/Support/GenericDomTree.h>
#include <llvm/Support/GenericDomTreeConstruction.h>
#include <llvm/Support/raw_ostream.h>

#include <algorithm>
#include <functional>
#include <map>
#include <set>

namespace notdec::backend::structuring {
namespace detail {

struct DomOverlayGraph;

// LLVM's generic dominator tree works on pointer nodes with a parent graph.
// Keep this adapter local so MutableRegionGraph can stay ID-based.
struct DomOverlayNode {
  GraphNodeId Id = InvalidGraphNodeId;
  DomOverlayGraph *Parent = nullptr;
  std::vector<DomOverlayNode *> Preds;
  std::vector<DomOverlayNode *> Succs;

  DomOverlayGraph *getParent() const { return Parent; }
  bool isSyntheticRoot() const { return Id == InvalidGraphNodeId; }
  void printAsOperand(llvm::raw_ostream &OS, bool) const {
    if (isSyntheticRoot()) {
      OS << "synthetic-root";
      return;
    }
    OS << "node-" << Id;
  }
};

struct DomOverlayGraph {
  std::vector<DomOverlayNode> Nodes;
  std::vector<DomOverlayNode *> NodePtrs;
  std::map<GraphNodeId, DomOverlayNode *> ById;
  DomOverlayNode *Entry = nullptr;
  DomOverlayNode *Exit = nullptr;
  DomOverlayNode *SyntheticRoot = nullptr;

  size_t size() const { return NodePtrs.size(); }
};

} // namespace detail
} // namespace notdec::backend::structuring

namespace llvm {

template <>
struct GraphTraits<notdec::backend::structuring::detail::DomOverlayNode *> {
  using NodeRef = notdec::backend::structuring::detail::DomOverlayNode *;
  using ChildIteratorType = std::vector<NodeRef>::const_iterator;

  static NodeRef getEntryNode(NodeRef N) { return N; }
  static ChildIteratorType child_begin(NodeRef N) { return N->Succs.begin(); }
  static ChildIteratorType child_end(NodeRef N) { return N->Succs.end(); }
};

template <>
struct GraphTraits<
    Inverse<notdec::backend::structuring::detail::DomOverlayNode *>> {
  using NodeRef = notdec::backend::structuring::detail::DomOverlayNode *;
  using ChildIteratorType = std::vector<NodeRef>::const_iterator;

  static NodeRef getEntryNode(Inverse<NodeRef> G) { return G.Graph; }
  static ChildIteratorType child_begin(NodeRef N) { return N->Preds.begin(); }
  static ChildIteratorType child_end(NodeRef N) { return N->Preds.end(); }
};

template <>
struct GraphTraits<notdec::backend::structuring::detail::DomOverlayGraph *>
    : public GraphTraits<
          notdec::backend::structuring::detail::DomOverlayNode *> {
  using NodeRef = notdec::backend::structuring::detail::DomOverlayNode *;
  using nodes_iterator = std::vector<NodeRef>::const_iterator;

  static NodeRef
  getEntryNode(notdec::backend::structuring::detail::DomOverlayGraph *G) {
    return G->Entry;
  }
  static nodes_iterator
  nodes_begin(notdec::backend::structuring::detail::DomOverlayGraph *G) {
    return G->NodePtrs.begin();
  }
  static nodes_iterator
  nodes_end(notdec::backend::structuring::detail::DomOverlayGraph *G) {
    return G->NodePtrs.end();
  }
  static unsigned
  size(notdec::backend::structuring::detail::DomOverlayGraph *G) {
    return static_cast<unsigned>(G->size());
  }
};

template <>
struct GraphTraits<
    Inverse<notdec::backend::structuring::detail::DomOverlayGraph *>>
    : public GraphTraits<
          Inverse<notdec::backend::structuring::detail::DomOverlayNode *>> {
  using NodeRef = notdec::backend::structuring::detail::DomOverlayNode *;
  using nodes_iterator = std::vector<NodeRef>::const_iterator;

  static NodeRef getEntryNode(
      Inverse<notdec::backend::structuring::detail::DomOverlayGraph *> G) {
    return G.Graph->Exit;
  }
  static nodes_iterator nodes_begin(
      Inverse<notdec::backend::structuring::detail::DomOverlayGraph *> G) {
    return G.Graph->NodePtrs.begin();
  }
  static nodes_iterator nodes_end(
      Inverse<notdec::backend::structuring::detail::DomOverlayGraph *> G) {
    return G.Graph->NodePtrs.end();
  }
};

} // namespace llvm

namespace notdec::backend::structuring {

static bool contains(const std::vector<GraphNodeId> &Values, GraphNodeId Id) {
  return std::find(Values.begin(), Values.end(), Id) != Values.end();
}

static bool contains(const std::set<GraphNodeId> &Values, GraphNodeId Id) {
  return Values.find(Id) != Values.end();
}

static std::set<GraphNodeId> toSet(const std::vector<GraphNodeId> &Values) {
  return {Values.begin(), Values.end()};
}

static void appendUniqueBlock(std::vector<BlockId> &Values, BlockId Id) {
  if (std::find(Values.begin(), Values.end(), Id) == Values.end()) {
    Values.push_back(Id);
  }
}

static bool containsBlock(const std::vector<BlockId> &Values, BlockId Id) {
  return std::find(Values.begin(), Values.end(), Id) != Values.end();
}

static void appendUniqueNode(std::vector<detail::DomOverlayNode *> &Values,
                             detail::DomOverlayNode *Node) {
  if (std::find(Values.begin(), Values.end(), Node) == Values.end()) {
    Values.push_back(Node);
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
                     std::set<GraphNodeId> &InStack,
                     std::vector<GraphNodeId> &Order,
                     std::vector<VirtualEdge> &DroppedEdges) {
  if (!contains(Active, Id) || !Visited.insert(Id).second) {
    return;
  }
  InStack.insert(Id);
  const MutableRegionNode *Node = Graph.getNode(Id);
  std::vector<GraphNodeId> Succs = Node->Succs;
  std::sort(Succs.begin(), Succs.end());
  for (GraphNodeId Succ : Succs) {
    if (!contains(Active, Succ)) {
      continue;
    }
    if (contains(InStack, Succ)) {
      const MutableRegionNode *SuccNode = Graph.getNode(Succ);
      BlockId FromBlock =
          Node->Blocks.empty() ? InvalidBlockId : Node->Blocks.back();
      BlockId ToBlock = (SuccNode == nullptr || SuccNode->Blocks.empty())
                            ? InvalidBlockId
                            : SuccNode->Blocks.front();
      DroppedEdges.push_back(
          {Id, Succ, FromBlock, ToBlock, VirtualEdgeKind::Goto});
      continue;
    }
    dfsOrder(Graph, Active, Succ, Visited, InStack, Order, DroppedEdges);
  }
  InStack.erase(Id);
  Order.push_back(Id);
}

static detail::DomOverlayGraph
buildDomOverlay(const MutableRegionGraph &Graph,
                const std::vector<GraphNodeId> &Ids, GraphNodeId Entry,
                GraphNodeId Exit) {
  detail::DomOverlayGraph Overlay;
  Overlay.Nodes.resize(Ids.size() + 1);
  Overlay.SyntheticRoot = &Overlay.Nodes.front();
  Overlay.SyntheticRoot->Parent = &Overlay;
  Overlay.NodePtrs.push_back(Overlay.SyntheticRoot);

  for (unsigned Index = 0; Index < Ids.size(); ++Index) {
    detail::DomOverlayNode *Node = &Overlay.Nodes[Index + 1];
    Node->Id = Ids[Index];
    Node->Parent = &Overlay;
    Overlay.ById[Node->Id] = Node;
    Overlay.NodePtrs.push_back(Node);
  }

  Overlay.Entry = Overlay.ById[Entry];
  Overlay.Exit = Overlay.ById[Exit];
  if (Overlay.Entry == nullptr) {
    Overlay.Entry = Overlay.SyntheticRoot;
  }
  if (Overlay.Exit == nullptr) {
    Overlay.Exit = Overlay.Entry;
  }

  std::set<GraphNodeId> Active = toSet(Ids);
  for (GraphNodeId Id : Ids) {
    const MutableRegionNode *GraphNode = Graph.getNode(Id);
    detail::DomOverlayNode *From = Overlay.ById[Id];
    if (GraphNode == nullptr || From == nullptr) {
      continue;
    }
    for (GraphNodeId SuccId : GraphNode->Succs) {
      if (!contains(Active, SuccId)) {
        continue;
      }
      detail::DomOverlayNode *To = Overlay.ById[SuccId];
      appendUniqueNode(From->Succs, To);
      appendUniqueNode(To->Preds, From);
    }
  }

  bool HasRoot = false;
  for (GraphNodeId Id : Ids) {
    detail::DomOverlayNode *Node = Overlay.ById[Id];
    if (Node != nullptr && Node->Preds.empty()) {
      appendUniqueNode(Overlay.SyntheticRoot->Succs, Node);
      appendUniqueNode(Node->Preds, Overlay.SyntheticRoot);
      HasRoot = true;
    }
  }
  if (!HasRoot && Overlay.Entry != Overlay.SyntheticRoot) {
    appendUniqueNode(Overlay.SyntheticRoot->Succs, Overlay.Entry);
    appendUniqueNode(Overlay.Entry->Preds, Overlay.SyntheticRoot);
  }
  Overlay.Entry = Overlay.SyntheticRoot;
  return Overlay;
}

using DomOverlayTree =
    llvm::DominatorTreeBase<detail::DomOverlayNode, /*IsPostDom=*/false>;
using PostDomOverlayTree =
    llvm::DominatorTreeBase<detail::DomOverlayNode, /*IsPostDom=*/true>;

template <bool IsPostDom>
static GraphNodeId immediateNodeId(
    const llvm::DominatorTreeBase<detail::DomOverlayNode, IsPostDom> &Tree,
    detail::DomOverlayNode *Node) {
  auto *TreeNode = Tree.getNode(Node);
  if (TreeNode == nullptr || TreeNode->getIDom() == nullptr) {
    return InvalidGraphNodeId;
  }

  detail::DomOverlayNode *IDom = TreeNode->getIDom()->getBlock();
  if (IDom == nullptr || IDom->isSyntheticRoot()) {
    return InvalidGraphNodeId;
  }
  return IDom->Id;
}

static void fillDomAnalysis(MutableRegionGraphAnalysis &Result,
                            detail::DomOverlayGraph &Overlay) {
  DomOverlayTree DomTree;
  PostDomOverlayTree PostDomTree;
  DomTree.recalculate(Overlay);
  PostDomTree.recalculate(Overlay);

  for (detail::DomOverlayNode *Node : Overlay.NodePtrs) {
    if (Node->isSyntheticRoot()) {
      continue;
    }
    Result.Dominators[Node->Id];
    Result.PostDominators[Node->Id];
    Result.ImmediateDominators[Node->Id] = immediateNodeId(DomTree, Node);
    Result.ImmediatePostDominators[Node->Id] =
        immediateNodeId(PostDomTree, Node);
  }

  for (detail::DomOverlayNode *Node : Overlay.NodePtrs) {
    if (Node->isSyntheticRoot()) {
      continue;
    }
    for (detail::DomOverlayNode *Candidate : Overlay.NodePtrs) {
      if (Candidate->isSyntheticRoot()) {
        continue;
      }
      if (DomTree.dominates(Candidate, Node)) {
        Result.Dominators[Node->Id].insert(Candidate->Id);
      }
      if (PostDomTree.dominates(Candidate, Node)) {
        Result.PostDominators[Node->Id].insert(Candidate->Id);
      }
    }
  }
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

GraphNodeId
MutableRegionGraphAnalysis::immediateDominator(GraphNodeId Node) const {
  auto It = ImmediateDominators.find(Node);
  return It == ImmediateDominators.end() ? InvalidGraphNodeId : It->second;
}

GraphNodeId
MutableRegionGraphAnalysis::immediatePostDominator(GraphNodeId Node) const {
  auto It = ImmediatePostDominators.find(Node);
  return It == ImmediatePostDominators.end() ? InvalidGraphNodeId : It->second;
}

bool MutableRegionNode::hasExternalSuccessor(BlockId Target) const {
  return std::find(ExternalSuccs.begin(), ExternalSuccs.end(), Target) !=
         ExternalSuccs.end();
}

GraphNodeId MutableRegionGraph::addNode(BlockId Block, NodeId StructuredRoot,
                                        bool ExternalPlaceholder) {
  MutableRegionNode Node;
  Node.Id = static_cast<GraphNodeId>(Nodes.size());
  Node.Block = Block;
  Node.TailBlock = Block;
  Node.ExternalPlaceholder = ExternalPlaceholder;
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
        if (From != It->second || Succ == Block) {
          Graph.addEdge(From, It->second);
        }
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

MutableRegionGraph MutableRegionGraph::build(const StructuredCFG &Cfg,
                                             const RegionOverlay &Overlay) {
  const Region *R = Overlay.region();
  if (R == nullptr) {
    return {};
  }

  MutableRegionGraph Graph;
  std::map<BlockId, GraphNodeId> BlockToNode;
  std::map<BlockId, GraphNodeId> PlaceholderToNode;
  struct PendingSnapshotSuccessor {
    GraphNodeId From = InvalidGraphNodeId;
    BlockId Succ = InvalidBlockId;
  };
  std::vector<PendingSnapshotSuccessor> PendingSnapshotSuccs;

  auto getOrCreateExternalPlaceholder = [&](BlockId Block) {
    auto It = PlaceholderToNode.find(Block);
    if (It != PlaceholderToNode.end()) {
      return It->second;
    }
    GraphNodeId Id = Graph.addNode(Block, InvalidNodeId, true);
    PlaceholderToNode[Block] = Id;
    return Id;
  };

  for (const OverlayMember &Member :
       Overlay.manager()->members(Overlay.id())) {
    if (Member.Kind == OverlayMemberKind::Block) {
      if (containsBlock(R->Blocks, Member.Block)) {
        BlockToNode[Member.Block] = Graph.addNode(Member.Block);
      }
      continue;
    }

    if (Member.Kind != OverlayMemberKind::Structured) {
      continue;
    }

    const Region *ChildRegion = Overlay.manager()->getRegionData(Member.Region);
    if (ChildRegion == nullptr) {
      continue;
    }

    // Angr's finalize() replaces a reduced child region with its result node
    // in the parent overlay view. The overlay member table now records that
    // replacement; the graph builder turns it into one grouped reducer node.
    GraphNodeId NodeId =
        Graph.addNode(ChildRegion->Head, Member.StructuredRoot);
    MutableRegionNode *Node = Graph.getNode(NodeId);
    if (Node != nullptr) {
      Node->Blocks = ChildRegion->Blocks;
      auto SnapshotIt = Overlay.manager()->finalizedChildren(Overlay.id());
      for (const FinalizedChildRegion &Child : SnapshotIt) {
        if (Child.RegionData == nullptr ||
            Child.RegionData->Id != ChildRegion->Id) {
          continue;
        }
        for (BlockId Succ : Child.Snapshot.Successors) {
          if (containsBlock(ChildRegion->Blocks, Succ)) {
            continue;
          }
          // Angr's finalize() does not reconnect a child continue edge to the
          // enclosing loop head; exposing it here would add a fake parent edge.
          if (R->Kind == RegionKind::NaturalLoop && Succ == R->Head) {
            continue;
          }
          appendUniqueBlock(Node->ExternalSuccs, Succ);
          PendingSnapshotSuccs.push_back({NodeId, Succ});
        }
      }
    }
    for (BlockId Block : ChildRegion->Blocks) {
      if (containsBlock(R->Blocks, Block)) {
        BlockToNode[Block] = NodeId;
      }
    }
  }

  for (BlockId Block : R->Blocks) {
    if (BlockToNode.find(Block) == BlockToNode.end()) {
      BlockToNode[Block] = Graph.addNode(Block);
    }
  }

  for (BlockId Block : R->Blocks) {
    const CFGBlock *CfgBlock = Cfg.getBlock(Block);
    if (CfgBlock == nullptr) {
      continue;
    }

    GraphNodeId From = BlockToNode.at(Block);
    for (BlockId Succ : CfgBlock->Successors) {
      auto It = BlockToNode.find(Succ);
      if (It == BlockToNode.end()) {
        MutableRegionNode *FromNode = Graph.getNode(From);
        if (FromNode != nullptr) {
          appendUniqueBlock(FromNode->ExternalSuccs, Succ);
        }
        Graph.addEdge(From, getOrCreateExternalPlaceholder(Succ));
        continue;
      }
      if (From != It->second || Succ == Block) {
        Graph.addEdge(From, It->second);
      }
    }
  }

  for (const PendingSnapshotSuccessor &Pending : PendingSnapshotSuccs) {
    auto It = BlockToNode.find(Pending.Succ);
    GraphNodeId To = It == BlockToNode.end()
                         ? getOrCreateExternalPlaceholder(Pending.Succ)
                         : It->second;
    if (Pending.From != To) {
      Graph.addEdge(Pending.From, To);
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

bool MutableRegionGraph::hasCycle() const {
  std::set<GraphNodeId> Visited;
  std::set<GraphNodeId> InStack;

  std::function<bool(GraphNodeId)> Visit = [&](GraphNodeId Id) {
    if (!isActive(Id)) {
      return false;
    }
    if (contains(InStack, Id)) {
      return true;
    }
    if (!Visited.insert(Id).second) {
      return false;
    }

    InStack.insert(Id);
    const MutableRegionNode *Node = getNode(Id);
    if (Node != nullptr) {
      for (GraphNodeId Succ : Node->Succs) {
        if (Visit(Succ)) {
          return true;
        }
      }
    }
    InStack.erase(Id);
    return false;
  };

  for (GraphNodeId Id : activeNodes()) {
    if (Visit(Id)) {
      return true;
    }
  }
  return false;
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

std::size_t MutableRegionGraph::checkpoint() {
  Checkpoints.push_back({Nodes, VirtualizedEdges});
  return Checkpoints.size() - 1;
}

void MutableRegionGraph::rollback(std::size_t Checkpoint) {
  if (Checkpoint >= Checkpoints.size()) {
    return;
  }
  Nodes = Checkpoints[Checkpoint].first;
  VirtualizedEdges = Checkpoints[Checkpoint].second;
}

void MutableRegionGraph::commit(std::size_t Checkpoint) {
  if (Checkpoint >= Checkpoints.size()) {
    return;
  }
  Checkpoints.erase(Checkpoints.begin() + Checkpoint, Checkpoints.end());
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

void MutableRegionGraph::setStructuredRoot(GraphNodeId Id,
                                           NodeId StructuredRoot) {
  MutableRegionNode *Node = getNode(Id);
  if (Node != nullptr) {
    Node->StructuredRoot = StructuredRoot;
  }
}

void MutableRegionGraph::virtualizeEdge(GraphNodeId From, GraphNodeId To,
                                        VirtualEdgeKind Kind) {
  if (!hasEdge(From, To)) {
    return;
  }
  const MutableRegionNode *FromNode = getNode(From);
  const MutableRegionNode *ToNode = getNode(To);
  BlockId FromBlock =
      (FromNode == nullptr) ? InvalidBlockId : FromNode->TailBlock;
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
  BlockId TailBlock = InvalidBlockId;
  bool FoundOutgoingTail = false;
  bool FoundBackedgeTail = false;
  for (GraphNodeId Id : MemberSet) {
    const MutableRegionNode *Node = getNode(Id);
    Blocks.insert(Blocks.end(), Node->Blocks.begin(), Node->Blocks.end());
    if (!FoundOutgoingTail && Node->TailBlock != InvalidBlockId) {
      TailBlock = Node->TailBlock;
    }
    for (GraphNodeId Pred : Node->Preds) {
      if (!MemberSet.count(Pred) && isActive(Pred)) {
        Preds.insert(Pred);
      }
    }
    for (GraphNodeId Succ : Node->Succs) {
      if (!FoundBackedgeTail && MemberSet.count(Succ) &&
          !Node->Blocks.empty() && Node->Blocks.front() != RepresentativeBlock &&
          Node->TailBlock != InvalidBlockId) {
        TailBlock = Node->TailBlock;
        FoundBackedgeTail = true;
      }
      if (!MemberSet.count(Succ) && isActive(Succ)) {
        Succs.insert(Succ);
        if (!FoundBackedgeTail && Node->TailBlock != InvalidBlockId) {
          TailBlock = Node->TailBlock;
          FoundOutgoingTail = true;
        }
      }
    }
    for (BlockId ExternalSucc : Node->ExternalSuccs) {
      appendUniqueBlock(ExternalSuccs, ExternalSucc);
      if (!FoundBackedgeTail && Node->TailBlock != InvalidBlockId) {
        TailBlock = Node->TailBlock;
        FoundOutgoingTail = true;
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
  CollapsedNode->TailBlock = TailBlock;
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
  detail::DomOverlayGraph DomOverlay =
      buildDomOverlay(*this, Ids, Result.Entry, Result.Exit);
  fillDomAnalysis(Result, DomOverlay);

  std::set<GraphNodeId> Active = toSet(Ids);
  std::set<GraphNodeId> Visited;
  std::set<GraphNodeId> InStack;
  std::vector<GraphNodeId> PostOrder;
  dfsOrder(*this, Active, Result.Entry, Visited, InStack, PostOrder,
           Result.AcyclicDroppedEdges);
  for (GraphNodeId Id : Ids) {
    dfsOrder(*this, Active, Id, Visited, InStack, PostOrder,
             Result.AcyclicDroppedEdges);
  }
  std::reverse(PostOrder.begin(), PostOrder.end());
  for (unsigned Index = 0; Index < PostOrder.size(); ++Index) {
    Result.NodeOrder[PostOrder[Index]] = Index;
  }
  return Result;
}

} // namespace notdec::backend::structuring
