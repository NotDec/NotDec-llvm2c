#ifndef NOTDEC_BACKENDS_STRUCTURING_MUTABLEREGIONGRAPH_H
#define NOTDEC_BACKENDS_STRUCTURING_MUTABLEREGIONGRAPH_H

#include "notdec-backends/Structuring/Region.h"

#include <cstdint>
#include <limits>
#include <map>
#include <set>
#include <vector>

namespace notdec::backend::structuring {

using GraphNodeId = std::uint32_t;

constexpr GraphNodeId InvalidGraphNodeId =
    std::numeric_limits<GraphNodeId>::max();

enum class VirtualEdgeKind {
  Goto,
  Break,
  Continue,
};

struct VirtualEdge {
  GraphNodeId From = InvalidGraphNodeId;
  GraphNodeId To = InvalidGraphNodeId;
  BlockId FromBlock = InvalidBlockId;
  BlockId ToBlock = InvalidBlockId;
  VirtualEdgeKind Kind = VirtualEdgeKind::Goto;
};

// Mutable graph used inside structuring algorithms. Nodes can start as basic
// blocks and later become structured subtrees. This keeps destructive reducers
// like Phoenix out of backend-specific CFGs.
struct MutableRegionNode {
  GraphNodeId Id = InvalidGraphNodeId;
  BlockId Block = InvalidBlockId;
  BlockId TailBlock = InvalidBlockId;
  std::vector<BlockId> Blocks;
  NodeId StructuredRoot = InvalidNodeId;
  bool Active = true;
  std::vector<GraphNodeId> Preds;
  std::vector<GraphNodeId> Succs;
  std::vector<BlockId> ExternalSuccs;

  bool hasExternalSuccessor(BlockId Target) const;
};

struct MutableRegionGraphAnalysis {
  GraphNodeId Entry = InvalidGraphNodeId;
  GraphNodeId Exit = InvalidGraphNodeId;
  std::vector<VirtualEdge> AcyclicDroppedEdges;
  std::map<GraphNodeId, std::set<GraphNodeId>> Dominators;
  std::map<GraphNodeId, std::set<GraphNodeId>> PostDominators;
  std::map<GraphNodeId, GraphNodeId> ImmediateDominators;
  std::map<GraphNodeId, GraphNodeId> ImmediatePostDominators;
  std::map<GraphNodeId, unsigned> NodeOrder;

  bool dominates(GraphNodeId Dominator, GraphNodeId Node) const;
  bool postDominates(GraphNodeId Dominator, GraphNodeId Node) const;
  GraphNodeId immediateDominator(GraphNodeId Node) const;
  GraphNodeId immediatePostDominator(GraphNodeId Node) const;
};

class MutableRegionGraph {
public:
  static MutableRegionGraph build(const StructuredCFG &Cfg, const Region &R);
  static MutableRegionGraph
  build(const StructuredCFG &Cfg, const RegionTree &Regions, const Region &R,
        const std::map<RegionId, NodeId> &StructuredChildren);

  const std::vector<MutableRegionNode> &nodes() const { return Nodes; }
  const MutableRegionNode *getNode(GraphNodeId Id) const;
  MutableRegionNode *getNode(GraphNodeId Id);

  std::vector<GraphNodeId> activeNodes() const;
  bool hasEdge(GraphNodeId From, GraphNodeId To) const;
  void addEdge(GraphNodeId From, GraphNodeId To);
  void removeEdge(GraphNodeId From, GraphNodeId To);
  void setStructuredRoot(GraphNodeId Id, NodeId StructuredRoot);
  void virtualizeEdge(GraphNodeId From, GraphNodeId To, VirtualEdgeKind Kind);

  GraphNodeId collapseNodes(const std::vector<GraphNodeId> &Members,
                            BlockId RepresentativeBlock, NodeId StructuredRoot);

  GraphNodeId getNodeForBlock(BlockId Block) const;
  MutableRegionGraphAnalysis analyze() const;
  const std::vector<VirtualEdge> &virtualEdges() const {
    return VirtualizedEdges;
  }

private:
  GraphNodeId addNode(BlockId Block, NodeId StructuredRoot = InvalidNodeId);
  bool isActive(GraphNodeId Id) const;

  std::vector<MutableRegionNode> Nodes;
  std::vector<VirtualEdge> VirtualizedEdges;
};

} // namespace notdec::backend::structuring

#endif
