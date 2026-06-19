#include "notdec-backends/Structuring/SAILRStructurer.h"

#include <algorithm>
#include <utility>

namespace notdec::backend::structuring {
namespace {

unsigned inDegreeOf(const MutableRegionGraph &Graph, GraphNodeId Id) {
  const MutableRegionNode *Node = Graph.getNode(Id);
  return Node == nullptr ? 0U : static_cast<unsigned>(Node->Preds.size());
}

bool isSimpleReturnTarget(const StructuredCFG &Cfg,
                          const MutableRegionNode &Node) {
  if (Node.Blocks.empty()) {
    return false;
  }

  const CFGBlock *Block = Cfg.getBlock(Node.Blocks.back());
  return Block != nullptr && Block->Terminator == TerminatorKind::Return;
}

unsigned postDominatorPairCountAfterRemoving(const MutableRegionGraph &Graph,
                                             const VirtualEdge &Edge) {
  MutableRegionGraph Copy = Graph;
  Copy.removeEdge(Edge.From, Edge.To);

  MutableRegionGraphAnalysis Analysis = Copy.analyze();
  unsigned Count = 0;
  for (const auto &Entry : Analysis.PostDominators) {
    Count += static_cast<unsigned>(Entry.second.size());
  }
  return Count;
}

std::vector<VirtualEdge>
filterByLeastSiblingEdges(const MutableRegionGraph &Graph,
                          const std::vector<VirtualEdge> &Edges) {
  std::vector<VirtualEdge> Best;
  unsigned BestCount = 0;
  bool Found = false;

  for (const VirtualEdge &Edge : Edges) {
    unsigned SiblingCount = inDegreeOf(Graph, Edge.To);
    if (SiblingCount == 0) {
      continue;
    }
    --SiblingCount;
    if (SiblingCount == 0) {
      continue;
    }

    if (!Found || SiblingCount < BestCount) {
      Best.clear();
      BestCount = SiblingCount;
      Found = true;
    }
    if (SiblingCount == BestCount) {
      Best.push_back(Edge);
    }
  }

  return Found ? Best : Edges;
}

std::vector<VirtualEdge>
filterByMostPostDominators(const MutableRegionGraph &Graph,
                           const std::vector<VirtualEdge> &Edges) {
  if (Edges.size() > 10 || Graph.activeNodes().size() > 50) {
    return Edges;
  }

  std::vector<VirtualEdge> Best;
  unsigned BestCount = 0;
  bool Found = false;

  for (const VirtualEdge &Edge : Edges) {
    unsigned Count = postDominatorPairCountAfterRemoving(Graph, Edge);
    if (!Found || Count > BestCount) {
      Best.clear();
      BestCount = Count;
      Found = true;
    }
    if (Count == BestCount) {
      Best.push_back(Edge);
    }
  }

  return Found ? Best : Edges;
}

std::vector<VirtualEdge>
filterByReturnTarget(const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
                     const std::vector<VirtualEdge> &Edges) {
  std::vector<VirtualEdge> Best;
  for (const VirtualEdge &Edge : Edges) {
    const MutableRegionNode *Node = Graph.getNode(Edge.To);
    if (Node != nullptr && isSimpleReturnTarget(Cfg, *Node)) {
      Best.push_back(Edge);
    }
  }
  return Best.empty() ? Edges : Best;
}

} // namespace

std::vector<VirtualEdge> SAILRStructurer::orderVirtualizableEdges(
    const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
    const MutableRegionGraphAnalysis &Analysis,
    std::vector<VirtualEdge> Edges) const {
  if (Edges.size() <= 1) {
    return Edges;
  }

  std::sort(Edges.begin(), Edges.end(),
            [](const VirtualEdge &A, const VirtualEdge &B) {
              return std::make_pair(A.FromBlock, A.ToBlock) <
                     std::make_pair(B.FromBlock, B.ToBlock);
            });

  std::vector<VirtualEdge> Best = filterByLeastSiblingEdges(Graph, Edges);
  if (Best.size() > 1) {
    Best = filterByMostPostDominators(Graph, Best);
  }
  if (Best.size() > 1) {
    Best = filterByReturnTarget(Cfg, Graph, Best);
  }

  return PhoenixStructurer::orderVirtualizableEdges(Cfg, Graph, Analysis,
                                                    std::move(Best));
}

} // namespace notdec::backend::structuring
