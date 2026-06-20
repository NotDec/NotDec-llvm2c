#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"

#include <cassert>
#include <memory>

using namespace notdec::backend::structuring;

namespace {

class HintStructurer : public PhoenixStructurer {
public:
  using PhoenixStructurer::virtualizeOneEdge;

  HintStructurer(GraphNodeId From, GraphNodeId To) : From(From), To(To) {}

protected:
  std::vector<VirtualEdge> edgeVirtualizationHints(
      const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
      const MutableRegionGraphAnalysis &Analysis) const override {
    (void)Cfg;
    (void)Analysis;
    const MutableRegionNode *FromNode = Graph.getNode(From);
    const MutableRegionNode *ToNode = Graph.getNode(To);
    assert(FromNode != nullptr);
    assert(ToNode != nullptr);
    return {{From, To, FromNode->TailBlock, ToNode->Blocks.front(),
             VirtualEdgeKind::Goto}};
  }

private:
  GraphNodeId From;
  GraphNodeId To;
};

class TestSAILRStructurer : public SAILRStructurer {
public:
  using SAILRStructurer::SAILRStructurer;
  using SAILRStructurer::orderVirtualizableEdges;
};

CFGBlock block(BlockId Id, std::vector<BlockId> Successors) {
  CFGBlock Block;
  Block.Id = Id;
  Block.Successors = std::move(Successors);
  Block.Terminator = Block.Successors.empty() ? TerminatorKind::Return
                                              : TerminatorKind::Fallthrough;
  return Block;
}

CFGBlock branchBlock(BlockId Id, std::vector<BlockId> Successors) {
  CFGBlock Block = block(Id, std::move(Successors));
  Block.Terminator = TerminatorKind::Branch;
  return Block;
}

CFGBlock switchBlock(BlockId Id, std::vector<BlockId> Successors) {
  CFGBlock Block = block(Id, std::move(Successors));
  Block.Terminator = TerminatorKind::Switch;
  if (Block.Successors.size() > 1) {
    for (BlockId Target : std::vector<BlockId>(Block.Successors.begin() + 1,
                                               Block.Successors.end())) {
      Block.Cases.push_back({{}, Target});
    }
  }
  return Block;
}

void testAcyclicDroppedEdges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2, 3}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {1, 5}));
  Cfg.addBlock(block(5, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();

  assert(Analysis.AcyclicDroppedEdges.size() == 1);
  const VirtualEdge &Edge = Analysis.AcyclicDroppedEdges.front();
  assert(Edge.FromBlock == 4);
  assert(Edge.ToBlock == 1);
}

void testEdgeVirtualizationHints() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  HintStructurer Structurer(0, 2);

  assert(Structurer.virtualizeOneEdge(Cfg, Root, Graph, Tree));
  assert(Graph.virtualEdges().size() == 1);
  const VirtualEdge &Edge = Graph.virtualEdges().front();
  assert(Edge.FromBlock == 0);
  assert(Edge.ToBlock == 2);
  const MutableRegionNode *Source = Graph.getNode(0);
  assert(Source != nullptr);
  assert(Source->StructuredRoot != InvalidNodeId);
}

void testSwitchVirtualizationInstallsSourceRoot() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2, 3}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  HintStructurer Structurer(0, 2);

  assert(Structurer.virtualizeOneEdge(Cfg, Root, Graph, Tree));
  const MutableRegionNode *Source = Graph.getNode(0);
  assert(Source != nullptr);
  assert(Source->StructuredRoot != InvalidNodeId);
}

void testFallthroughVirtualizationInstallsSourceRoot() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  HintStructurer Structurer(0, 1);

  assert(Structurer.virtualizeOneEdge(Cfg, Root, Graph, Tree));
  const MutableRegionNode *Source = Graph.getNode(0);
  assert(Source != nullptr);
  assert(Source->StructuredRoot != InvalidNodeId);
}

void testStructurerRegistryNames() {
  llvm::ArrayRef<std::string_view> Names = registeredStructurerNames();
  assert(Names.size() == 3);
  assert(Names[0] == "goto");
  assert(Names[1] == "phoenix");
  assert(Names[2] == "sailr");

  std::unique_ptr<Structurer> Goto = createStructurer("GOTO");
  std::unique_ptr<Structurer> Phoenix = createStructurer("Phoenix");
  std::unique_ptr<Structurer> Sailr = createStructurer("sailr");
  std::unique_ptr<Structurer> Missing = createStructurer("dream");
  assert(Goto != nullptr);
  assert(Phoenix != nullptr);
  assert(Sailr != nullptr);
  assert(Missing == nullptr);
}

void testMergedNaturalLoopKeepsAllLatchPaths() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 5}));
  Cfg.addBlock(branchBlock(1, {2, 3}));
  Cfg.addBlock(block(2, {0}));
  Cfg.addBlock(branchBlock(3, {5, 4}));
  Cfg.addBlock(block(4, {0}));
  Cfg.addBlock(block(5, {}));

  RegionTree Regions = RegionIdentifier::identifyRoot(Cfg);
  const Region *Root = Regions.getRegion(Regions.root());
  assert(Root != nullptr);
  assert(Root->Children.size() == 1);

  const Region *Loop = Regions.getRegion(Root->Children.front());
  assert(Loop != nullptr);
  assert(Loop->Kind == RegionKind::NaturalLoop);
  assert(Loop->Head == 0);
  assert(Loop->Blocks == std::vector<BlockId>({0, 1, 2, 3, 4}));
  assert(Loop->Successors == std::vector<BlockId>({5}));
  assert(Loop->Follow == 5);
}

void testSAILROrderPrefersLeastSiblingEdges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {3, 4}));
  Cfg.addBlock(block(3, {}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  std::vector<VirtualEdge> Edges = {
      {1, 3, 1, 3, VirtualEdgeKind::Goto},
      {2, 4, 2, 4, VirtualEdgeKind::Goto},
  };

  TestSAILRStructurer Structurer;
  std::vector<VirtualEdge> Ordered =
      Structurer.orderVirtualizableEdges(Cfg, Graph, Analysis, Edges);
  assert(!Ordered.empty());
  assert(Ordered.front().FromBlock == 1);
  assert(Ordered.front().ToBlock == 3);
}

void testSAILROrderPrefersMostPostDominators() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {3, 4}));
  Cfg.addBlock(block(2, {3, 4}));
  Cfg.addBlock(block(3, {5}));
  Cfg.addBlock(block(4, {5, 6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {5}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5, 6};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  std::vector<VirtualEdge> Edges = {
      {1, 3, 1, 3, VirtualEdgeKind::Goto},
      {1, 4, 1, 4, VirtualEdgeKind::Goto},
  };

  TestSAILRStructurer Structurer;
  std::vector<VirtualEdge> Ordered =
      Structurer.orderVirtualizableEdges(Cfg, Graph, Analysis, Edges);
  assert(!Ordered.empty());
  assert(Ordered.front().FromBlock == 1);
  assert(Ordered.front().ToBlock == 4);
}

void testSAILROrderPrefersReturnTargetTieBreak() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {3, 4}));
  Cfg.addBlock(block(2, {3, 4}));
  Cfg.addBlock(block(3, {}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  std::vector<VirtualEdge> Edges = {
      {1, 3, 1, 3, VirtualEdgeKind::Goto},
      {1, 4, 1, 4, VirtualEdgeKind::Goto},
  };

  TestSAILRStructurer Structurer(/*PostDomMaxEdges=*/0,
                                 /*PostDomMaxGraphSize=*/0);
  std::vector<VirtualEdge> Ordered =
      Structurer.orderVirtualizableEdges(Cfg, Graph, Analysis, Edges);
  assert(!Ordered.empty());
  assert(Ordered.front().FromBlock == 1);
  assert(Ordered.front().ToBlock == 3);
}

} // namespace

int main() {
  testAcyclicDroppedEdges();
  testEdgeVirtualizationHints();
  testSwitchVirtualizationInstallsSourceRoot();
  testFallthroughVirtualizationInstallsSourceRoot();
  testStructurerRegistryNames();
  testMergedNaturalLoopKeepsAllLatchPaths();
  testSAILROrderPrefersLeastSiblingEdges();
  testSAILROrderPrefersMostPostDominators();
  testSAILROrderPrefersReturnTargetTieBreak();
  return 0;
}
