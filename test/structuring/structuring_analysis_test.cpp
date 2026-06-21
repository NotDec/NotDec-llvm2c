#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/GotoManager.h"
#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"
#include "notdec-backends/Structuring/RegionOverlay.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"
#include "notdec-backends/Structuring/StructuringEvaluator.h"
#include "notdec-backends/Structuring/StructuringOptimizationPass.h"
#include "notdec-backends/Structuring/StructuringQuality.h"

#include <algorithm>
#include <cassert>
#include <map>
#include <memory>
#include <vector>

using namespace notdec::backend::structuring;

namespace {

class HintStructurer : public PhoenixStructurer {
public:
  using PhoenixStructurer::lastResortRefinement;
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
  using SAILRStructurer::useImprovedCyclicSchemas;
};

class TestPhoenixStructurer : public PhoenixStructurer {
public:
  using PhoenixStructurer::refineCyclic;
  using PhoenixStructurer::virtualizeOneEdge;
};

class OrderCaptureStructurer : public PhoenixStructurer {
public:
  using PhoenixStructurer::virtualizeOneEdge;

  std::vector<VirtualEdge> orderVirtualizableEdges(
      const StructuredCFG &Cfg, const MutableRegionGraph &Graph,
      const MutableRegionGraphAnalysis &Analysis,
      std::vector<VirtualEdge> Edges) const override {
    (void)Cfg;
    (void)Graph;
    CapturedOrder = Analysis.NodeOrder;
    CapturedEdges = Edges;
    return {};
  }

  mutable std::map<GraphNodeId, unsigned> CapturedOrder;
  mutable std::vector<VirtualEdge> CapturedEdges;
};

bool treeContainsKind(const StructuredTree &Tree, NodeId Id,
                      StructuredNodeKind Kind) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == Kind) {
    return true;
  }
  for (NodeId Child : Node->Children) {
    if (treeContainsKind(Tree, Child, Kind)) {
      return true;
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    if (treeContainsKind(Tree, Case.Body, Kind)) {
      return true;
    }
  }
  return treeContainsKind(Tree, Node->Then, Kind) ||
         treeContainsKind(Tree, Node->Else, Kind) ||
         treeContainsKind(Tree, Node->Body, Kind) ||
         treeContainsKind(Tree, Node->Default, Kind);
}

bool treeContainsGotoTarget(const StructuredTree &Tree, NodeId Id,
                            BlockId Target) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == StructuredNodeKind::Goto && Node->Target == Target) {
    return true;
  }
  for (NodeId Child : Node->Children) {
    if (treeContainsGotoTarget(Tree, Child, Target)) {
      return true;
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    if (treeContainsGotoTarget(Tree, Case.Body, Target)) {
      return true;
    }
  }
  return treeContainsGotoTarget(Tree, Node->Then, Target) ||
         treeContainsGotoTarget(Tree, Node->Else, Target) ||
         treeContainsGotoTarget(Tree, Node->Body, Target) ||
         treeContainsGotoTarget(Tree, Node->Default, Target);
}

class RecordingRegionStructurer : public RegionStructurer {
public:
  bool supportsChildRegions() const override { return true; }

  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override {
    (void)Cfg;
    SeenRegions.push_back(R.Kind);

    StructuredNode Node;
    Node.Kind = StructuredNodeKind::Sequence;
    Node.Block = R.Head;
    return Tree.addNode(std::move(Node));
  }

  std::vector<RegionKind> SeenRegions;
};

class FailingRegionStructurer : public RegionStructurer {
public:
  bool supportsChildRegions() const override { return true; }

  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override {
    (void)Cfg;
    SeenHeads.push_back(R.Head);
    if (R.Head == FailingHead) {
      return InvalidNodeId;
    }

    StructuredNode Node;
    Node.Kind = StructuredNodeKind::Sequence;
    Node.Block = R.Head;
    return Tree.addNode(std::move(Node));
  }

  BlockId FailingHead = InvalidBlockId;
  std::vector<BlockId> SeenHeads;
};

class GotoEmittingRegionStructurer : public RegionStructurer {
public:
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override {
    (void)Cfg;
    StructuredNode Root;
    Root.Kind = StructuredNodeKind::Sequence;

    StructuredNode Body;
    Body.Kind = StructuredNodeKind::BasicBlock;
    Body.Block = R.Head;
    Root.Children.push_back(Tree.addNode(std::move(Body)));

    StructuredNode Goto;
    Goto.Kind = StructuredNodeKind::Goto;
    Goto.Target = Target;
    Root.Children.push_back(Tree.addNode(std::move(Goto)));

    return Tree.addNode(std::move(Root));
  }

  BlockId Target = InvalidBlockId;
};

class CFGEdgeGotoRegionStructurer : public RegionStructurer {
public:
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override {
    StructuredNode Root;
    Root.Kind = StructuredNodeKind::Sequence;

    for (BlockId Id : R.Blocks) {
      const CFGBlock *Block = Cfg.getBlock(Id);
      if (Block == nullptr) {
        continue;
      }

      StructuredNode Label;
      Label.Kind = StructuredNodeKind::Label;
      Label.Block = Id;
      Root.Children.push_back(Tree.addNode(std::move(Label)));

      for (BlockId Succ : Block->Successors) {
        StructuredNode Goto;
        Goto.Kind = StructuredNodeKind::Goto;
        Goto.Target = Succ;
        Root.Children.push_back(Tree.addNode(std::move(Goto)));
      }
    }

    return Tree.addNode(std::move(Root));
  }
};

class FailOnBlockRegionStructurer : public CFGEdgeGotoRegionStructurer {
public:
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override {
    if (std::find(R.Blocks.begin(), R.Blocks.end(), FailingBlock) !=
        R.Blocks.end()) {
      return InvalidNodeId;
    }
    return CFGEdgeGotoRegionStructurer::structureRegion(Cfg, R, Tree);
  }

  BlockId FailingBlock = 99;
};

class RemoveFirstSuccessorPass : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override {
    (void)Current;
    for (CFGBlock &Block : Graph.blocks()) {
      if (!Block.Successors.empty()) {
        Block.Successors.clear();
        Block.Terminator = TerminatorKind::Return;
        return true;
      }
    }
    return false;
  }
};

class AddFirstSuccessorPass : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override {
    (void)Current;
    for (CFGBlock &Block : Graph.blocks()) {
      if (Block.Id == 0 && Block.Successors.empty()) {
        Block.Successors.push_back(1);
        Block.Terminator = TerminatorKind::Fallthrough;
        return true;
      }
    }
    return false;
  }
};

class RecoverThenRemoveSuccessorPass : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

  unsigned Attempts = 0;

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override {
    (void)Current;
    ++Attempts;

    if (Attempts == 1) {
      for (CFGBlock &Block : Graph.blocks()) {
        if (Block.Id == 0) {
          Block.Successors = {99};
          Block.Terminator = TerminatorKind::Fallthrough;
          break;
        }
      }

      CFGBlock BadBlock;
      BadBlock.Id = 99;
      BadBlock.Terminator = TerminatorKind::Return;
      Graph.addBlock(std::move(BadBlock));
      return true;
    }

    for (CFGBlock &Block : Graph.blocks()) {
      if (Block.Id == 0 && !Block.Successors.empty()) {
        Block.Successors.clear();
        Block.Terminator = TerminatorKind::Return;
        return true;
      }
    }
    return false;
  }
};

class GotoRegionTester : public GotoStructurer {
public:
  using GotoStructurer::structureRegion;
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
  Root.Head = 2;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();

  assert(Analysis.AcyclicDroppedEdges.size() == 1);
  const VirtualEdge &Edge = Analysis.AcyclicDroppedEdges.front();
  assert(Edge.FromBlock == 4);
  assert(Edge.ToBlock == 1);
}

void testMutableRegionGraphCheckpointRestoresMutations() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 2;
  Root.Blocks = {0, 1, 2};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  std::size_t Checkpoint = Graph.checkpoint();
  Graph.virtualizeEdge(Graph.getNodeForBlock(0), Graph.getNodeForBlock(1),
                       VirtualEdgeKind::Goto);
  Graph.collapseNodes({Graph.getNodeForBlock(1), Graph.getNodeForBlock(2)}, 1,
                      42);
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.activeNodes().size() == 2);
  const MutableRegionNode *Collapsed = Graph.getNode(Graph.getNodeForBlock(1));
  assert(Collapsed != nullptr);
  assert(Collapsed->SourceNodes ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(1),
                                      OverlayNodeKey::block(2)}));

  Graph.rollback(Checkpoint);
  Graph.commit(Checkpoint);
  assert(Graph.virtualEdges().empty());
  assert(Graph.activeNodes().size() == 3);
  assert(Graph.hasEdge(Graph.getNodeForBlock(0), Graph.getNodeForBlock(1)));
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

void testOverlayVirtualizationReplacesSourceNode() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};

  RegionTree Regions;
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);
  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  GraphNodeId From = Graph.getNodeForBlock(0);
  GraphNodeId To = Graph.getNodeForBlock(2);
  StructuredTree Tree;
  HintStructurer Structurer(From, To);

  assert(Structurer.virtualizeOneEdge(Cfg, Root, Graph, Tree, RootOverlay));
  const MutableRegionNode *Source = Graph.getNode(From);
  assert(Source != nullptr);
  assert(Source->StructuredRoot != InvalidNodeId);
  OverlayNodeKey Structured =
      OverlayNodeKey::structured(Source->StructuredRoot, RootId);
  assert(Source->SourceNodes == std::vector<OverlayNodeKey>({Structured}));

  const std::vector<OverlayMember> &Members = Manager.members(RootId);
  auto StructuredIt = std::find_if(
      Members.begin(), Members.end(), [&](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured &&
               Manager.nodeKey(Member) == Structured;
      });
  assert(StructuredIt != Members.end());
  assert(std::find_if(Members.begin(), Members.end(),
                      [](const OverlayMember &Member) {
                        return Member.Kind == OverlayMemberKind::Block &&
                               Member.Block == 0;
                      }) == Members.end());

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true,
                            /*IncludeMarkedEdges=*/true);
  bool HasKeptEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == Structured &&
               Edge.targetsMember() && Edge.To.Kind == OverlayMemberKind::Block &&
               Edge.To.Block == 1;
      }) != Edges.end();
  bool HasRemovedEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == Structured &&
               Edge.targetsMember() && Edge.To.Kind == OverlayMemberKind::Block &&
               Edge.To.Block == 2;
      }) != Edges.end();
  assert(HasKeptEdge);
  assert(!HasRemovedEdge);
}

void testSwitchVirtualizationInstallsSourceRoot() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2, 3}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::NaturalLoop;
  Root.Head = 0;
  Root.Latch = 3;
  Root.Successors = {4};
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
  llvm::ArrayRef<std::string_view> KnownNames = knownStructurerNames();
  assert(KnownNames.size() == 4);
  assert(KnownNames[0] == "goto");
  assert(KnownNames[1] == "phoenix");
  assert(KnownNames[2] == "sailr");
  assert(KnownNames[3] == "dream");

  llvm::ArrayRef<std::string_view> Names = registeredStructurerNames();
  assert(Names.size() == 3);
  assert(Names[0] == "goto");
  assert(Names[1] == "phoenix");
  assert(Names[2] == "sailr");
  assert(resolveStructurerName("structured-goto") == "goto");
  assert(resolveStructurerName("structured-phoenix") == "phoenix");
  assert(resolveStructurerName("structured-sailr") == "sailr");
  assert(resolveStructurerName("structured-dream") == "dream");
  assert(resolveStructurerName("phoenix").empty());
  assert(resolveStructurerName("missing").empty());

  std::unique_ptr<Structurer> Goto = createStructurer("GOTO");
  std::unique_ptr<Structurer> Phoenix = createStructurer("Phoenix");
  std::unique_ptr<Structurer> Sailr = createStructurer("sailr");
  std::unique_ptr<Structurer> StructuredPhoenix =
      createStructurer("structured-phoenix");
  std::unique_ptr<Structurer> Missing = createStructurer("dream");
  assert(Goto != nullptr);
  assert(Phoenix != nullptr);
  assert(Sailr != nullptr);
  assert(StructuredPhoenix != nullptr);
  assert(Missing == nullptr);
}

void testGotoManagerCollectsSequenceGotoSources() {
  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = 10;
  Root.Children.push_back(Tree.addNode(std::move(Body)));

  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 20;
  Root.Children.push_back(Tree.addNode(std::move(Goto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  GotoManager Manager = GotoManager::collect(Tree);
  assert(Manager.size() == 1);
  assert(Manager.isGotoEdge(10, 20));
  assert(!Manager.isGotoEdge(20, 10));
  std::vector<StructuredGoto> Gotos = Manager.gotosInBlock(10);
  assert(Gotos.size() == 1);
  assert(Gotos.front().Source == 10);
  assert(Gotos.front().Target == 20);
}

void testGotoManagerCollectsIfGotoSources() {
  StructuredTree Tree;
  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 30;
  NodeId GotoId = Tree.addNode(std::move(Goto));

  StructuredNode IfNode;
  IfNode.Kind = StructuredNodeKind::If;
  IfNode.Block = 11;
  IfNode.Then = GotoId;

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;
  Root.Children.push_back(Tree.addNode(std::move(IfNode)));
  Tree.setRoot(Tree.addNode(std::move(Root)));

  GotoManager Manager = GotoManager::collect(Tree);
  assert(Manager.size() == 1);
  assert(Manager.isGotoEdge(11, 30));
  assert(Manager.gotosInBlock(10).empty());
}

void testStructuringEvaluatorCollectsGotoSummary() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  GotoEmittingRegionStructurer Structurer;
  Structurer.Target = 1;
  StructuringEvaluation Result =
      StructuringEvaluator().evaluate(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Tree.root() != InvalidNodeId);
  assert(Result.Gotos.isGotoEdge(0, 1));
  assert(Result.Quality.GotoTargets[1] == 1);
}

void testStructuringEvaluatorRemovesEdgesForTrialOnly() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  CFGEdgeGotoRegionStructurer Structurer;
  StructuringEvaluation Result =
      StructuringEvaluator().evaluate(Cfg, Structurer, {{0, 1}});

  assert(Result.Succeeded);
  assert(Result.Gotos.empty());
  const CFGBlock *Original = Cfg.getBlock(0);
  assert(Original != nullptr);
  assert((Original->Successors == std::vector<BlockId>{1}));
}

void testControlFlowStructureCounterCollectsSharedQuality() {
  StructuredTree Tree;

  StructuredNode Label10;
  Label10.Kind = StructuredNodeKind::Label;
  Label10.Block = 10;

  StructuredNode Goto20;
  Goto20.Kind = StructuredNodeKind::Goto;
  Goto20.Target = 20;

  StructuredNode WhileNode;
  WhileNode.Kind = StructuredNodeKind::While;
  WhileNode.Block = 11;
  WhileNode.Body = Tree.addNode(std::move(Goto20));

  StructuredNode Label20;
  Label20.Kind = StructuredNodeKind::Label;
  Label20.Block = 20;

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;
  Root.Children.push_back(Tree.addNode(std::move(Label10)));
  Root.Children.push_back(Tree.addNode(std::move(WhileNode)));
  Root.Children.push_back(Tree.addNode(std::move(Label20)));
  Tree.setRoot(Tree.addNode(std::move(Root)));

  ControlFlowStructureCounter Counter =
      ControlFlowStructureCounter::collect(Tree);
  assert(Counter.WhileLoops == 1);
  assert(Counter.DoWhileLoops == 0);
  assert(Counter.GotoTargets[20] == 1);
  assert((Counter.OrderedLabels == std::vector<BlockId>{10, 20}));
}

void testRelativeQualityRejectsBackwardGotoTrade() {
  ControlFlowStructureCounter Initial;
  Initial.GotoTargets[20] = 1;
  Initial.OrderedLabels = {10, 20};

  ControlFlowStructureCounter Current;
  Current.GotoTargets[10] = 1;
  Current.OrderedLabels = {10, 20};

  assert(!improvesRelativeStructuringQuality(Initial, Current));
  assert(improvesRelativeStructuringQuality(Current, Initial));
}

void testRelativeQualityRejectsMoreGotoTargets() {
  ControlFlowStructureCounter Initial;
  Initial.GotoTargets[20] = 2;
  Initial.OrderedLabels = {10, 20};

  ControlFlowStructureCounter Current;
  Current.GotoTargets[10] = 1;
  Current.GotoTargets[20] = 1;
  Current.OrderedLabels = {10, 20};

  assert(!improvesRelativeStructuringQuality(Initial, Current));
}

void testStructuringOptimizationPassAcceptsImprovedGraph() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  CFGEdgeGotoRegionStructurer Structurer;
  RemoveFirstSuccessorPass Pass;
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.empty());
  assert(Result.Evaluation.Gotos.empty());
}

void testStructuringOptimizationPassRejectsNewGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  CFGEdgeGotoRegionStructurer Structurer;
  AddFirstSuccessorPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(!Result.Succeeded);
}

void testStructuringOptimizationPassUsesRemovedEdgesForInitialGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.EdgesToRemove.push_back({0, 1});
  CFGEdgeGotoRegionStructurer Structurer;
  RemoveFirstSuccessorPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(!Result.Succeeded);
}

void testStructuringOptimizationPassRecoversAndContinuesFixedPoint() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 2;
  FailOnBlockRegionStructurer Structurer;
  RecoverThenRemoveSuccessorPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Pass.Attempts == 2);
  assert(Result.Succeeded);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.empty());
  assert(Result.Output.getBlock(99) == nullptr);
}

void testRecursiveStructurerVisitsChildBeforeParent() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {0}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions = RegionIdentifier::identifyRoot(Cfg);
  RegionId RootId = Regions.root();
  const Region *Root = Regions.getRegion(RootId);
  assert(Root != nullptr);
  assert(Root->Children.size() == 1);
  OverlayManager Manager(std::move(Regions));

  RecordingRegionStructurer Structurer;
  StructuredTree Tree =
      RecursiveStructurer().structure(Cfg, Manager, Structurer);
  assert(Tree.root() != InvalidNodeId);
  assert(Structurer.SeenRegions.size() == 2);
  assert(Structurer.SeenRegions[0] == RegionKind::NaturalLoop);
  assert(Structurer.SeenRegions[1] == RegionKind::Root);
  assert(Manager.getStructuredRoot(RootId) == InvalidNodeId);
  assert(Manager.finalizedChildren(RootId).empty());
}

void testRecursiveStructurerVisitsDissolvedChildMembers() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {1, 2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Nested;
  Nested.Kind = RegionKind::NaturalLoop;
  Nested.Head = 1;
  Nested.Blocks = {1};
  RegionId NestedId = Regions.addRegion(Nested);
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 0;
  Child.Blocks = {0, 1};
  Child.Children = {NestedId};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 2;
  Root.Blocks = {0, 1, 2};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  FailingRegionStructurer Structurer;
  Structurer.FailingHead = 0;
  StructuredTree Tree =
      RecursiveStructurer().structure(Cfg, Manager, Structurer);
  assert(Tree.root() != InvalidNodeId);
  assert(Structurer.SeenHeads == std::vector<BlockId>({1, 0, 2}));
  assert(Manager.parentOf(NestedId) == ChildId);
  assert(Manager.parentOf(ChildId) == RootId);
}

void testGotoRegionSkipsChildBlocks() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  RegionTree Regions;
  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = 1;
  Loop.Blocks = {1};
  RegionId LoopId = Regions.addRegion(Loop);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1};
  Root.Children = {LoopId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *LoopOverlay = Manager.getRegion(LoopId);
  RegionOverlay *RootOverlay = Manager.getRegion(RootId);
  assert(LoopOverlay != nullptr);
  assert(RootOverlay != nullptr);

  GotoRegionTester Structurer;
  StructuredTree Tree;
  NodeId LoopRoot = Structurer.structureRegion(Cfg, *LoopOverlay, Tree);
  NodeId UnfinalizedRoot = Structurer.structureRegion(Cfg, *RootOverlay, Tree);
  assert(LoopRoot != InvalidNodeId);
  const StructuredNode *UnfinalizedRootNode = Tree.getNode(UnfinalizedRoot);
  assert(UnfinalizedRootNode != nullptr);
  bool FoundUnfinalizedChildLabel = false;
  for (NodeId ChildId : UnfinalizedRootNode->Children) {
    const StructuredNode *Child = Tree.getNode(ChildId);
    if (Child != nullptr && Child->Kind == StructuredNodeKind::Label &&
        Child->Block == 1) {
      FoundUnfinalizedChildLabel = true;
    }
  }
  assert(FoundUnfinalizedChildLabel);

  SuccessorSnapshot Snapshot = LoopOverlay->snapshotSuccessors();
  assert(Snapshot.Successors.empty());
  assert(Snapshot.NodeSuccessors.empty());
  LoopOverlay->finalize(LoopRoot, Snapshot);
  NodeId RootRoot = Structurer.structureRegion(Cfg, *RootOverlay, Tree);
  const StructuredNode *RootNode = Tree.getNode(RootRoot);
  assert(RootNode != nullptr);
  assert(!RootNode->Children.empty());
  assert(RootNode->Children.front() == LoopRoot);
  for (NodeId ChildId : RootNode->Children) {
    const StructuredNode *Child = Tree.getNode(ChildId);
    assert(Child == nullptr || Child->Kind != StructuredNodeKind::Label ||
           Child->Block != 1);
  }
}

void testVisibleRegionTreeOnlyIncludesFinalizedChildren() {
  RegionTree Regions;
  Region FirstChild;
  FirstChild.Kind = RegionKind::NaturalLoop;
  FirstChild.Head = 1;
  FirstChild.Blocks = {1};
  FirstChild.Successors = {3};
  RegionId FirstChildId = Regions.addRegion(FirstChild);
  Region SecondChild;
  SecondChild.Kind = RegionKind::NaturalLoop;
  SecondChild.Head = 2;
  SecondChild.Blocks = {2};
  RegionId SecondChildId = Regions.addRegion(SecondChild);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  Root.Children = {FirstChildId, SecondChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionTree InitialVisible = Manager.visibleRegionTree();
  const Region *InitialRoot = InitialVisible.getRegion(RootId);
  assert(InitialRoot != nullptr);
  assert(InitialRoot->Children.empty());
  assert(Manager.finalizedChildren(RootId).empty());

  RegionOverlay *FirstChildOverlay = Manager.getRegion(FirstChildId);
  assert(FirstChildOverlay != nullptr);
  SuccessorSnapshot FirstChildSnapshot =
      FirstChildOverlay->snapshotSuccessors();
  assert(FirstChildSnapshot.Successors == std::vector<BlockId>({3}));
  assert(FirstChildSnapshot.NodeSuccessors ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(3)}));
  FirstChildOverlay->finalize(42, FirstChildSnapshot);
  std::vector<FinalizedChildRegion> Finalized = Manager.finalizedChildren(RootId);
  assert(Finalized.size() == 1);
  assert(Finalized.front().RegionData != nullptr);
  assert(Finalized.front().RegionData->Id == FirstChildId);
  assert(Finalized.front().StructuredRoot == 42);
  assert(Finalized.front().Snapshot.Successors == std::vector<BlockId>({3}));
  assert(Finalized.front().Snapshot.NodeSuccessors ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(3)}));

  RegionTree Visible = Manager.visibleRegionTree();
  const Region *VisibleRoot = Visible.getRegion(RootId);
  assert(VisibleRoot != nullptr);
  assert(VisibleRoot->Children == std::vector<RegionId>({FirstChildId}));

  FirstChildOverlay->dissolve();
  RegionTree DissolvedVisible = Manager.visibleRegionTree();
  const Region *DissolvedRoot = DissolvedVisible.getRegion(RootId);
  assert(DissolvedRoot != nullptr);
  assert(DissolvedRoot->Children.empty());
  assert(Manager.finalizedChildren(RootId).empty());

  std::size_t Checkpoint = Manager.checkpoint();
  FirstChildOverlay->finalize(42, FirstChildSnapshot);
  assert(!Manager.finalizedChildren(RootId).empty());
  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.finalizedChildren(RootId).empty());
}

void testOverlayManagerInitialMembersMatchRegionTree() {
  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1, 2};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  assert(Manager.parentOf(ChildId) == RootId);
  assert(Manager.ownerOf(0) == RootId);
  assert(Manager.ownerOf(1) == ChildId);
  assert(Manager.ownerOf(2) == ChildId);
  assert(Manager.ownerOf(3) == RootId);

  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(RootMembers.size() == 3);
  assert(RootMembers[0].Kind == OverlayMemberKind::Region);
  assert(RootMembers[0].Region == ChildId);
  assert(RootMembers[1].Kind == OverlayMemberKind::Block);
  assert(RootMembers[1].Block == 0);
  assert(RootMembers[2].Kind == OverlayMemberKind::Block);
  assert(RootMembers[2].Block == 3);

  const std::vector<OverlayMember> &ChildMembers = Manager.members(ChildId);
  assert(ChildMembers.size() == 2);
  assert(ChildMembers[0].Kind == OverlayMemberKind::Block);
  assert(ChildMembers[0].Block == 1);
  assert(ChildMembers[1].Kind == OverlayMemberKind::Block);
  assert(ChildMembers[1].Block == 2);
}

void testOverlayManagerFinalizeAndDissolveUpdateMembers() {
  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1, 2};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);

  std::size_t Checkpoint = Manager.checkpoint();
  ChildOverlay->finalize(42, SuccessorSnapshot{{3}});
  const std::vector<OverlayMember> &FinalizedRootMembers =
      Manager.members(RootId);
  assert(FinalizedRootMembers.size() == 3);
  assert(FinalizedRootMembers[0].Kind == OverlayMemberKind::Structured);
  assert(FinalizedRootMembers[0].Region == ChildId);
  assert(FinalizedRootMembers[0].StructuredRoot == 42);
  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.members(RootId).front().Kind == OverlayMemberKind::Region);

  Checkpoint = Manager.checkpoint();
  ChildOverlay->dissolve();
  const std::vector<OverlayMember> &DissolvedRootMembers =
      Manager.members(RootId);
  assert(DissolvedRootMembers.size() == 4);
  assert(DissolvedRootMembers[0].Kind == OverlayMemberKind::Block);
  assert(DissolvedRootMembers[0].Block == 1);
  assert(DissolvedRootMembers[1].Kind == OverlayMemberKind::Block);
  assert(DissolvedRootMembers[1].Block == 2);
  assert(Manager.ownerOf(1) == RootId);
  assert(Manager.ownerOf(2) == RootId);
  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.ownerOf(1) == ChildId);
  assert(Manager.ownerOf(2) == ChildId);
  assert(Manager.members(RootId).front().Kind == OverlayMemberKind::Region);
}

void testOverlayManagerKeepsSharedCFGSuccessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  assert(Manager.sharedSuccessors(0) == std::vector<BlockId>({1, 2}));
  assert(Manager.sharedSuccessors(1) == std::vector<BlockId>({2}));
  assert(Manager.sharedSuccessors(2).empty());
  assert(Manager.sharedSuccessors(99).empty());
}

void testOverlayManagerDerivesVisibleSuccessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {0, 1}));
  Cfg.addBlock(block(1, {}));

  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  RegionOverlay *Root = Manager.root();
  assert(Root != nullptr);
  assert(Root->children().size() == 1);
  RegionId LoopId = Root->children().front();

  assert(Manager.visibleSuccessors(Root->id()).empty());
  assert(Manager.visibleSuccessors(LoopId) == std::vector<BlockId>({1}));
}

void testOverlayVisibleSuccessorsMatchIdentifiedLoopSuccessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 4}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  RegionOverlay *Root = Manager.root();
  assert(Root != nullptr);
  assert(Root->children().size() == 1);
  RegionId LoopId = Root->children().front();
  const Region *Loop = Manager.getRegionData(LoopId);
  assert(Loop != nullptr);
  assert(Manager.visibleSuccessors(LoopId) == Loop->Successors);
}

void testSnapshotSuccessorsFallsBackWithoutSharedCFG() {
  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1};
  Child.Successors = {3};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);
  assert(ChildOverlay->snapshotSuccessors().Successors ==
         std::vector<BlockId>({3}));
}

void testOverlayManagerDerivesQuotientEdges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {1, 3}));
  Cfg.addBlock(block(3, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1, 2};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  std::vector<OverlayViewEdge> MemberEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  assert(MemberEdges.size() == 2);
  auto HasRootToChild =
      std::find_if(MemberEdges.begin(), MemberEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 0 &&
                            Edge.To.Kind == OverlayMemberKind::Region &&
                            Edge.To.Region == ChildId &&
                            Edge.targetsMember();
                   }) != MemberEdges.end();
  auto HasChildToFollow =
      std::find_if(MemberEdges.begin(), MemberEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Region &&
                            Edge.From.Region == ChildId &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 3 && Edge.targetsMember();
                   }) != MemberEdges.end();
  assert(HasRootToChild);
  assert(HasChildToFollow);

  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(ChildId, /*IncludeSuccessors=*/true);
  assert(FullEdges.size() == 3);
  auto HasExternalFollow =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 2 && !Edge.targetsMember() &&
                            Edge.ExternalSuccessor == 3;
                   }) != FullEdges.end();
  assert(HasExternalFollow);
}

void testOverlayManagerQuotientKeepsBlockSelfLoop() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {0, 1}));
  Cfg.addBlock(block(1, {}));

  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  RegionOverlay *Root = Manager.root();
  assert(Root != nullptr);
  assert(Root->children().size() == 1);
  RegionId LoopId = Root->children().front();

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  assert(Edges.size() == 2);
  auto HasSelfLoop =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return Edge.From.Kind == OverlayMemberKind::Block &&
               Edge.From.Block == 0 &&
               Edge.To.Kind == OverlayMemberKind::Block && Edge.To.Block == 0 &&
               Edge.targetsMember();
      }) != Edges.end();
  auto HasExternalFollow =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return Edge.From.Kind == OverlayMemberKind::Block &&
               Edge.From.Block == 0 && !Edge.targetsMember() &&
               Edge.ExternalSuccessor == 1;
      }) != Edges.end();
  assert(HasSelfLoop);
  assert(HasExternalFollow);
}

void testOverlayAcyclicViewsFilterBackEdgesByOrder() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {1}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionTree Regions;
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  std::map<OverlayNodeKey, unsigned> DerivedOrder =
      Manager.quasiTopologicalNodeOrder(RootId);
  assert(DerivedOrder[OverlayNodeKey::block(0)] == 0);
  assert(DerivedOrder[OverlayNodeKey::block(1)] == 1);
  assert(DerivedOrder[OverlayNodeKey::block(2)] == 2);

  std::vector<OverlayViewEdge> RawEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  std::vector<OverlayViewEdge> AcyclicEdges = Manager.quotientEdgesAcyclic(
      RootId, /*IncludeSuccessors=*/false, DerivedOrder);
  assert(RawEdges.size() == 3);
  assert(AcyclicEdges.size() == 2);
  auto HasBackEdge =
      std::find_if(AcyclicEdges.begin(), AcyclicEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 2 &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 1;
                   }) != AcyclicEdges.end();
  assert(!HasBackEdge);

  std::vector<OverlayViewEdge> BlacklistedEdges =
      Manager.quotientEdgesBlacklisted(
          RootId, /*IncludeSuccessors=*/false,
          {{OverlayNodeKey::block(0), OverlayNodeKey::block(1)}});
  assert(BlacklistedEdges.size() == 2);
  auto HasBlacklistedEdge =
      std::find_if(BlacklistedEdges.begin(), BlacklistedEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 0 &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 1;
                   }) != BlacklistedEdges.end();
  assert(!HasBlacklistedEdge);
  assert(Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false).size() ==
         3);

  Region Child;
  Child.Kind = RegionKind::Root;
  Child.Head = 1;
  Child.Blocks = {1};
  RegionTree ChildRegions;
  RegionId ChildId = ChildRegions.addRegion(Child);
  Region Parent;
  Parent.Kind = RegionKind::Root;
  Parent.Head = 0;
  Parent.Blocks = {0, 1, 2};
  Parent.Children = {ChildId};
  RegionId ParentId = ChildRegions.addRegion(Parent);
  ChildRegions.setRoot(ParentId);

  OverlayManager ChildManager(std::move(ChildRegions), Cfg);
  assert(ChildManager.visibleSuccessors(ChildId) ==
         std::vector<BlockId>({2}));
  assert(ChildManager.visibleSuccessorsBlacklisted(
             ChildId, {{OverlayNodeKey::block(1), OverlayNodeKey::block(2)}})
             .empty());
  std::map<OverlayNodeKey, unsigned> ChildOrder = {
      {OverlayNodeKey::block(1), 1},
      {OverlayNodeKey::block(2), 0},
  };
  assert(ChildManager.visibleSuccessorsAcyclic(ChildId, ChildOrder).empty());
  std::map<OverlayNodeKey, unsigned> DerivedChildOrder =
      ChildManager.quasiTopologicalNodeOrder(ChildId);
  assert(DerivedChildOrder[OverlayNodeKey::block(1)] == 0);
  assert(DerivedChildOrder[OverlayNodeKey::block(2)] == 1);
}

void testPhoenixOverlayPathUsesOverlayNodeOrder() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  MutableRegionGraphAnalysis Analysis = Graph.analyze();
  assert(Analysis.NodeOrder[0] == 1);
  assert(Analysis.NodeOrder[1] == 0);
  assert(Analysis.NodeOrder[2] == 2);

  OrderCaptureStructurer Plain;
  StructuredTree PlainTree;
  assert(!Plain.virtualizeOneEdge(Cfg, Root, Graph, PlainTree));
  assert(Plain.CapturedOrder == Analysis.NodeOrder);

  RegionTree Regions;
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);
  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph OverlayGraph =
      MutableRegionGraph::build(Cfg, *RootOverlay);
  OrderCaptureStructurer OverlayPath;
  StructuredTree OverlayTree;
  assert(!OverlayPath.virtualizeOneEdge(Cfg, Root, OverlayGraph, OverlayTree,
                                        RootOverlay));
  assert(OverlayPath.CapturedOrder[0] == 0);
  assert(OverlayPath.CapturedOrder[1] == 1);
  assert(OverlayPath.CapturedOrder[2] == 2);
  assert(OverlayPath.CapturedOrder != Analysis.NodeOrder);
}

void testPhoenixOverlayLastResortUsesOverlayAcyclicCandidates() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {0, 2}));
  Cfg.addBlock(block(2, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};

  RegionTree Regions;
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);
  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  OrderCaptureStructurer Structurer;
  StructuredTree Tree;
  assert(!Structurer.virtualizeOneEdge(Cfg, Root, Graph, Tree, RootOverlay));

  bool HasBackEdge =
      std::find_if(Structurer.CapturedEdges.begin(),
                   Structurer.CapturedEdges.end(), [](const VirtualEdge &Edge) {
                     return Edge.FromBlock == 1 && Edge.ToBlock == 0;
                   }) != Structurer.CapturedEdges.end();
  assert(!Structurer.CapturedEdges.empty());
  assert(!HasBackEdge);
}

void testOverlayGraphBuildsEdgesFromQuotientView() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {1, 3}));
  Cfg.addBlock(block(3, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1, 2};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);

  GraphNodeId EntryId = Graph.getNodeForBlock(0);
  GraphNodeId ChildHeadId = Graph.getNodeForBlock(1);
  GraphNodeId FollowId = Graph.getNodeForBlock(3);
  GraphNodeId ChildTailId = Graph.getNodeForBlock(2);
  assert(EntryId != InvalidGraphNodeId);
  assert(ChildHeadId != InvalidGraphNodeId);
  assert(FollowId != InvalidGraphNodeId);
  assert(ChildTailId == ChildHeadId);
  assert(Graph.hasEdge(EntryId, ChildHeadId));
  assert(Graph.hasEdge(ChildHeadId, FollowId));
  assert(!Graph.hasEdge(ChildHeadId, ChildHeadId));
}

void testOverlayFullViewAddsLoopSuccessorEdges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = 0;
  Loop.Blocks = {0};
  RegionId LoopId = Regions.addRegion(Loop);
  Regions.setRoot(LoopId);

  OverlayManager Manager(std::move(Regions), Cfg);
  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  auto HasSuccessorEdge =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return !Edge.sourcesMember() && Edge.ExternalSource == 1 &&
               !Edge.targetsMember() && Edge.ExternalSuccessor == 2;
      }) != Edges.end();
  assert(HasSuccessorEdge);

  RegionOverlay *LoopOverlay = Manager.root();
  assert(LoopOverlay != nullptr);
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *LoopOverlay);
  GraphNodeId FirstSuccId = Graph.getNodeForBlock(1);
  GraphNodeId SecondSuccId = Graph.getNodeForBlock(2);
  assert(FirstSuccId != InvalidGraphNodeId);
  assert(SecondSuccId != InvalidGraphNodeId);
  assert(Graph.hasEdge(FirstSuccId, SecondSuccId));
}

void testOverlayViewOnlyMutationsAffectQuotientEdges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = 0;
  Loop.Blocks = {0};
  RegionId LoopId = Regions.addRegion(Loop);
  Regions.setRoot(LoopId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *LoopOverlay = Manager.root();
  assert(LoopOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();
  LoopOverlay->hideEdgeToSuccessor(1);
  assert(Manager.visibleSuccessors(LoopId) == std::vector<BlockId>({2}));

  OverlayEdgeEndpoint Head =
      OverlayEdgeEndpoint::member(OverlayMember::block(0));
  OverlayEdgeEndpoint SecondSucc = OverlayEdgeEndpoint::external(2);
  LoopOverlay->removeEdgeWithSuccessorsOnly(Head, SecondSucc);
  std::vector<OverlayViewEdge> MemberOnlyEdges =
      Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/false);
  assert(MemberOnlyEdges.empty());
  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  bool HasHeadToSecondSucc =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 0 && !Edge.targetsMember() &&
                            Edge.ExternalSuccessor == 2;
                   }) != FullEdges.end();
  assert(!HasHeadToSecondSucc);

  OverlayEdgeEndpoint FirstSucc = OverlayEdgeEndpoint::external(1);
  LoopOverlay->addExtraFullEdge(FirstSucc, SecondSucc);
  FullEdges = Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  bool HasExtraEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return !Edge.sourcesMember() && Edge.ExternalSource == 1 &&
                            !Edge.targetsMember() &&
                            Edge.ExternalSuccessor == 2;
                   }) != FullEdges.end();
  assert(HasExtraEdge);

  OverlayNodeKey ExternalStructured = OverlayNodeKey::structured(99);
  OverlayEdgeEndpoint StructuredSucc =
      OverlayEdgeEndpoint::external(ExternalStructured);
  LoopOverlay->addExtraFullEdge(StructuredSucc, SecondSucc);
  LoopOverlay->addExtraFullEdge(FirstSucc, StructuredSucc);
  FullEdges = Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  bool HasStructuredSourceEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return !Edge.sourcesMember() &&
                            Edge.sourceNode() == ExternalStructured &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(2);
                   }) != FullEdges.end();
  bool HasStructuredTargetEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return !Edge.sourcesMember() &&
                            Edge.sourceNode() == OverlayNodeKey::block(1) &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == ExternalStructured;
                   }) != FullEdges.end();
  assert(HasStructuredSourceEdge);
  assert(HasStructuredTargetEdge);

  LoopOverlay->removeEdgeWithSuccessorsOnly(StructuredSucc, SecondSucc);
  FullEdges = Manager.quotientEdges(LoopId, /*IncludeSuccessors=*/true);
  HasStructuredSourceEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return !Edge.sourcesMember() &&
                            Edge.sourceNode() == ExternalStructured &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(2);
                   }) != FullEdges.end();
  assert(!HasStructuredSourceEdge);

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.visibleSuccessors(LoopId) == std::vector<BlockId>({1, 2}));
}

void testOverlaySharedEdgeMutationsUpdateViews() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();

  RootOverlay->hideEdge(0, 1);
  assert(Manager.visibleSuccessors(RootId).empty());
  RootOverlay->addEdge(0, 1);
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({1}));

  RootOverlay->addEdge(0, 2);
  assert(Manager.visibleSuccessors(RootId) ==
         std::vector<BlockId>({1, 2}));
  RootOverlay->detachEdge(0, 1);
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({2}));

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({1}));
}

void testOverlaySharedNodeSuccessorsCanTargetStructuredResults() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  OverlayNodeKey Structured = OverlayNodeKey::structured(42, RootId);
  Manager.addNodeEdge(Structured, OverlayNodeKey::block(1));
  assert(Manager.sharedNodeSuccessors(Structured).size() == 1);
  assert(Manager.sharedNodeSuccessors(Structured).front().isBlock());
  assert(Manager.sharedNodeSuccessors(Structured).front().Block == 1);
  assert(Manager.sharedSuccessors(0) == std::vector<BlockId>({1}));

  std::size_t Checkpoint = Manager.checkpoint();
  Manager.detachNodeEdge(Structured, OverlayNodeKey::block(1));
  assert(Manager.sharedNodeSuccessors(Structured).empty());
  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.sharedNodeSuccessors(Structured).size() == 1);
}

void testOverlayCollapseRegionRewiresSharedGraph() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::Root;
  Child.Head = 1;
  Child.Blocks = {1, 2};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();

  ChildOverlay->collapseTo(77);
  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(!RootMembers.empty());
  assert(RootMembers.front().Kind == OverlayMemberKind::Structured);
  assert(RootMembers.front().StructuredRoot == 77);
  assert(Manager.members(ChildId).empty());
  assert(Manager.getStructuredRoot(ChildId) == 77);
  OverlayNodeKey Result = Manager.nodeKey(RootMembers.front());

  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)) ==
         std::vector<OverlayNodeKey>({Result}));
  assert(Manager.sharedNodeSuccessors(Result) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(3)}));
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(1)).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(2)).empty());
  assert(Manager.ownerOf(1) == InvalidRegionId);
  assert(Manager.ownerOf(2) == InvalidRegionId);

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(1)}));
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(1)) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(2)}));
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(2)) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(3)}));
  assert(Manager.members(RootId).front().Kind == OverlayMemberKind::Region);
}

void testOverlayReplaceNodesRewiresSharedGraphAndBookkeeping() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();

  RootOverlay->hideEdge(1, 2);
  RootOverlay->addExtraFullEdge(OverlayEdgeEndpoint::external(3),
                                OverlayEdgeEndpoint::member(
                                    OverlayMember::block(1)));
  RootOverlay->replaceNodes({OverlayNodeKey::block(1)}, 88);

  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  auto StructuredIt = std::find_if(
      RootMembers.begin(), RootMembers.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured &&
               Member.StructuredRoot == 88;
      });
  assert(StructuredIt != RootMembers.end());
  OverlayNodeKey Result = Manager.nodeKey(*StructuredIt);
  assert(Manager.representativeBlock(*StructuredIt) == 1);
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)) ==
         std::vector<OverlayNodeKey>({Result}));
  assert(Manager.sharedNodeSuccessors(Result) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(2)}));
  assert(Manager.ownerOf(1) == InvalidRegionId);

  std::vector<OverlayViewEdge> MemberEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  bool HasHiddenStructuredEdge =
      std::find_if(MemberEdges.begin(), MemberEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) == Result &&
                            Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != MemberEdges.end();
  assert(!HasHiddenStructuredEdge);

  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true);
  bool HasRemappedExtraEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return !Edge.sourcesMember() &&
                            Edge.sourceNode() == OverlayNodeKey::block(3) &&
                            Edge.targetsMember() &&
                            Manager.nodeKey(Edge.To) == Result;
                   }) != FullEdges.end();
  assert(HasRemappedExtraEdge);

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(1)}));
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(1)) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(2)}));
  assert(Manager.ownerOf(1) == RootId);
}

void testOverlayAbsorbSuccessorIntoStructuredMember() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  RootOverlay->replaceNodes({OverlayNodeKey::block(0)}, 88);
  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(RootMembers.size() == 1);
  OverlayMember Structured = RootMembers.front();
  assert(Structured.Kind == OverlayMemberKind::Structured);

  OverlayEdgeEndpoint Succ = OverlayEdgeEndpoint::external(1);
  OverlayEdgeEndpoint SuccOut = OverlayEdgeEndpoint::external(2);
  OverlayEdgeEndpoint NewNode = OverlayEdgeEndpoint::member(Structured);
  RootOverlay->addExtraFullEdge(Succ, SuccOut);
  RootOverlay->absorbSuccessorInto(Succ, NewNode);

  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true);
  bool HasAbsorbedEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) ==
                                Manager.nodeKey(Structured) &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(2);
                   }) != FullEdges.end();
  bool StillShowsOriginalSuccessor =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) ==
                                Manager.nodeKey(Structured) &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(1);
                   }) != FullEdges.end();
  assert(HasAbsorbedEdge);
  assert(!StillShowsOriginalSuccessor);
}

void testOverlayReplaceNodesAbsorbsSuccessor() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  OverlayEdgeEndpoint Succ = OverlayEdgeEndpoint::external(1);
  OverlayEdgeEndpoint SuccOut = OverlayEdgeEndpoint::external(2);
  RootOverlay->addExtraFullEdge(Succ, SuccOut);
  RootOverlay->replaceNodes({OverlayNodeKey::block(0)}, 88,
                            OverlayNodeKey::block(1));

  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(RootMembers.size() == 1);
  OverlayMember Structured = RootMembers.front();
  assert(Structured.Kind == OverlayMemberKind::Structured);

  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true);
  bool HasAbsorbedEdge =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) ==
                                Manager.nodeKey(Structured) &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(2);
                   }) != FullEdges.end();
  bool StillShowsOriginalSuccessor =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) ==
                                Manager.nodeKey(Structured) &&
                            !Edge.targetsMember() &&
                            Edge.targetNode() == OverlayNodeKey::block(1);
                   }) != FullEdges.end();
  assert(HasAbsorbedEdge);
  assert(!StillShowsOriginalSuccessor);
}

void testOverlayEdgeMarksFilterAndRemap() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();

  RootOverlay->markEdge(OverlayEdgeEndpoint::member(OverlayMember::block(0)),
                        OverlayEdgeEndpoint::member(OverlayMember::block(1)),
                        "cyclic_refinement_outgoing");
  assert(Manager.visibleSuccessors(RootId).empty());
  assert(Manager.visibleSuccessors(RootId, /*IncludeMarkedEdges=*/true)
             .empty());
  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  bool HasMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Edge.From.Kind == OverlayMemberKind::Block &&
               Edge.From.Block == 0 && Edge.targetsMember() &&
               Edge.To.Kind == OverlayMemberKind::Block && Edge.To.Block == 1;
      }) != Edges.end();
  assert(!HasMarkedEdge);
  Edges = Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false,
                                /*IncludeMarkedEdges=*/true);
  HasMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Edge.From.Kind == OverlayMemberKind::Block &&
               Edge.From.Block == 0 && Edge.targetsMember() &&
               Edge.To.Kind == OverlayMemberKind::Block && Edge.To.Block == 1;
      }) != Edges.end();
  assert(HasMarkedEdge);

  RootOverlay->dropEdgeMarksFrom(OverlayNodeKey::block(0),
                                 "cyclic_refinement_outgoing");
  Edges = Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  HasMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(), [](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Edge.From.Kind == OverlayMemberKind::Block &&
               Edge.From.Block == 0 && Edge.targetsMember() &&
               Edge.To.Kind == OverlayMemberKind::Block && Edge.To.Block == 1;
      }) != Edges.end();
  assert(HasMarkedEdge);

  RootOverlay->markEdge(OverlayEdgeEndpoint::member(OverlayMember::block(1)),
                        OverlayEdgeEndpoint::member(OverlayMember::block(2)),
                        "cyclic_refinement_outgoing");
  std::vector<OverlayViewEdge> RawFullEdges = Manager.quotientEdges(
      RootId, /*IncludeSuccessors=*/true, /*IncludeMarkedEdges=*/true);
  bool RawFullHasMarkedEdge =
      std::find_if(RawFullEdges.begin(), RawFullEdges.end(),
                   [](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Edge.From.Kind == OverlayMemberKind::Block &&
                            Edge.From.Block == 1 && Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != RawFullEdges.end();
  assert(RawFullHasMarkedEdge);
  RootOverlay->replaceNodes({OverlayNodeKey::block(1)}, 88);
  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  auto StructuredIt = std::find_if(
      RootMembers.begin(), RootMembers.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured &&
               Member.StructuredRoot == 88;
      });
  assert(StructuredIt != RootMembers.end());
  OverlayNodeKey Structured = Manager.nodeKey(*StructuredIt);
  Edges = Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  bool HasRemappedMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) == Structured &&
                            Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != Edges.end();
  assert(!HasRemappedMarkedEdge);
  Edges = Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false,
                                /*IncludeMarkedEdges=*/true);
  HasRemappedMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) == Structured &&
                            Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != Edges.end();
  assert(HasRemappedMarkedEdge);

  RootOverlay->dropEdgeMarksFrom(Structured, "cyclic_refinement_outgoing");
  Edges = Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  HasRemappedMarkedEdge =
      std::find_if(Edges.begin(), Edges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) == Structured &&
                            Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != Edges.end();
  assert(HasRemappedMarkedEdge);

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.members(RootId).size() == 3);
  assert(Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false).size() ==
         2);
}

void testPhoenixOverlayPathSyncsReducerCollapseToOverlay() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  PhoenixStructurer Structurer;
  StructuredTree Tree;
  NodeId RootNode = Structurer.structureRegion(Cfg, *RootOverlay, Tree);
  assert(RootNode != InvalidNodeId);

  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(RootMembers.size() == 1);
  assert(RootMembers.front().Kind == OverlayMemberKind::Structured);
  assert(RootMembers.front().StructuredRoot == RootNode);
  assert(Manager.representativeBlock(RootMembers.front()) == 0);
  OverlayNodeKey Result = Manager.nodeKey(RootMembers.front());
  assert(Manager.sharedNodeSuccessors(Result).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(1)).empty());
}

void testPhoenixOverlayPathSyncsRepeatedReducerCollapses() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  PhoenixStructurer Structurer;
  StructuredTree Tree;
  NodeId RootNode = Structurer.structureRegion(Cfg, *RootOverlay, Tree);
  assert(RootNode != InvalidNodeId);

  const std::vector<OverlayMember> &RootMembers = Manager.members(RootId);
  assert(RootMembers.size() == 1);
  assert(RootMembers.front().Kind == OverlayMemberKind::Structured);
  assert(RootMembers.front().StructuredRoot == RootNode);
  OverlayNodeKey Result = Manager.nodeKey(RootMembers.front());
  assert(Manager.sharedNodeSuccessors(Result).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(1)).empty());
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(2)).empty());
}

void testPhoenixOverlayLastResortDetachesVirtualizedEdge() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  GraphNodeId From = Graph.getNodeForBlock(0);
  GraphNodeId To = Graph.getNodeForBlock(1);
  assert(From != InvalidGraphNodeId);
  assert(To != InvalidGraphNodeId);
  assert(Graph.hasEdge(From, To));
  assert(Manager.sharedNodeSuccessors(OverlayNodeKey::block(0)).size() == 2);

  HintStructurer Structurer(From, To);
  StructuredTree Tree;
  assert(Structurer.lastResortRefinement(Cfg, Root, Graph, Tree, RootOverlay));

  assert(!Graph.hasEdge(From, To));
  const MutableRegionNode *Source = Graph.getNode(From);
  assert(Source != nullptr);
  assert(Source->StructuredRoot != InvalidNodeId);
  OverlayNodeKey Structured =
      OverlayNodeKey::structured(Source->StructuredRoot, RootId);
  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true,
                            /*IncludeMarkedEdges=*/true);
  bool HasRemovedEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == Structured &&
               Edge.targetsMember() && Edge.To.Kind == OverlayMemberKind::Block &&
               Edge.To.Block == 1;
      }) != Edges.end();
  bool HasKeptEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == Structured &&
               Edge.targetsMember() && Edge.To.Kind == OverlayMemberKind::Block &&
               Edge.To.Block == 2;
      }) != Edges.end();
  assert(!HasRemovedEdge);
  assert(HasKeptEdge);
}

void testOverlayBlockMemberMutationsUpdateOwnersAndViews() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  std::size_t Checkpoint = Manager.checkpoint();

  RootOverlay->addBlockMember(3);
  assert(Manager.ownerOf(3) == RootId);
  assert(Manager.members(RootId).back().Kind == OverlayMemberKind::Block);
  assert(Manager.members(RootId).back().Block == 3);
  RootOverlay->addEdge(0, 3);
  assert(Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false).size() ==
         1);

  RootOverlay->removeBlockMember(3);
  assert(Manager.ownerOf(3) == InvalidRegionId);
  assert(Manager.sharedSuccessors(0).empty());
  assert(Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false).empty());

  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  assert(Manager.ownerOf(3) == InvalidRegionId);
  assert(Manager.members(RootId).size() == 1);
}

void testChildOverlayGraphKeepsExternalFollowPlaceholder() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {0, 1}));
  Cfg.addBlock(block(1, {}));

  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  RegionOverlay *Root = Manager.root();
  assert(Root != nullptr);
  assert(Root->children().size() == 1);

  RegionOverlay *Loop = Manager.getRegion(Root->children().front());
  assert(Loop != nullptr);
  assert(Loop->kind() == RegionKind::NaturalLoop);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *Loop);
  GraphNodeId HeadId = Graph.getNodeForBlock(0);
  GraphNodeId FollowId = Graph.getNodeForBlock(1);
  const MutableRegionNode *Head = Graph.getNode(HeadId);
  const MutableRegionNode *Follow = Graph.getNode(FollowId);
  assert(Head != nullptr);
  assert(Follow != nullptr);
  assert(Follow->ExternalPlaceholder);
  assert(Head->Succs.size() == 2);
  assert(Graph.hasEdge(HeadId, HeadId));
  assert(Graph.hasEdge(HeadId, FollowId));
  assert(Head->hasExternalSuccessor(1));
}

void testFinalizedChildSnapshotAddsParentVisibleSuccessor() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {0}));
  Cfg.addBlock(block(1, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 0;
  Child.Blocks = {0};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);
  ChildOverlay->finalize(42, SuccessorSnapshot{{1}});
  OverlayNodeKey ChildKey = Manager.nodeKey(Manager.members(RootId)[0]);
  assert(Manager.sharedNodeSuccessors(ChildKey) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(1)}));

  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);

  GraphNodeId ChildNodeId = Graph.getNodeForBlock(0);
  GraphNodeId FollowNodeId = Graph.getNodeForBlock(1);
  const MutableRegionNode *ChildNode = Graph.getNode(ChildNodeId);
  const MutableRegionNode *FollowNode = Graph.getNode(FollowNodeId);
  assert(ChildNode != nullptr);
  assert(FollowNode != nullptr);
  assert(!FollowNode->ExternalPlaceholder);
  assert(ChildNode->hasExternalSuccessor(1));
  assert(Graph.hasEdge(ChildNodeId, FollowNodeId));
}

void testOverlayGraphUsesStructuredMemberSourceRegion() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1};
  RegionId ChildId = Regions.addRegion(Child);
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  Root.Children = {ChildId};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);
  ChildOverlay->finalize(42, SuccessorSnapshot{{2}});
  assert(Manager.members(RootId)[0].Kind == OverlayMemberKind::Structured);
  assert(Manager.members(RootId)[0].Region == ChildId);
  OverlayNodeKey ChildKey = Manager.nodeKey(Manager.members(RootId)[0]);
  assert(ChildKey.isStructured());
  assert(ChildKey.StructuredRoot == 42);
  assert(ChildKey.Region == ChildId);
  assert(Manager.representativeBlock(Manager.members(RootId)[0]) == 1);
  assert(Manager.sharedNodeSuccessors(ChildKey) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(2)}));
  assert(Manager.visibleSuccessors(RootId).empty());
  Manager.addNodeEdge(ChildKey, OverlayNodeKey::block(3));
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({3}));
  Manager.hideNodeEdge(RootId, ChildKey, OverlayNodeKey::block(3));
  assert(Manager.visibleSuccessors(RootId).empty());
  Manager.addNodeEdge(ChildKey, OverlayNodeKey::block(3));
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({3}));
  OverlayNodeKey ExternalStructured = OverlayNodeKey::structured(77);
  Manager.addNodeEdge(ChildKey, ExternalStructured);
  std::vector<OverlayNodeKey> NodeSuccs = Manager.visibleNodeSuccessors(RootId);
  assert(std::find(NodeSuccs.begin(), NodeSuccs.end(),
                   OverlayNodeKey::block(3)) != NodeSuccs.end());
  assert(std::find(NodeSuccs.begin(), NodeSuccs.end(),
                   ExternalStructured) != NodeSuccs.end());
  assert(Manager.visibleSuccessors(RootId) == std::vector<BlockId>({3}));
  std::vector<OverlayViewEdge> FullEdges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true);
  auto HasStructuredToFollow =
      std::find_if(FullEdges.begin(), FullEdges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.From.Kind == OverlayMemberKind::Structured &&
                            Edge.From.StructuredRoot == 42 &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != FullEdges.end();
  assert(HasStructuredToFollow);

  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  GraphNodeId ChildNodeId = Graph.getNodeForBlock(1);
  const MutableRegionNode *ChildNode = Graph.getNode(ChildNodeId);
  assert(ChildNode != nullptr);
  assert(ChildNode->StructuredRoot == 42);
  assert(ChildNode->Blocks == std::vector<BlockId>({1}));
  assert(ChildNode->SourceNodes == std::vector<OverlayNodeKey>({ChildKey}));
  assert(ChildNode->hasExternalSuccessor(2));
}

void testFinalizedChildSnapshotSkipsParentLoopHead() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {1}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Child;
  Child.Kind = RegionKind::NaturalLoop;
  Child.Head = 1;
  Child.Blocks = {1};
  RegionId ChildId = Regions.addRegion(Child);
  Region ParentLoop;
  ParentLoop.Kind = RegionKind::NaturalLoop;
  ParentLoop.Head = 0;
  ParentLoop.Blocks = {0, 1, 2};
  ParentLoop.Children = {ChildId};
  RegionId ParentLoopId = Regions.addRegion(ParentLoop);
  Regions.setRoot(ParentLoopId);

  OverlayManager Manager(std::move(Regions));
  RegionOverlay *ChildOverlay = Manager.getRegion(ChildId);
  assert(ChildOverlay != nullptr);
  ChildOverlay->finalize(42, SuccessorSnapshot{{0, 2}});
  OverlayNodeKey ChildKey = Manager.nodeKey(Manager.members(ParentLoopId)[0]);
  assert(Manager.sharedNodeSuccessors(ChildKey) ==
         std::vector<OverlayNodeKey>({OverlayNodeKey::block(2)}));

  RegionOverlay *ParentOverlay = Manager.root();
  assert(ParentOverlay != nullptr);
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *ParentOverlay);

  GraphNodeId ChildNodeId = Graph.getNodeForBlock(1);
  GraphNodeId ParentHeadId = Graph.getNodeForBlock(0);
  GraphNodeId FollowNodeId = Graph.getNodeForBlock(2);
  const MutableRegionNode *ChildNode = Graph.getNode(ChildNodeId);
  assert(ChildNode != nullptr);
  assert(!ChildNode->hasExternalSuccessor(0));
  assert(ChildNode->hasExternalSuccessor(2));
  assert(!Graph.hasEdge(ChildNodeId, ParentHeadId));
  assert(Graph.hasEdge(ChildNodeId, FollowNodeId));
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

void testRefineCyclicReducesGraphNaturalLoop() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 4}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));
  std::vector<GraphNodeId> Active = Graph.activeNodes();
  assert(Active.size() == 3);

  bool FoundLoop = false;
  for (GraphNodeId Id : Active) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::While) {
      FoundLoop = true;
      assert(RootNode->Block == 1);
      assert(Node->Succs.size() == 1);
      const MutableRegionNode *Follow = Graph.getNode(Node->Succs.front());
      assert(Follow != nullptr);
      assert(!Follow->Blocks.empty());
      assert(Follow->Blocks.front() == 4);
    }
  }
  assert(FoundLoop);
}

void testRefineCyclicOverlayMarksSuccessorBreakEdge() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 4}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  GraphNodeId BreakSourceId = Graph.getNodeForBlock(2);
  assert(BreakSourceId != InvalidGraphNodeId);

  StructuredTree Tree;
  TestPhoenixStructurer Structurer;
  assert(Structurer.refineCyclic(Cfg, Manager.regionTree(), *RootOverlay->region(),
                                 Graph, Tree, RootOverlay));

  const MutableRegionNode *BreakSource = Graph.getNode(BreakSourceId);
  assert(BreakSource != nullptr);
  assert(BreakSource->StructuredRoot != InvalidNodeId);
  assert(treeContainsKind(Tree, BreakSource->StructuredRoot,
                          StructuredNodeKind::Break));

  const std::vector<OverlayMember> &Members = Manager.members(RootId);
  auto LoopIt =
      std::find_if(Members.begin(), Members.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured;
      });
  assert(LoopIt != Members.end());
  OverlayNodeKey LoopNode = Manager.nodeKey(*LoopIt);

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true);
  bool HasFollowEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        OverlayNodeKey Target =
            Edge.targetsMember() ? Manager.nodeKey(Edge.To) : Edge.targetNode();
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == LoopNode &&
               Target == OverlayNodeKey::block(4);
      }) != Edges.end();
  assert(HasFollowEdge);
}

void testRefineCyclicRewritesStructuredSuccessorGoto() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 4}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  GraphNodeId BreakSourceId = Graph.getNodeForBlock(2);
  assert(BreakSourceId != InvalidGraphNodeId);

  StructuredTree Tree;
  StructuredNode SourceSequence;
  SourceSequence.Kind = StructuredNodeKind::Sequence;
  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = 2;
  SourceSequence.Children.push_back(Tree.addNode(std::move(Body)));
  StructuredNode GotoFollow;
  GotoFollow.Kind = StructuredNodeKind::Goto;
  GotoFollow.Target = 4;
  SourceSequence.Children.push_back(Tree.addNode(std::move(GotoFollow)));
  NodeId OriginalRoot = Tree.addNode(std::move(SourceSequence));
  Graph.setStructuredRoot(BreakSourceId, OriginalRoot);

  TestPhoenixStructurer Structurer;
  assert(Structurer.refineCyclic(Cfg, Manager.regionTree(), *RootOverlay->region(),
                                 Graph, Tree, RootOverlay));

  const MutableRegionNode *BreakSource = Graph.getNode(BreakSourceId);
  assert(BreakSource != nullptr);
  assert(BreakSource->StructuredRoot != InvalidNodeId);
  assert(BreakSource->StructuredRoot != OriginalRoot);
  assert(treeContainsKind(Tree, BreakSource->StructuredRoot,
                          StructuredNodeKind::Break));
  assert(!treeContainsGotoTarget(Tree, BreakSource->StructuredRoot, 4));
}

void testRefineCyclicDropsOverlayRefinementMarksAfterCollapse() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {0}));
  Cfg.addBlock(block(2, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  RootOverlay->markEdge(OverlayEdgeEndpoint::member(OverlayMember::block(0)),
                        OverlayEdgeEndpoint::member(OverlayMember::block(2)),
                        "cyclic_refinement_outgoing");

  StructuredTree Tree;
  TestPhoenixStructurer Structurer;
  assert(Structurer.refineCyclic(Cfg, Manager.regionTree(), *RootOverlay->region(),
                                 Graph, Tree, RootOverlay));

  const std::vector<OverlayMember> &Members = Manager.members(RootId);
  auto StructuredIt =
      std::find_if(Members.begin(), Members.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured;
      });
  assert(StructuredIt != Members.end());
  OverlayNodeKey Structured = Manager.nodeKey(*StructuredIt);

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/false);
  bool HasFollowEdge =
      std::find_if(Edges.begin(), Edges.end(),
                   [&](const OverlayViewEdge &Edge) {
                     return Edge.sourcesMember() &&
                            Manager.nodeKey(Edge.From) == Structured &&
                            Edge.targetsMember() &&
                            Edge.To.Kind == OverlayMemberKind::Block &&
                            Edge.To.Block == 2;
                   }) != Edges.end();
  assert(HasFollowEdge);
}

void testRefineCyclicMergesMultipleLatches() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {1}));
  Cfg.addBlock(block(5, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::While) {
      FoundLoop = true;
      assert(std::find(Node->Blocks.begin(), Node->Blocks.end(), 3) !=
             Node->Blocks.end());
      assert(std::find(Node->Blocks.begin(), Node->Blocks.end(), 4) !=
             Node->Blocks.end());
    }
  }
  assert(FoundLoop);
}

void testRefineCyclicVirtualizesExtraContinues() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {1}));
  Cfg.addBlock(block(5, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.virtualEdges().front().Kind == VirtualEdgeKind::Continue);
  assert(Graph.virtualEdges().front().FromBlock == 4);
  assert(Graph.virtualEdges().front().ToBlock == 1);
}

void testRefineCyclicOverlayVirtualizesExtraContinues() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {1}));
  Cfg.addBlock(block(5, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Manager.regionTree(), *RootOverlay->region(),
                                 Graph, Tree, RootOverlay));
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.virtualEdges().front().Kind == VirtualEdgeKind::Continue);
  assert(Graph.virtualEdges().front().FromBlock == 4);
  assert(Graph.virtualEdges().front().ToBlock == 1);

  const std::vector<OverlayMember> &Members = Manager.members(RootId);
  auto LoopIt =
      std::find_if(Members.begin(), Members.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured;
      });
  assert(LoopIt != Members.end());
  OverlayNodeKey LoopNode = Manager.nodeKey(*LoopIt);

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true,
                            /*IncludeMarkedEdges=*/true);
  bool HasFollowEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        OverlayNodeKey Target =
            Edge.targetsMember() ? Manager.nodeKey(Edge.To) : Edge.targetNode();
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == LoopNode &&
               Target == OverlayNodeKey::block(5);
      }) != Edges.end();
  bool HasHeadEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        OverlayNodeKey Target =
            Edge.targetsMember() ? Manager.nodeKey(Edge.To) : Edge.targetNode();
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == LoopNode &&
               Target == OverlayNodeKey::block(1);
      }) != Edges.end();
  assert(HasFollowEdge);
  assert(!HasHeadEdge);
}

void testRefineCyclicSkipsSwitchSourceContinueRewrite() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 4}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {1}));
  Cfg.addBlock(block(5, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  StructuredNode SwitchRoot;
  SwitchRoot.Kind = StructuredNodeKind::Switch;
  SwitchRoot.Block = 4;
  NodeId SwitchRootId = Tree.addNode(std::move(SwitchRoot));
  Graph.setStructuredRoot(Graph.getNodeForBlock(4), SwitchRootId);

  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.virtualEdges().front().Kind == VirtualEdgeKind::Continue);
  assert(Graph.virtualEdges().front().FromBlock == 4);
  const MutableRegionNode *Source =
      Graph.getNode(Graph.virtualEdges().front().From);
  assert(Source != nullptr);
  assert(Source->StructuredRoot == SwitchRootId);
}

void testRefineCyclicBuildsDoWhileFromLatchCondition() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {1, 3}));
  Cfg.addBlock(block(3, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::DoWhile) {
      FoundLoop = true;
      assert(RootNode->Block == 2);
      assert(Node->Succs.size() == 1);
      const MutableRegionNode *Follow = Graph.getNode(Node->Succs.front());
      assert(Follow != nullptr);
      assert(!Follow->Blocks.empty());
      assert(Follow->Blocks.front() == 3);
    }
  }
  assert(FoundLoop);
}

void testRefineCyclicPrefersDoWhileWhenLatchConditionWouldBecomeBreak() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 4}));
  Cfg.addBlock(branchBlock(2, {1, 3}));
  Cfg.addBlock(block(3, {}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::DoWhile) {
      FoundLoop = true;
      assert(RootNode->Block == 2);
      assert(Node->Succs.size() == 1);
      const MutableRegionNode *Follow = Graph.getNode(Node->Succs.front());
      assert(Follow != nullptr);
      assert(!Follow->Blocks.empty());
      assert(Follow->Blocks.front() == 3);
    }
  }
  assert(FoundLoop);
}

void testRefineCyclicBuildsDoWhileAfterIfJoin() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(branchBlock(3, {0, 4}));
  Cfg.addBlock(block(4, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::DoWhile) {
      FoundLoop = true;
      assert(RootNode->Block == 3);
      const StructuredNode *Body = Tree.getNode(RootNode->Body);
      assert(Body != nullptr);
      bool HasIf = false;
      for (NodeId Child : Body->Children) {
        const StructuredNode *ChildNode = Tree.getNode(Child);
        HasIf |= ChildNode != nullptr && ChildNode->Kind == StructuredNodeKind::If;
      }
      assert(HasIf);
    }
  }
  assert(FoundLoop);
}

void testStructureNaturalLoopKeepsDoWhileAfterIfJoin() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(branchBlock(3, {0, 4}));
  Cfg.addBlock(block(4, {}));

  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = 0;
  Loop.Latch = 3;
  Loop.Successors = {4};
  Loop.Blocks = {0, 1, 2, 3};

  StructuredTree Tree;
  PhoenixStructurer Structurer;
  NodeId Root = Structurer.structureRegion(Cfg, Loop, Tree);
  const StructuredNode *RootNode = Tree.getNode(Root);
  assert(RootNode != nullptr);
  if (RootNode->Kind == StructuredNodeKind::Sequence &&
      !RootNode->Children.empty()) {
    RootNode = Tree.getNode(RootNode->Children.front());
  }
  assert(RootNode != nullptr);
  assert(RootNode->Kind == StructuredNodeKind::DoWhile);
}

void testRefineCyclicVirtualizesNonFollowExits() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 6}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 5, 6};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.virtualEdges().front().ToBlock == 6);

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::While) {
      FoundLoop = true;
      assert(Node->Succs.size() == 1);
      const MutableRegionNode *Follow = Graph.getNode(Node->Succs.front());
      assert(Follow != nullptr);
      assert(!Follow->Blocks.empty());
      assert(Follow->Blocks.front() == 5);
    }
  }
  assert(FoundLoop);
}

void testRefineCyclicOverlayVirtualizesNonFollowExits() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(branchBlock(1, {2, 5}));
  Cfg.addBlock(branchBlock(2, {3, 6}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  RegionTree Regions;
  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 5, 6};
  RegionId RootId = Regions.addRegion(Root);
  Regions.setRoot(RootId);

  OverlayManager Manager(std::move(Regions), Cfg);
  RegionOverlay *RootOverlay = Manager.root();
  assert(RootOverlay != nullptr);

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, *RootOverlay);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Manager.regionTree(), *RootOverlay->region(),
                                 Graph, Tree, RootOverlay));
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.virtualEdges().front().ToBlock == 6);

  const std::vector<OverlayMember> &Members = Manager.members(RootId);
  auto LoopIt =
      std::find_if(Members.begin(), Members.end(), [](const OverlayMember &Member) {
        return Member.Kind == OverlayMemberKind::Structured;
      });
  assert(LoopIt != Members.end());
  OverlayNodeKey LoopNode = Manager.nodeKey(*LoopIt);

  std::vector<OverlayViewEdge> Edges =
      Manager.quotientEdges(RootId, /*IncludeSuccessors=*/true,
                            /*IncludeMarkedEdges=*/true);
  bool HasFollowEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        OverlayNodeKey Target =
            Edge.targetsMember() ? Manager.nodeKey(Edge.To) : Edge.targetNode();
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == LoopNode &&
               Target == OverlayNodeKey::block(5);
      }) != Edges.end();
  bool HasNonFollowEdge =
      std::find_if(Edges.begin(), Edges.end(), [&](const OverlayViewEdge &Edge) {
        OverlayNodeKey Target =
            Edge.targetsMember() ? Manager.nodeKey(Edge.To) : Edge.targetNode();
        return Edge.sourcesMember() && Manager.nodeKey(Edge.From) == LoopNode &&
               Target == OverlayNodeKey::block(6);
      }) != Edges.end();
  assert(HasFollowEdge);
  assert(!HasNonFollowEdge);
}

void testRefineCyclicKeepsDanglingNonFollowExit() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {3, 5}));
  Cfg.addBlock(branchBlock(3, {1, 6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {7}));
  Cfg.addBlock(block(7, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 5, 6, 7};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(!Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));
  assert(Graph.virtualEdges().empty());
}

void testRefineCyclicStructuresChildRegionMultipleSuccessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {3, 5}));
  Cfg.addBlock(branchBlock(3, {1, 6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  Region Loop;
  Loop.Kind = RegionKind::NaturalLoop;
  Loop.Head = 1;
  Loop.Blocks = {1, 2, 3, 5, 6};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Loop);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Loop, Graph, Tree));
  assert(!Graph.virtualEdges().empty());
}

void testRefineCyclicPrefersMostCommonExitAsFollow() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {3, 6}));
  Cfg.addBlock(branchBlock(3, {4, 5}));
  Cfg.addBlock(branchBlock(4, {1, 6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  Region Root;
  Root.Kind = RegionKind::Root;
  Root.Head = 0;
  Root.Blocks = {0, 1, 2, 3, 4, 5, 6};

  RegionTree Regions;
  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  StructuredTree Tree;
  TestPhoenixStructurer Structurer;

  assert(Structurer.refineCyclic(Cfg, Regions, Root, Graph, Tree));

  bool FoundLoop = false;
  for (GraphNodeId Id : Graph.activeNodes()) {
    const MutableRegionNode *Node = Graph.getNode(Id);
    assert(Node != nullptr);
    if (Node->StructuredRoot == InvalidNodeId) {
      continue;
    }
    const StructuredNode *RootNode = Tree.getNode(Node->StructuredRoot);
    assert(RootNode != nullptr);
    if (RootNode->Kind == StructuredNodeKind::InfiniteLoop) {
      FoundLoop = true;
      assert(Node->Succs.size() == 1);
      const MutableRegionNode *Follow = Graph.getNode(Node->Succs.front());
      assert(Follow != nullptr);
      assert(!Follow->Blocks.empty());
      assert(Follow->Blocks.front() == 6);
    }
  }
  assert(FoundLoop);
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

void testSAILRImprovePhoenixFlagFollowsConstructor() {
  TestSAILRStructurer Enabled;
  TestSAILRStructurer Disabled(/*ImprovePhoenix=*/false);
  assert(Enabled.useImprovedCyclicSchemas());
  assert(!Disabled.useImprovedCyclicSchemas());
}

} // namespace

int main() {
  testAcyclicDroppedEdges();
  testMutableRegionGraphCheckpointRestoresMutations();
  testEdgeVirtualizationHints();
  testOverlayVirtualizationReplacesSourceNode();
  testSwitchVirtualizationInstallsSourceRoot();
  testFallthroughVirtualizationInstallsSourceRoot();
  testStructurerRegistryNames();
  testGotoManagerCollectsSequenceGotoSources();
  testGotoManagerCollectsIfGotoSources();
  testStructuringEvaluatorCollectsGotoSummary();
  testStructuringEvaluatorRemovesEdgesForTrialOnly();
  testControlFlowStructureCounterCollectsSharedQuality();
  testRelativeQualityRejectsBackwardGotoTrade();
  testRelativeQualityRejectsMoreGotoTargets();
  testStructuringOptimizationPassAcceptsImprovedGraph();
  testStructuringOptimizationPassRejectsNewGotos();
  testStructuringOptimizationPassUsesRemovedEdgesForInitialGotos();
  testStructuringOptimizationPassRecoversAndContinuesFixedPoint();
  testRecursiveStructurerVisitsChildBeforeParent();
  testRecursiveStructurerVisitsDissolvedChildMembers();
  testGotoRegionSkipsChildBlocks();
  testVisibleRegionTreeOnlyIncludesFinalizedChildren();
  testOverlayManagerInitialMembersMatchRegionTree();
  testOverlayManagerFinalizeAndDissolveUpdateMembers();
  testOverlayManagerKeepsSharedCFGSuccessors();
  testOverlayManagerDerivesVisibleSuccessors();
  testOverlayVisibleSuccessorsMatchIdentifiedLoopSuccessors();
  testSnapshotSuccessorsFallsBackWithoutSharedCFG();
  testOverlayManagerDerivesQuotientEdges();
  testOverlayManagerQuotientKeepsBlockSelfLoop();
  testOverlayAcyclicViewsFilterBackEdgesByOrder();
  testPhoenixOverlayPathUsesOverlayNodeOrder();
  testPhoenixOverlayLastResortUsesOverlayAcyclicCandidates();
  testOverlayGraphBuildsEdgesFromQuotientView();
  testOverlayFullViewAddsLoopSuccessorEdges();
  testOverlayViewOnlyMutationsAffectQuotientEdges();
  testOverlaySharedEdgeMutationsUpdateViews();
  testOverlaySharedNodeSuccessorsCanTargetStructuredResults();
  testOverlayCollapseRegionRewiresSharedGraph();
  testOverlayReplaceNodesRewiresSharedGraphAndBookkeeping();
  testOverlayAbsorbSuccessorIntoStructuredMember();
  testOverlayReplaceNodesAbsorbsSuccessor();
  testOverlayEdgeMarksFilterAndRemap();
  testPhoenixOverlayPathSyncsReducerCollapseToOverlay();
  testPhoenixOverlayPathSyncsRepeatedReducerCollapses();
  testPhoenixOverlayLastResortDetachesVirtualizedEdge();
  testOverlayBlockMemberMutationsUpdateOwnersAndViews();
  testChildOverlayGraphKeepsExternalFollowPlaceholder();
  testFinalizedChildSnapshotAddsParentVisibleSuccessor();
  testOverlayGraphUsesStructuredMemberSourceRegion();
  testFinalizedChildSnapshotSkipsParentLoopHead();
  testMergedNaturalLoopKeepsAllLatchPaths();
  testRefineCyclicReducesGraphNaturalLoop();
  testRefineCyclicOverlayMarksSuccessorBreakEdge();
  testRefineCyclicRewritesStructuredSuccessorGoto();
  testRefineCyclicDropsOverlayRefinementMarksAfterCollapse();
  testRefineCyclicMergesMultipleLatches();
  testRefineCyclicVirtualizesExtraContinues();
  testRefineCyclicOverlayVirtualizesExtraContinues();
  testRefineCyclicSkipsSwitchSourceContinueRewrite();
  testRefineCyclicBuildsDoWhileFromLatchCondition();
  testRefineCyclicPrefersDoWhileWhenLatchConditionWouldBecomeBreak();
  testRefineCyclicBuildsDoWhileAfterIfJoin();
  testStructureNaturalLoopKeepsDoWhileAfterIfJoin();
  testRefineCyclicVirtualizesNonFollowExits();
  testRefineCyclicOverlayVirtualizesNonFollowExits();
  testRefineCyclicKeepsDanglingNonFollowExit();
  testRefineCyclicStructuresChildRegionMultipleSuccessors();
  testRefineCyclicPrefersMostCommonExitAsFollow();
  testSAILROrderPrefersLeastSiblingEdges();
  testSAILROrderPrefersMostPostDominators();
  testSAILROrderPrefersReturnTargetTieBreak();
  testSAILRImprovePhoenixFlagFollowsConstructor();
  return 0;
}
