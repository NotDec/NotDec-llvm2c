#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"
#include "notdec-backends/Structuring/RegionOverlay.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"

#include <cassert>
#include <map>
#include <memory>
#include <vector>

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
  using SAILRStructurer::useImprovedCyclicSchemas;
};

class TestPhoenixStructurer : public PhoenixStructurer {
public:
  using PhoenixStructurer::refineCyclic;
};

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
  Root.Head = 0;
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
  Root.Head = 0;
  Root.Blocks = {0, 1, 2};

  MutableRegionGraph Graph = MutableRegionGraph::build(Cfg, Root);
  std::size_t Checkpoint = Graph.checkpoint();
  Graph.virtualizeEdge(Graph.getNodeForBlock(0), Graph.getNodeForBlock(1),
                       VirtualEdgeKind::Goto);
  Graph.collapseNodes({Graph.getNodeForBlock(1), Graph.getNodeForBlock(2)}, 1,
                      42);
  assert(Graph.virtualEdges().size() == 1);
  assert(Graph.activeNodes().size() == 2);

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
  FirstChildOverlay->finalize(42, FirstChildSnapshot);
  std::vector<FinalizedChildRegion> Finalized = Manager.finalizedChildren(RootId);
  assert(Finalized.size() == 1);
  assert(Finalized.front().RegionData != nullptr);
  assert(Finalized.front().RegionData->Id == FirstChildId);
  assert(Finalized.front().StructuredRoot == 42);
  assert(Finalized.front().Snapshot.Successors == std::vector<BlockId>({3}));

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
  testSwitchVirtualizationInstallsSourceRoot();
  testFallthroughVirtualizationInstallsSourceRoot();
  testStructurerRegistryNames();
  testRecursiveStructurerVisitsChildBeforeParent();
  testGotoRegionSkipsChildBlocks();
  testVisibleRegionTreeOnlyIncludesFinalizedChildren();
  testOverlayManagerInitialMembersMatchRegionTree();
  testOverlayManagerFinalizeAndDissolveUpdateMembers();
  testChildOverlayGraphKeepsExternalFollowPlaceholder();
  testFinalizedChildSnapshotAddsParentVisibleSuccessor();
  testFinalizedChildSnapshotSkipsParentLoopHead();
  testMergedNaturalLoopKeepsAllLatchPaths();
  testRefineCyclicReducesGraphNaturalLoop();
  testRefineCyclicMergesMultipleLatches();
  testRefineCyclicVirtualizesExtraContinues();
  testRefineCyclicSkipsSwitchSourceContinueRewrite();
  testRefineCyclicBuildsDoWhileFromLatchCondition();
  testRefineCyclicPrefersDoWhileWhenLatchConditionWouldBecomeBreak();
  testRefineCyclicBuildsDoWhileAfterIfJoin();
  testStructureNaturalLoopKeepsDoWhileAfterIfJoin();
  testRefineCyclicVirtualizesNonFollowExits();
  testRefineCyclicKeepsDanglingNonFollowExit();
  testRefineCyclicStructuresChildRegionMultipleSuccessors();
  testRefineCyclicPrefersMostCommonExitAsFollow();
  testSAILROrderPrefersLeastSiblingEdges();
  testSAILROrderPrefersMostPostDominators();
  testSAILROrderPrefersReturnTargetTieBreak();
  testSAILRImprovePhoenixFlagFollowsConstructor();
  return 0;
}
