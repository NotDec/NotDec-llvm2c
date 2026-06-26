#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/GotoManager.h"
#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/LLVMFunctionCFGBuilder.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"
#include "notdec-backends/Structuring/RegionOverlay.h"
#include "notdec-backends/Structuring/SAILRDeoptimization.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"
#include "notdec-backends/Structuring/StructurerRegistry.h"
#include "notdec-backends/Structuring/StructuringEvaluator.h"
#include "notdec-backends/Structuring/StructuringOptimizationPass.h"
#include "notdec-backends/Structuring/StructuringOptimizationPipeline.h"
#include "notdec-backends/Structuring/StructuringQuality.h"
#include "notdec-backends/Solidity/BodyBuilder.h"

#include <algorithm>
#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Instructions.h>

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

class TestDuplicationReverter : public DuplicationReverter {
public:
  using DuplicationReverter::DuplicationReverter;
  using DuplicationReverter::getNewGotos;
  using DuplicationReverter::runOnGraph;
};

class TestSwitchDefaultCaseDuplicator : public SwitchDefaultCaseDuplicator {
public:
  using SwitchDefaultCaseDuplicator::SwitchDefaultCaseDuplicator;
  using SwitchDefaultCaseDuplicator::runOnGraph;
};

class TestSwitchReusedEntryRewriter : public SwitchReusedEntryRewriter {
public:
  using SwitchReusedEntryRewriter::SwitchReusedEntryRewriter;
  using SwitchReusedEntryRewriter::runOnGraph;
};

class TestLoweredSwitchSimplifier : public LoweredSwitchSimplifier {
public:
  using LoweredSwitchSimplifier::LoweredSwitchSimplifier;
  using LoweredSwitchSimplifier::runOnGraph;
};

class TestReturnDuplicatorLow : public ReturnDuplicatorLow {
public:
  using ReturnDuplicatorLow::ReturnDuplicatorLow;
  using ReturnDuplicatorLow::getNewGotos;
  using ReturnDuplicatorLow::runOnGraph;
};

class TestCrossJumpReverter : public CrossJumpReverter {
public:
  using CrossJumpReverter::CrossJumpReverter;
  using CrossJumpReverter::runOnGraph;
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

  const char *name() const override { return "RemoveFirstSuccessorPass"; }

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

  const char *name() const override { return "AddFirstSuccessorPass"; }

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

class AddFirstSuccessorIgnoringNewGotosPass : public AddFirstSuccessorPass {
public:
  using AddFirstSuccessorPass::AddFirstSuccessorPass;

protected:
  GotoManager getNewGotos(const StructuredCFG &Cfg,
                          const StructuringEvaluation &Initial,
                          const StructuringEvaluation &Current) const override {
    (void)Cfg;
    (void)Current;
    return Initial.Gotos;
  }
};

class RetargetFirstSuccessorIgnoringNewGotosPass
    : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

  const char *name() const override {
    return "RetargetFirstSuccessorIgnoringNewGotosPass";
  }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override {
    (void)Current;
    CFGBlock *Block = Graph.getBlock(0);
    if (Block == nullptr || Block->Successors != std::vector<BlockId>({2})) {
      return false;
    }
    Block->Successors = {1};
    return true;
  }

  GotoManager getNewGotos(const StructuredCFG &Cfg,
                          const StructuringEvaluation &Initial,
                          const StructuringEvaluation &Current) const override {
    (void)Cfg;
    (void)Current;
    return Initial.Gotos;
  }
};

class RecoverThenRemoveSuccessorPass : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

  const char *name() const override { return "RecoverThenRemoveSuccessorPass"; }

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

// First trial adds copied dephication state and a bad block, then the second
// trial checks that analyze() rolled everything back before retrying.
class RecoverCopiedDephicationPass : public StructuringOptimizationPass {
public:
  using StructuringOptimizationPass::StructuringOptimizationPass;

  const char *name() const override {
    return "RecoverCopiedDephicationPass";
  }

  unsigned Attempts = 0;
  BlockId CopiedEdge = InvalidBlockId;
  BlockId CopiedMerge = InvalidBlockId;

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override {
    (void)Current;
    ++Attempts;

    if (Attempts == 1) {
      std::optional<DuplicatedRegion> CopyRegion =
          Graph.duplicateRegion({1, 4, 3});
      assert(CopyRegion.has_value());
      CopiedEdge = CopyRegion->copyOf(4);
      CopiedMerge = CopyRegion->copyOf(3);
      assert(CopiedEdge != InvalidBlockId);
      assert(CopiedMerge != InvalidBlockId);
      assert(Graph.dephicationVVars().size() == 2);
      assert(Graph.dephicationIncomings().size() == 2);

      CFGBlock *Block0 = Graph.getBlock(0);
      assert(Block0 != nullptr);
      Block0->Successors = {99};
      Block0->Terminator = TerminatorKind::Fallthrough;

      CFGBlock BadBlock;
      BadBlock.Id = 99;
      BadBlock.Terminator = TerminatorKind::Return;
      Graph.addBlock(std::move(BadBlock));
      return true;
    }

    CFGBlock *Block0 = Graph.getBlock(0);
    if (Block0 == nullptr || Block0->Successors != std::vector<BlockId>({1})) {
      return false;
    }
    assert(Graph.getBlock(99) == nullptr);
    assert(Graph.getBlock(CopiedEdge) == nullptr);
    assert(Graph.getBlock(CopiedMerge) == nullptr);
    assert(Graph.dephicationVVars().size() == 1);
    assert(Graph.dephicationIncomings().size() == 1);

    Block0->Successors.clear();
    Block0->Terminator = TerminatorKind::Return;
    return true;
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

bool hasSinglePayload(const std::vector<PayloadRef> &Refs, PayloadId Id) {
  return Refs.size() == 1 && Refs.front().Id == Id;
}

bool containsLineSubstring(const std::vector<std::string> &Lines,
                           llvm::StringRef Needle) {
  return std::find_if(Lines.begin(), Lines.end(), [&](const std::string &Line) {
           return Line.find(Needle.str()) != std::string::npos;
         }) != Lines.end();
}

PayloadRef addStringPayload(std::vector<std::string> &Payloads,
                            std::string Text) {
  Payloads.push_back(std::move(Text));
  return PayloadRef{Payloads.size() - 1};
}

std::string testValueName(const llvm::Value &V, llvm::StringRef Fallback) {
  if (V.hasName()) {
    return V.getName().str();
  }
  return Fallback.str();
}

class StringPayloadProvider : public LLVMFunctionCFGBuilder::PayloadProvider {
public:
  explicit StringPayloadProvider(std::vector<std::string> &Payloads)
      : Payloads(Payloads) {}

  void collectStatements(const llvm::BasicBlock &BB,
                         std::vector<PayloadRef> &Out) override {
    (void)BB;
    (void)Out;
  }

  PayloadRef getCondition(const llvm::Value &V,
                          llvm::StringRef FallbackName) override {
    return addStringPayload(Payloads, testValueName(V, FallbackName));
  }

  PayloadRef getSwitchCase(const llvm::ConstantInt &V) override {
    return addStringPayload(Payloads, std::to_string(V.getZExtValue()));
  }

  PayloadRef getPhiAssignment(const llvm::PHINode &Phi,
                              const llvm::Value &IncomingValue,
                              llvm::StringRef PhiName,
                              llvm::StringRef IncomingName) override {
    (void)Phi;
    (void)IncomingValue;
    return addStringPayload(Payloads,
                            (PhiName + " = " + IncomingName + ";").str());
  }

private:
  std::vector<std::string> &Payloads;
};

llvm::Function *makeSharedPhiFunction(llvm::LLVMContext &Context,
                                      llvm::Module &Module) {
  llvm::Type *I1Ty = llvm::Type::getInt1Ty(Context);
  llvm::Type *I32Ty = llvm::Type::getInt32Ty(Context);
  auto *FnTy =
      llvm::FunctionType::get(I32Ty, {I1Ty, I32Ty, I32Ty, I32Ty, I32Ty},
                              /*isVarArg=*/false);
  auto *F = llvm::Function::Create(FnTy, llvm::GlobalValue::ExternalLinkage,
                                   "phi_merge", Module);
  auto ArgIt = F->arg_begin();
  llvm::Value *Cond = &*ArgIt++;
  Cond->setName("cond");
  llvm::Value *A = &*ArgIt++;
  A->setName("a");
  llvm::Value *B = &*ArgIt++;
  B->setName("b");
  llvm::Value *C = &*ArgIt++;
  C->setName("c");
  llvm::Value *D = &*ArgIt++;
  D->setName("d");

  llvm::BasicBlock *Entry = llvm::BasicBlock::Create(Context, "entry", F);
  llvm::BasicBlock *Then = llvm::BasicBlock::Create(Context, "then", F);
  llvm::BasicBlock *Else = llvm::BasicBlock::Create(Context, "else", F);
  llvm::BasicBlock *Merge = llvm::BasicBlock::Create(Context, "merge", F);

  llvm::IRBuilder<> Builder(Context);
  Builder.SetInsertPoint(Entry);
  Builder.CreateCondBr(Cond, Then, Else);

  Builder.SetInsertPoint(Then);
  Builder.CreateBr(Merge);

  Builder.SetInsertPoint(Else);
  Builder.CreateBr(Merge);

  Builder.SetInsertPoint(Merge);
  auto *Phi = Builder.CreatePHI(I32Ty, 2, "x");
  Phi->addIncoming(A, Then);
  Phi->addIncoming(B, Else);
  auto *Phi2 = Builder.CreatePHI(I32Ty, 2, "y");
  Phi2->addIncoming(C, Then);
  Phi2->addIncoming(D, Else);
  Builder.CreateRet(Phi);

  return F;
}

llvm::Function *makeSharedPhiSwitchFunction(llvm::LLVMContext &Context,
                                            llvm::Module &Module) {
  llvm::IntegerType *I32Ty = llvm::Type::getInt32Ty(Context);
  auto *FnTy = llvm::FunctionType::get(I32Ty, {I32Ty, I32Ty, I32Ty},
                                       /*isVarArg=*/false);
  auto *F = llvm::Function::Create(FnTy, llvm::GlobalValue::ExternalLinkage,
                                   "phi_switch", Module);
  auto ArgIt = F->arg_begin();
  llvm::Value *X = &*ArgIt++;
  X->setName("x");
  llvm::Value *A = &*ArgIt++;
  A->setName("a");
  llvm::Value *B = &*ArgIt++;
  B->setName("b");

  llvm::BasicBlock *Entry = llvm::BasicBlock::Create(Context, "entry", F);
  llvm::BasicBlock *Case1 = llvm::BasicBlock::Create(Context, "case1", F);
  llvm::BasicBlock *Case2 = llvm::BasicBlock::Create(Context, "case2", F);
  llvm::BasicBlock *Default = llvm::BasicBlock::Create(Context, "default", F);
  llvm::BasicBlock *Shared = llvm::BasicBlock::Create(Context, "shared", F);

  llvm::IRBuilder<> Builder(Context);
  Builder.SetInsertPoint(Entry);
  llvm::SwitchInst *Switch = Builder.CreateSwitch(X, Default, 2);
  Switch->addCase(llvm::ConstantInt::get(I32Ty, 1), Case1);
  Switch->addCase(llvm::ConstantInt::get(I32Ty, 2), Case2);

  Builder.SetInsertPoint(Case1);
  Builder.CreateBr(Shared);

  Builder.SetInsertPoint(Case2);
  Builder.CreateBr(Shared);

  Builder.SetInsertPoint(Default);
  Builder.CreateRet(llvm::ConstantInt::get(I32Ty, 0));

  Builder.SetInsertPoint(Shared);
  auto *Phi = Builder.CreatePHI(I32Ty, 2, "p");
  Phi->addIncoming(A, Case1);
  Phi->addIncoming(B, Case2);
  Builder.CreateRet(Phi);

  return F;
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

void testGotoManagerCollectsSwitchGotoEdgeKinds() {
  StructuredTree Tree;

  StructuredNode DefaultGoto;
  DefaultGoto.Kind = StructuredNodeKind::Goto;
  DefaultGoto.Target = 20;
  NodeId DefaultId = Tree.addNode(std::move(DefaultGoto));

  StructuredNode CaseGoto;
  CaseGoto.Kind = StructuredNodeKind::Goto;
  CaseGoto.Target = 30;
  NodeId CaseId = Tree.addNode(std::move(CaseGoto));

  StructuredNode Switch;
  Switch.Kind = StructuredNodeKind::Switch;
  Switch.Block = 10;
  Switch.Default = DefaultId;
  Switch.StructuredCases.push_back({{}, 30, CaseId});
  Tree.setRoot(Tree.addNode(std::move(Switch)));

  GotoManager Manager = GotoManager::collect(Tree);
  assert(Manager.size() == 2);

  std::vector<StructuredGoto> Gotos = Manager.gotosInBlock(10);
  assert(Gotos.size() == 1);
  bool SawDefault = false;
  for (const StructuredGoto &Goto : Gotos) {
    if (Goto.Target == 20) {
      SawDefault = Goto.EdgeKind == StructuredGotoEdgeKind::SwitchDefault;
    }
  }
  assert(SawDefault);

  Gotos = Manager.gotosInBlock(30);
  assert(Gotos.size() == 1);
  bool SawCase = false;
  for (const StructuredGoto &Goto : Gotos) {
    if (Goto.Target == 30) {
      SawCase = Goto.EdgeKind == StructuredGotoEdgeKind::SwitchCase;
    }
  }
  assert(SawCase);
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

void testStructuredCFGDuplicatesBlockBodySource() {
  StructuredCFG Cfg;
  CFGBlock Source = block(10, {11});
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  assert(Cfg.getBlock(10)->BodyMaterialized);

  BlockId CopyId = Cfg.duplicateBlock(10, {11});
  assert(CopyId == 12);

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == 10);
  assert(Copy->CopiedFromBlock == 10);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(!Copy->BodyMaterialized);
  assert(Copy->BodyBlock == 10);
  assert(Cfg.bodyBlock(CopyId) == 10);
  assert(Cfg.getBodyBlock(CopyId) == Cfg.getBlock(10));
  assert(Copy->Statements.empty());
  assert((Copy->Successors == std::vector<BlockId>{11}));

  BlockId SecondCopyId = Cfg.duplicateBlock(CopyId, {});
  const CFGBlock *SecondCopy = Cfg.getBlock(SecondCopyId);
  assert(SecondCopy != nullptr);
  assert(SecondCopy->Origin == CFGBlockOrigin::Copied);
  assert(SecondCopy->SourceBlock == 10);
  assert(SecondCopy->CopiedFromBlock == CopyId);
  assert(SecondCopy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(!SecondCopy->BodyMaterialized);
  assert(SecondCopy->BodyBlock == 10);
  assert(Cfg.bodyBlock(SecondCopyId) == 10);

  assert(Cfg.materializeBlockBody(CopyId));
  Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(hasSinglePayload(Copy->Statements, 7));
}

void testStructuredCFGDuplicateCopyKeepsOriginalBodySource() {
  StructuredCFG Cfg;
  CFGBlock Source = block(10, {11});
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));

  BlockId CopyId = Cfg.duplicateBlock(10, {11});
  assert(CopyId == 12);
  BlockId CopyCopyId = Cfg.duplicateBlock(CopyId, {});
  assert(CopyCopyId == 13);

  const CFGBlock *CopyCopy = Cfg.getBlock(CopyCopyId);
  assert(CopyCopy != nullptr);
  assert(CopyCopy->Origin == CFGBlockOrigin::Copied);
  assert(CopyCopy->SourceBlock == 10);
  assert(CopyCopy->CopiedFromBlock == CopyId);
  assert(CopyCopy->BodyBlock == 10);
  assert(Cfg.bodyBlock(CopyCopyId) == 10);

  assert(Cfg.materializeBlockBody(CopyCopyId));
  CopyCopy = Cfg.getBlock(CopyCopyId);
  assert(CopyCopy != nullptr);
  assert(CopyCopy->BodyMaterialized);
  assert(CopyCopy->BodyBlock == CopyCopyId);
  assert(hasSinglePayload(CopyCopy->Statements, 7));
}

void testStructuredCFGMaterializeRewritesCopiedPayloads() {
  StructuredCFG Cfg;
  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Statements.push_back({7});
  Source.Statements.push_back({8});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  std::size_t StatementRewrites = 0;
  std::size_t ConditionRewrites = 0;
  std::size_t CaseRewrites = 0;
  std::vector<PayloadRef> CommittedPayloads;
  std::size_t AbortNotifications = 0;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t Index) -> std::optional<PayloadRef> {
        assert(Context.SourceBlock == 10);
        assert(Context.BodyBlock == 10);
        assert(Context.CopyBlock == 13);
        assert(Context.CopiedFromBlock == 10);
        assert(Context.OriginalPredecessor == 90);
        assert(Context.NewPredecessor == 91);
        assert(Context.OriginalCases.size() == 1);
        assert(Context.NewCases.size() == 1);
        assert(Context.OriginalCases.front().Value.Id == 71);
        assert(Context.NewCases.front().Value.Id == InvalidPayloadId);
        assert(Context.OriginalCases.front().Target == 12);
        assert(Context.NewCases.front().Target == 12);
        assert(Context.OriginalSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.NewSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.OriginalTerminator == TerminatorKind::Switch);
        assert(Context.NewTerminator == TerminatorKind::Switch);
        assert(Context.CopyKind == CFGBlockCopyKind::RegionCopy);
        assert(Context.CreatedBy == CFGBlockCreator::SAILRDeoptimization);

        if (Kind == PayloadMaterializeKind::Statement) {
          ++StatementRewrites;
          return PayloadRef{Payload.Id + 1000 + Index};
        }
        if (Kind == PayloadMaterializeKind::Condition) {
          ++ConditionRewrites;
          return PayloadRef{Payload.Id + 2000};
        }

        assert(Kind == PayloadMaterializeKind::SwitchCaseValue);
        ++CaseRewrites;
        return PayloadRef{Payload.Id + 3000 + Index};
      });
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.CopyBlock == 13);
        assert(Context.OriginalTarget == InvalidBlockId);
        assert(Context.NewTarget == InvalidBlockId);
        if (Result == PayloadMaterializeResult::Committed) {
          CommittedPayloads = Payloads;
        } else {
          ++AbortNotifications;
        }
      });

  BlockId CopyId = Cfg.duplicateBlock(10, {11, 12});
  assert(CopyId == 13);
  assert(Cfg.materializeBlockBody(CopyId, 90, 91));

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == 10);
  assert(Copy->CopiedFromBlock == 10);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert((Copy->Successors == std::vector<BlockId>{11, 12}));
  assert(Copy->Statements.size() == 2);
  assert(Copy->Statements[0].Id == 1007);
  assert(Copy->Statements[1].Id == 1009);
  assert(Copy->Condition.Id == 2070);
  assert(Copy->Cases.size() == 1);
  assert(Copy->Cases.front().Value.Id == 3071);
  assert(Copy->Cases.front().Target == 12);
  assert(StatementRewrites == 2);
  assert(ConditionRewrites == 1);
  assert(CaseRewrites == 1);
  assert(AbortNotifications == 0);
  assert(CommittedPayloads.size() == 4);
  assert(CommittedPayloads[0].Id == 1007);
  assert(CommittedPayloads[1].Id == 1009);
  assert(CommittedPayloads[2].Id == 2070);
  assert(CommittedPayloads[3].Id == 3071);
}

void testStructuredCFGMaterializeFastPathReportsCommit() {
  StructuredCFG Cfg;
  CFGBlock Source = block(10, {11});
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));

  bool SawCommit = false;
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.CopyBlock == 12);
        assert(Result == PayloadMaterializeResult::Committed);
        assert(Payloads.empty());
        SawCommit = true;
      });

  BlockId CopyId = Cfg.duplicateBlock(10, {11});
  assert(CopyId == 12);
  assert(Cfg.materializeBlockBody(CopyId));
  assert(SawCommit);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->BodyMaterialized);
}

void testStructuredCFGMaterializeFastPathKeepsCopiedSwitchIdentity() {
  StructuredCFG Cfg;
  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Statements.push_back({7});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  BlockId CopyId = Cfg.duplicateBlock(10, {11, 12});
  assert(CopyId == 13);
  assert(Cfg.materializeBlockBody(CopyId));

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == 10);
  assert(Copy->CopiedFromBlock == 10);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->Terminator == TerminatorKind::Switch);
  assert(Copy->Condition.Id == 70);
  assert(Copy->Cases.size() == 1);
  assert(Copy->Cases.front().Value.Id == 71);
  assert(Copy->Cases.front().Target == 12);
}

void testStructuredCFGMaterializeSelfReportsFullContext() {
  StructuredCFG Cfg;
  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Statements.push_back({7});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  bool SawCommit = false;
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.SourceBlock == 10);
        assert(Context.BodyBlock == 10);
        assert(Context.CopyBlock == 10);
        assert(Context.CopiedFromBlock == InvalidBlockId);
        assert(Context.OriginalCases.size() == 1);
        assert(Context.NewCases.size() == 1);
        assert(Context.OriginalSuccessors ==
               std::vector<BlockId>({11, 12}));
        assert(Context.NewSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.OriginalTerminator == TerminatorKind::Switch);
        assert(Context.NewTerminator == TerminatorKind::Switch);
        assert(Result == PayloadMaterializeResult::Committed);
        assert(Payloads.empty());
        SawCommit = true;
      });

  assert(Cfg.materializeBlockBody(10));
  assert(SawCommit);
  const CFGBlock *Block = Cfg.getBlock(10);
  assert(Block != nullptr);
  assert(Block->BodyMaterialized);
}

void testStructuredCFGMaterializeReportsGroupedPredecessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {10}));
  Cfg.addBlock(block(2, {10}));

  CFGBlock Source = block(10, {});
  Source.Terminator = TerminatorKind::Return;
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));

  BlockId CopyId = Cfg.duplicateBlock(10, {});
  assert(CopyId != InvalidBlockId);

  bool SawGroupedPreds = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement) {
          assert(Context.OriginalPredecessor == 1);
          assert(Context.NewPredecessor == 101);
          assert(Context.OriginalPredecessors == std::vector<BlockId>({1, 2}));
          assert(Context.NewPredecessors ==
                 std::vector<BlockId>({101, 102}));
          SawGroupedPreds = true;
          return PayloadRef{Payload.Id + Context.NewPredecessors.size()};
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  assert(Cfg.hasPredecessorRewritePayloadMaterializeHook());
  assert(Cfg.hasGroupedPredecessorRewritePayloadMaterializeHook());
  assert(Cfg.materializeBlockBody(CopyId, {1, 2}, {101, 102}));

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(SawGroupedPreds);
  assert(hasSinglePayload(Copy->Statements, 9));
}

void testStructuredCFGMaterializeRewriteFailureIsAtomic() {
  StructuredCFG Cfg;
  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Statements.push_back({7});
  Source.Statements.push_back({8});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  std::vector<PayloadRef> AbortedPayloads;
  std::size_t CommitNotifications = 0;
  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &, PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement && Index == 1) {
          return std::nullopt;
        }
        return PayloadRef{Payload.Id + 1000};
      });
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.CopyBlock == 13);
        if (Result == PayloadMaterializeResult::Aborted) {
          AbortedPayloads = Payloads;
        } else {
          ++CommitNotifications;
        }
      });

  BlockId CopyId = Cfg.duplicateBlock(10, {11, 12});
  assert(CopyId == 13);
  assert(!Cfg.materializeBlockBody(CopyId));

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(!Copy->BodyMaterialized);
  assert(Copy->BodyBlock == 10);
  assert(Copy->Statements.empty());
  assert(Copy->Condition.Id == InvalidPayloadId);
  assert(Copy->Cases.size() == 1);
  assert(Copy->Cases.front().Value.Id == InvalidPayloadId);
  assert(Copy->Cases.front().Target == 12);
  assert(CommitNotifications == 0);
  assert(AbortedPayloads.size() == 1);
  assert(AbortedPayloads.front().Id == 1007);
}

void testGotoStructurerRendersVirtualBlockBodySource() {
  StructuredCFG Cfg;
  CFGBlock Source = block(10, {});
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));

  CFGBlock Virtual = block(20, {});
  Virtual.BodyBlock = 10;
  Cfg.addBlock(std::move(Virtual));

  StructuredTree Tree = GotoStructurer().structure(Cfg);
  bool FoundVirtualBody = false;
  for (const StructuredNode &Node : Tree.nodes()) {
    if (Node.Kind == StructuredNodeKind::BasicBlock && Node.Block == 20) {
      FoundVirtualBody = true;
      assert(hasSinglePayload(Node.Statements, 7));
    }
  }
  assert(FoundVirtualBody);
}

void testGotoStructurerRendersSyntheticForwarder() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(10, {}));
  BlockId Synthetic = Cfg.createSyntheticBlock({10});

  StructuredTree Tree = GotoStructurer().structure(Cfg);
  bool FoundSyntheticBody = false;
  for (const StructuredNode &Node : Tree.nodes()) {
    if (Node.Kind == StructuredNodeKind::BasicBlock &&
        Node.Block == Synthetic) {
      FoundSyntheticBody = true;
      assert(Node.Statements.empty());
    }
  }

  assert(FoundSyntheticBody);
}

void testGotoStructurerRendersSyntheticGoto() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(10, {}));
  BlockId Synthetic = Cfg.createSyntheticGotoEdge(20, 10);

  StructuredTree Tree = GotoStructurer().structure(Cfg);
  bool FoundSyntheticBody = false;
  std::size_t SyntheticGotoCount = 0;
  for (const StructuredNode &Node : Tree.nodes()) {
    if (Node.Kind == StructuredNodeKind::BasicBlock &&
        Node.Block == Synthetic) {
      FoundSyntheticBody = true;
      assert(Node.Statements.empty());
    }
    if (Node.Kind == StructuredNodeKind::Goto && Node.Target == 10) {
      ++SyntheticGotoCount;
    }
  }

  assert(FoundSyntheticBody);
  assert(SyntheticGotoCount == 1);
}

void testPhoenixStructurerRendersElseIfScope() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  CFGBlock Then = block(1, {3});
  Then.Statements.push_back({41});
  Cfg.addBlock(std::move(Then));

  CFGBlock Else = block(2, {3});
  Else.Statements.push_back({42});
  Cfg.addBlock(std::move(Else));

  CFGBlock Merge = block(3, {});
  Merge.Terminator = TerminatorKind::Return;
  Merge.Statements.push_back({43});
  Cfg.addBlock(std::move(Merge));

  StructuredTree Tree = PhoenixStructurer().structure(Cfg);
  bool FoundIf = false;
  bool FoundThen = false;
  bool FoundElse = false;
  for (const StructuredNode &Node : Tree.nodes()) {
    if (Node.Kind == StructuredNodeKind::If) {
      FoundIf = true;
    } else if (Node.Kind == StructuredNodeKind::BasicBlock && Node.Block == 1) {
      FoundThen = hasSinglePayload(Node.Statements, 41);
    } else if (Node.Kind == StructuredNodeKind::BasicBlock && Node.Block == 2) {
      FoundElse = hasSinglePayload(Node.Statements, 42);
    }
  }

  assert(FoundIf);
  assert(FoundThen);
  assert(FoundElse);
}

void testSolidityBodyBuilderRendersVirtualBlockBodySource() {
  std::vector<std::string> Payloads = {"copy-body"};
  StructuredTree Tree;

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Label;
  Label.Kind = StructuredNodeKind::Label;
  Label.Block = 20;
  Root.Children.push_back(Tree.addNode(std::move(Label)));

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = 20;
  Body.Statements.push_back({0});
  Root.Children.push_back(Tree.addNode(std::move(Body)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  std::vector<std::string> Out =
      notdec::backend::solidity::BodyBuilder::renderStructuredBody(Tree, Payloads);
  assert(std::find(Out.begin(), Out.end(), "// block_20:") != Out.end());
  assert(std::find(Out.begin(), Out.end(), "copy-body") != Out.end());
}

void testSolidityBodyBuilderRendersSyntheticForwarder() {
  std::vector<std::string> Payloads;
  StructuredTree Tree;

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Label;
  Label.Kind = StructuredNodeKind::Label;
  Label.Block = 20;
  Root.Children.push_back(Tree.addNode(std::move(Label)));

  StructuredNode Body;
  Body.Kind = StructuredNodeKind::BasicBlock;
  Body.Block = 20;
  Root.Children.push_back(Tree.addNode(std::move(Body)));

  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 10;
  Root.Children.push_back(Tree.addNode(std::move(Goto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  std::vector<std::string> Out =
      notdec::backend::solidity::BodyBuilder::renderStructuredBody(Tree, Payloads);
  assert(std::find(Out.begin(), Out.end(), "// block_20:") != Out.end());
  assert(std::find(Out.begin(), Out.end(), "// goto block_10") != Out.end());
  assert(std::find(Out.begin(), Out.end(), "unknown") == Out.end());
}

void testSolidityBodyBuilderConsumesStructuredSyntheticGoto() {
  std::vector<std::string> Payloads;
  StructuredCFG Cfg;
  Cfg.addBlock(block(10, {}));
  BlockId Synthetic = Cfg.createSyntheticGoto(20, 10);

  StructuredTree Tree = GotoStructurer().structure(Cfg);
  std::vector<std::string> Out =
      notdec::backend::solidity::BodyBuilder::renderStructuredBody(Tree,
                                                                   Payloads);

  assert(std::find(Out.begin(), Out.end(),
                   "// block_" + std::to_string(Synthetic) + ":") !=
         Out.end());
  assert(std::find(Out.begin(), Out.end(), "// goto block_10") != Out.end());
  assert(std::find(Out.begin(), Out.end(), "unknown") == Out.end());
}

void testLLVMFunctionCFGBuilderMaterializesPhiEdgePayloads() {
  llvm::LLVMContext Context;
  llvm::Module Module("shared-phi-test", Context);
  llvm::Function *F = makeSharedPhiFunction(Context, Module);

  std::vector<std::string> Payloads;
  StringPayloadProvider Provider(Payloads);
  StructuredCFG Cfg = LLVMFunctionCFGBuilder::build(*F, Provider);

  assert(Cfg.hasEdge(1, 4));
  assert(Cfg.hasEdge(2, 5));
  assert(!Cfg.hasEdge(1, 3));
  assert(!Cfg.hasEdge(2, 3));

  const CFGBlock *ThenEdge = Cfg.getBlock(4);
  const CFGBlock *ElseEdge = Cfg.getBlock(5);
  assert(ThenEdge != nullptr);
  assert(ElseEdge != nullptr);
  assert(ThenEdge->Origin == CFGBlockOrigin::Synthetic);
  assert(ElseEdge->Origin == CFGBlockOrigin::Synthetic);
  assert(ThenEdge->CreatedBy == CFGBlockCreator::SAILRDephication);
  assert(ElseEdge->CreatedBy == CFGBlockCreator::SAILRDephication);
  assert(ThenEdge->SyntheticSource == 1);
  assert(ThenEdge->SyntheticTarget == 3);
  assert(ElseEdge->SyntheticSource == 2);
  assert(ElseEdge->SyntheticTarget == 3);
  assert(ThenEdge->Successors == std::vector<BlockId>({3}));
  assert(ElseEdge->Successors == std::vector<BlockId>({3}));
  assert(ThenEdge->Statements.size() == 2);
  assert(ElseEdge->Statements.size() == 2);
  assert(Payloads[ThenEdge->Statements[0].Id] == "x = a;");
  assert(Payloads[ThenEdge->Statements[1].Id] == "y = c;");
  assert(Payloads[ElseEdge->Statements[0].Id] == "x = b;");
  assert(Payloads[ElseEdge->Statements[1].Id] == "y = d;");

  const std::vector<DephicationVVar> &VVars = Cfg.dephicationVVars();
  const std::vector<DephicationIncoming> &Incomings =
      Cfg.dephicationIncomings();
  assert(VVars.size() == 2);
  assert(VVars[0].Id == 0);
  assert(VVars[0].Name == "x");
  assert(VVars[0].MergeBlock == 3);
  assert(VVars[1].Id == 1);
  assert(VVars[1].Name == "y");
  assert(VVars[1].MergeBlock == 3);
  assert(Incomings.size() == 4);
  assert(Incomings[0].Target == 0);
  assert(Incomings[0].SourceTarget == 0);
  assert(Incomings[0].IncomingBlock == 1);
  assert(Incomings[0].MergeBlock == 3);
  assert(Incomings[0].EdgeBlock == 4);
  assert(Incomings[0].Assignment.Id == ThenEdge->Statements[0].Id);
  assert(Incomings[0].IncomingName == "a");
  assert(Incomings[1].Target == 1);
  assert(Incomings[1].SourceTarget == 1);
  assert(Incomings[1].IncomingBlock == 1);
  assert(Incomings[1].MergeBlock == 3);
  assert(Incomings[1].EdgeBlock == 4);
  assert(Incomings[1].Assignment.Id == ThenEdge->Statements[1].Id);
  assert(Incomings[1].IncomingName == "c");
  assert(Incomings[2].Target == 0);
  assert(Incomings[2].SourceTarget == 0);
  assert(Incomings[2].IncomingBlock == 2);
  assert(Incomings[2].MergeBlock == 3);
  assert(Incomings[2].EdgeBlock == 5);
  assert(Incomings[2].Assignment.Id == ElseEdge->Statements[0].Id);
  assert(Incomings[2].IncomingName == "b");
  assert(Incomings[3].Target == 1);
  assert(Incomings[3].SourceTarget == 1);
  assert(Incomings[3].IncomingBlock == 2);
  assert(Incomings[3].MergeBlock == 3);
  assert(Incomings[3].EdgeBlock == 5);
  assert(Incomings[3].Assignment.Id == ElseEdge->Statements[1].Id);
  assert(Incomings[3].IncomingName == "d");
}

void testSolidityBodyBuilderReadsSharedPhiAssignments() {
  llvm::LLVMContext Context;
  llvm::Module Module("solidity-shared-phi-test", Context);
  llvm::Function *F = makeSharedPhiFunction(Context, Module);

  std::vector<std::string> Out =
      notdec::backend::solidity::BodyBuilder::readBody(*F);

  assert(containsLineSubstring(Out, "x = a;"));
  assert(containsLineSubstring(Out, "x = b;"));
}

void testSolidityBodyBuilderReadsCopiedSharedPhiAssignments() {
  llvm::LLVMContext Context;
  llvm::Module Module("solidity-copied-shared-phi-test", Context);
  llvm::Function *F = makeSharedPhiSwitchFunction(Context, Module);

  std::vector<std::string> Out =
      notdec::backend::solidity::BodyBuilder::readBody(*F);

  assert(containsLineSubstring(Out, "p_copy1 = a;"));
  assert(containsLineSubstring(Out, "p = b;"));
  assert(containsLineSubstring(Out, "return p_copy1;"));
  assert(containsLineSubstring(Out, "return p;"));
}

void testSolidityBodyBuilderRewritesCopiedDephicationVVars() {
  using notdec::backend::solidity::BodyBuilder;

  std::vector<std::pair<std::string, std::string>> Copies = {{"p", "p_copy1"}};
  assert(BodyBuilder::rewriteCopiedDephicationVVars("p = a;", Copies) ==
         "p_copy1 = a;");
  assert(BodyBuilder::rewriteCopiedDephicationVVars("if (p) {", Copies) ==
         "if (p_copy1) {");
  assert(BodyBuilder::rewriteCopiedDephicationVVars("panic = p_copy0;", Copies) ==
         "panic = p_copy0;");
}

void testStructuredCFGRemoveBlockMaterializesCopiedBody() {
  StructuredCFG Cfg;
  CFGBlock Source = block(10, {});
  Source.Statements.push_back({7});
  Cfg.addBlock(std::move(Source));

  BlockId CopyId = Cfg.duplicateBlock(10, {});
  assert(CopyId != InvalidBlockId);
  assert(Cfg.removeBlock(10));

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == 10);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Cfg.getBodyBlock(CopyId) == Copy);
  assert(hasSinglePayload(Copy->Statements, 7));
}

void testStructuredCFGRemoveBlockRejectsUnmaterializedCopy() {
  StructuredCFG Cfg;

  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  CFGBlock Copy = switchBlock(20, {11});
  Copy.Origin = CFGBlockOrigin::Copied;
  Copy.SourceBlock = 10;
  Copy.CopyKind = CFGBlockCopyKind::RegionCopy;
  Copy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  Copy.BodyBlock = 10;
  Copy.BodyMaterialized = false;
  Cfg.addBlock(std::move(Copy));

  assert(!Cfg.removeBlock(10));

  const CFGBlock *SourceBlock = Cfg.getBlock(10);
  const CFGBlock *CopyBlock = Cfg.getBlock(20);
  assert(SourceBlock != nullptr && CopyBlock != nullptr);
  assert(CopyBlock->BodyBlock == 10);
  assert(!CopyBlock->BodyMaterialized);
  assert(CopyBlock->Successors == std::vector<BlockId>{11});
}

void testStructuredCFGRemoveBlockIsAtomicOnMaterializeFailure() {
  StructuredCFG Cfg;

  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Statements.push_back({70});
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  CFGBlock GoodCopy = switchBlock(20, {11, 12});
  GoodCopy.Origin = CFGBlockOrigin::Copied;
  GoodCopy.SourceBlock = 10;
  GoodCopy.CopyKind = CFGBlockCopyKind::RegionCopy;
  GoodCopy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  GoodCopy.BodyBlock = 10;
  GoodCopy.BodyMaterialized = false;
  Cfg.addBlock(std::move(GoodCopy));

  CFGBlock BadCopy = switchBlock(30, {11, 12});
  BadCopy.Origin = CFGBlockOrigin::Copied;
  BadCopy.SourceBlock = 10;
  BadCopy.CopyKind = CFGBlockCopyKind::RegionCopy;
  BadCopy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  BadCopy.BodyBlock = 10;
  BadCopy.BodyMaterialized = false;
  Cfg.addBlock(std::move(BadCopy));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Context.CopyBlock == 30 &&
            Kind == PayloadMaterializeKind::Statement) {
          return std::nullopt;
        }
        return Payload;
      });

  assert(!Cfg.removeBlock(10));

  const CFGBlock *SourceBlock = Cfg.getBlock(10);
  const CFGBlock *GoodCopyBlock = Cfg.getBlock(20);
  const CFGBlock *BadCopyBlock = Cfg.getBlock(30);
  assert(SourceBlock != nullptr && GoodCopyBlock != nullptr &&
         BadCopyBlock != nullptr);
  assert(hasSinglePayload(SourceBlock->Statements, 70));
  assert(!GoodCopyBlock->BodyMaterialized);
  assert(GoodCopyBlock->BodyBlock == 10);
  assert(GoodCopyBlock->Statements.empty());
  assert(GoodCopyBlock->Condition.Id == InvalidPayloadId);
  assert(!BadCopyBlock->BodyMaterialized);
  assert(BadCopyBlock->BodyBlock == 10);
  assert(BadCopyBlock->Statements.empty());
  assert(BadCopyBlock->Condition.Id == InvalidPayloadId);
}

void testStructuredCFGRemoveBlocksIsAtomicOnLaterFailure() {
  StructuredCFG Cfg;

  Cfg.addBlock(block(10, {11}));
  CFGBlock Source = switchBlock(11, {12, 13});
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));

  CFGBlock BadCopy = switchBlock(20, {12});
  BadCopy.Origin = CFGBlockOrigin::Copied;
  BadCopy.SourceBlock = 11;
  BadCopy.CopyKind = CFGBlockCopyKind::RegionCopy;
  BadCopy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  BadCopy.BodyBlock = 11;
  BadCopy.BodyMaterialized = false;
  Cfg.addBlock(std::move(BadCopy));

  assert(!Cfg.removeBlocks({10, 11}));

  const CFGBlock *First = Cfg.getBlock(10);
  const CFGBlock *SourceBlock = Cfg.getBlock(11);
  const CFGBlock *BadCopyBlock = Cfg.getBlock(20);
  assert(First != nullptr && SourceBlock != nullptr && BadCopyBlock != nullptr);
  assert(First->Successors == std::vector<BlockId>{11});
  assert(!BadCopyBlock->BodyMaterialized);
  assert(BadCopyBlock->BodyBlock == 11);
  assert(BadCopyBlock->Condition.Id == InvalidPayloadId);
}

void testStructuredCFGMaterializesCopiedSwitchWithoutRewritingTargets() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(10, {11, 12});
  Switch.Condition = {70};
  Switch.Cases.clear();
  Switch.Cases.push_back({{71}, 12});
  Switch.Cases.push_back({{72}, 11});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Body = block(11, {13});
  Body.Statements.push_back({31});
  Cfg.addBlock(std::move(Body));

  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({10, 11});
  assert(CopyRegion.has_value());

  BlockId CopySwitchId = CopyRegion->copyOf(10);
  BlockId CopyBodyId = CopyRegion->copyOf(11);
  assert(CopySwitchId != InvalidBlockId);
  assert(CopyBodyId != InvalidBlockId);

  std::size_t CaseRewrites = 0;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::SwitchCaseValue) {
          return Payload;
        }

        assert(Context.SourceBlock == 10);
        assert(Context.BodyBlock == 10);
        assert(Context.CopyBlock == CopySwitchId);
        assert(Context.CopiedFromBlock == 10);
        assert(Context.OriginalPredecessor == 90);
        assert(Context.NewPredecessor == 91);
        assert(Context.OriginalCases.size() == 2);
        assert(Context.NewCases.size() == 2);
        assert(Context.OriginalCases[0].Value.Id == 71);
        assert(Context.OriginalCases[1].Value.Id == 72);
        assert(Context.NewCases[0].Value.Id == InvalidPayloadId);
        assert(Context.NewCases[1].Value.Id == InvalidPayloadId);
        assert(Context.OriginalCases[0].Target == 12);
        assert(Context.OriginalCases[1].Target == 11);
        assert(Context.NewCases[0].Target == 12);
        assert(Context.NewCases[1].Target == CopyBodyId);
        assert(Context.OriginalSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.NewSuccessors ==
               std::vector<BlockId>({CopyBodyId, 12}));
        assert(Context.OriginalTerminator == TerminatorKind::Switch);
        assert(Context.NewTerminator == TerminatorKind::Switch);
        if (Index == 0) {
          assert(Context.OriginalTarget == 12);
          assert(Context.NewTarget == 12);
        } else {
          assert(Index == 1);
          assert(Context.OriginalTarget == 11);
          assert(Context.NewTarget == CopyBodyId);
        }
        ++CaseRewrites;
        return PayloadRef{Payload.Id + 1000 + Index};
      });

  assert(Cfg.materializeBlockBody(CopySwitchId, 90, 91));
  const CFGBlock *CopySwitch = Cfg.getBlock(CopySwitchId);
  assert(CopySwitch != nullptr);
  assert(CopySwitch->Origin == CFGBlockOrigin::Copied);
  assert(CopySwitch->SourceBlock == 10);
  assert(CopySwitch->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(CopySwitch->BodyMaterialized);
  assert(CopySwitch->BodyBlock == CopySwitchId);
  assert(CopySwitch->Terminator == TerminatorKind::Switch);
  assert(CopySwitch->Condition.Id == 70);
  assert(CopySwitch->Successors == std::vector<BlockId>({CopyBodyId, 12}));
  assert(CopySwitch->Cases.size() == 2);
  assert(CopySwitch->Cases[0].Value.Id == 1071);
  assert(CopySwitch->Cases[0].Target == 12);
  assert(CopySwitch->Cases[1].Value.Id == 1073);
  assert(CopySwitch->Cases[1].Target == CopyBodyId);
  assert(CaseRewrites == 2);
}

void testStructuredCFGDuplicateCopyKeepsSwitchBodySource() {
  StructuredCFG Cfg;

  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  BlockId CopyId = Cfg.duplicateBlock(10, {11, 12});
  assert(CopyId == 13);
  BlockId CopyCopyId = Cfg.duplicateBlock(CopyId, {11, 12});
  assert(CopyCopyId == 14);

  const CFGBlock *CopyCopy = Cfg.getBlock(CopyCopyId);
  assert(CopyCopy != nullptr);
  assert(CopyCopy->Origin == CFGBlockOrigin::Copied);
  assert(CopyCopy->SourceBlock == 10);
  assert(CopyCopy->CopiedFromBlock == CopyId);
  assert(CopyCopy->BodyBlock == 10);
  assert(CopyCopy->Successors == std::vector<BlockId>({11, 12}));
  assert(CopyCopy->Cases.size() == 1);
  assert(CopyCopy->Cases.front().Target == 12);
  assert(CopyCopy->Cases.front().Value.Id == InvalidPayloadId);

  std::size_t CaseRewrites = 0;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::SwitchCaseValue) {
          return Payload;
        }
        assert(Context.SourceBlock == 10);
        assert(Context.BodyBlock == 10);
        assert(Context.CopyBlock == CopyCopyId);
        assert(Context.CopiedFromBlock == CopyId);
        assert(Context.OriginalCases.size() == 1);
        assert(Context.NewCases.size() == 1);
        assert(Context.OriginalCases.front().Target == 12);
        assert(Context.NewCases.front().Target == 12);
        assert(Context.OriginalSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.NewSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.OriginalTarget == 12);
        assert(Context.NewTarget == 12);
        assert(Index == 0);
        ++CaseRewrites;
        return PayloadRef{Payload.Id + 1000};
      });

  assert(Cfg.materializeBlockBody(CopyCopyId));
  CopyCopy = Cfg.getBlock(CopyCopyId);
  assert(CopyCopy != nullptr);
  assert(CopyCopy->BodyMaterialized);
  assert(CopyCopy->BodyBlock == CopyCopyId);
  assert(CopyCopy->Successors == std::vector<BlockId>({11, 12}));
  assert(CopyCopy->Cases.front().Value.Id == 1071);
  assert(CopyCopy->Cases.front().Target == 12);
  assert(CaseRewrites == 1);
}

void testStructuredCFGMaterializeCopyOfCopyKeepsSwitchIdentity() {
  StructuredCFG Cfg;

  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  BlockId CopyId = Cfg.duplicateBlock(10, {11, 12});
  assert(CopyId == 13);
  BlockId CopyCopyId = Cfg.duplicateBlock(CopyId, {11, 12});
  assert(CopyCopyId == 14);

  bool SawCopyOfCopy = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::SwitchCaseValue) {
          return Payload;
        }

        assert(Context.SourceBlock == 10);
        assert(Context.BodyBlock == 10);
        assert(Context.CopyBlock == CopyCopyId);
        assert(Context.CopiedFromBlock == CopyId);
        assert(Context.OriginalCases.size() == 1);
        assert(Context.NewCases.size() == 1);
        assert(Context.OriginalCases.front().Target == 12);
        assert(Context.NewCases.front().Target == 12);
        assert(Context.OriginalSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.NewSuccessors == std::vector<BlockId>({11, 12}));
        assert(Context.OriginalTarget == 12);
        assert(Context.NewTarget == 12);
        assert(Index == 0);
        SawCopyOfCopy = true;
        return PayloadRef{Payload.Id + 1000};
      });

  assert(Cfg.materializeBlockBody(CopyCopyId));
  const CFGBlock *CopyCopy = Cfg.getBlock(CopyCopyId);
  assert(CopyCopy != nullptr);
  assert(CopyCopy->BodyMaterialized);
  assert(CopyCopy->BodyBlock == CopyCopyId);
  assert(CopyCopy->Cases.front().Value.Id == 1071);
  assert(CopyCopy->Cases.front().Target == 12);
  assert(SawCopyOfCopy);
}

void testStructuredCFGMaterializeRejectsMismatchedSwitchCases() {
  StructuredCFG Cfg;

  CFGBlock Source = switchBlock(10, {11, 12});
  Source.Condition = {70};
  Source.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Source));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));

  CFGBlock Copy = switchBlock(20, {11});
  Copy.Origin = CFGBlockOrigin::Copied;
  Copy.SourceBlock = 10;
  Copy.CopyKind = CFGBlockCopyKind::RegionCopy;
  Copy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  Copy.BodyBlock = 10;
  Copy.BodyMaterialized = false;
  Cfg.addBlock(std::move(Copy));

  assert(!Cfg.materializeBlockBody(20));

  const CFGBlock *CopyBlock = Cfg.getBlock(20);
  assert(CopyBlock != nullptr);
  assert(!CopyBlock->BodyMaterialized);
  assert(CopyBlock->BodyBlock == 10);
  assert(CopyBlock->Statements.empty());
  assert(CopyBlock->Condition.Id == InvalidPayloadId);
  assert(CopyBlock->Cases.empty());
}

void testStructuredCFGRejectsInconsistentCopiedSwitchSuccessors() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(10, {11, 12});
  Cfg.addBlock(std::move(Switch));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));

  assert(Cfg.duplicateBlock(10, {13}) == InvalidBlockId);

  const CFGBlock *Original = Cfg.getBlock(10);
  assert(Original != nullptr);
  assert(Original->Successors == std::vector<BlockId>({11, 12}));
  assert(Original->Cases.size() == 1);
  assert(Original->Cases.front().Target == 12);
}

void testStructuredCFGMaterializeFailsWhenBodySourceIsMissing() {
  StructuredCFG Cfg;

  CFGBlock Copy = block(20, {});
  Copy.Origin = CFGBlockOrigin::Copied;
  Copy.SourceBlock = 10;
  Copy.CopyKind = CFGBlockCopyKind::RegionCopy;
  Copy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  Copy.BodyBlock = 10;
  Copy.BodyMaterialized = false;
  Copy.Statements.push_back({7});
  Cfg.addBlock(std::move(Copy));

  assert(!Cfg.materializeBlockBody(20));

  const CFGBlock *Block = Cfg.getBlock(20);
  assert(Block != nullptr);
  assert(Block->Origin == CFGBlockOrigin::Copied);
  assert(Block->SourceBlock == 10);
  assert(Block->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(!Block->BodyMaterialized);
  assert(Block->BodyBlock == 10);
  assert(hasSinglePayload(Block->Statements, 7));
}

void testStructuredCFGRedirectPredecessorsIsAtomic() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {}));
  Cfg.addBlock(block(4, {}));

  assert(!Cfg.redirectPredecessors(2, 4, {0, 1}));
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors == std::vector<BlockId>{2});
  assert(Block1->Successors == std::vector<BlockId>{3});

  assert(Cfg.redirectPredecessors(2, 4, {0}));
  assert(Block0->Successors == std::vector<BlockId>{4});
}

void testStructuredCFGRedirectPredecessorsUpdatesSwitchCases() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {}));

  assert(Cfg.redirectPredecessors(2, 3, {0}));
  const CFGBlock *Switch = Cfg.getBlock(0);
  assert(Switch != nullptr);
  assert(Switch->Successors == std::vector<BlockId>({1, 3}));
  assert(Switch->Cases.size() == 1);
  assert(Switch->Cases.front().Target == 3);
}

void testStructuredCFGReplaceEdgeUpdatesSwitchCases() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9});
  Switch.Cases.push_back({{}, 2});
  Cfg.addBlock(std::move(Switch));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(9, {}));

  assert(Cfg.replaceEdge(0, 2, 3));
  const CFGBlock *Block = Cfg.getBlock(0);
  assert(Block != nullptr);
  assert(Block->Successors == std::vector<BlockId>{9});
  assert(Block->Cases.size() == 1);
  assert(Block->Cases.front().Target == 3);
  assert(!Cfg.hasEdge(0, 2));
  assert(Cfg.hasEdge(0, 3));
}

void testStructuredCFGFindsCaseOnlyPredecessors() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9});
  Switch.Cases.push_back({{}, 2});
  Cfg.addBlock(std::move(Switch));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(9, {}));

  assert(Cfg.hasEdge(0, 2));
  assert(Cfg.predecessorsOf(2) == std::vector<BlockId>{0});
}

void testStructuredCFGSuccessorsOfIncludesCaseTargets() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9});
  Switch.Cases.push_back({{}, 2});
  Cfg.addBlock(std::move(Switch));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(9, {}));

  assert(Cfg.successorsOf(0) == std::vector<BlockId>({9, 2}));
}

void testStructuredCFGSuccessorsOfDeduplicatesCaseTargets() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9, 2});
  Switch.Cases.push_back({{}, 2});
  Cfg.addBlock(std::move(Switch));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(9, {}));

  assert(Cfg.successorsOf(0) == std::vector<BlockId>({9, 2}));
}

void testStructuredCFGDuplicateRegionRewritesInternalEdges() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(10, {11, 12});
  Switch.Cases.push_back({{}, 11});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Body = block(11, {13});
  Body.Statements.push_back({31});
  Cfg.addBlock(std::move(Body));

  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({10, 11});
  assert(CopyRegion.has_value());
  assert(CopyRegion->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(CopyRegion->CreatedBy == CFGBlockCreator::SAILRDeoptimization);

  BlockId CopySwitchId = CopyRegion->copyOf(10);
  BlockId CopyBodyId = CopyRegion->copyOf(11);
  assert(CopySwitchId != InvalidBlockId);
  assert(CopyBodyId != InvalidBlockId);
  assert(CopyRegion->originalOf(CopySwitchId) == 10);
  assert(CopyRegion->originalOf(CopyBodyId) == 11);
  assert(CopyRegion->originalOf(12) == InvalidBlockId);

  const CFGBlock *CopySwitch = Cfg.getBlock(CopySwitchId);
  const CFGBlock *CopyBody = Cfg.getBlock(CopyBodyId);
  assert(CopySwitch != nullptr && CopyBody != nullptr);
  assert(CopySwitch->Origin == CFGBlockOrigin::Copied);
  assert(CopyBody->Origin == CFGBlockOrigin::Copied);
  assert(CopySwitch->SourceBlock == 10);
  assert(CopyBody->SourceBlock == 11);
  assert(CopySwitch->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(CopyBody->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(!CopySwitch->BodyMaterialized);
  assert(!CopyBody->BodyMaterialized);
  assert(CopySwitch->BodyBlock == 10);
  assert(CopyBody->BodyBlock == 11);
  assert(CopySwitch->Successors == std::vector<BlockId>({CopyBodyId, 12}));
  assert(CopySwitch->Cases.size() == 2);
  assert(CopySwitch->Cases[0].Target == 12);
  assert(CopySwitch->Cases[1].Target == CopyBodyId);
  assert(CopyBody->Successors == std::vector<BlockId>{13});
  assert(CopyBody->Statements.empty());
  assert(Cfg.materializeBlockBody(CopyBodyId));
  CopyBody = Cfg.getBlock(CopyBodyId);
  assert(CopyBody != nullptr);
  assert(hasSinglePayload(CopyBody->Statements, 31));
}

void testStructuredCFGDuplicateRegionKeepsSyntheticForwarderIdentity() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  BlockId Forwarder = Cfg.createSyntheticForwarder(
      10, 2, CFGBlockCreator::SAILRDeoptimization);
  assert(Forwarder != InvalidBlockId);
  Cfg.addBlock(block(2, {}));

  bool SawCommit = false;
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.CopyBlock != InvalidBlockId);
        assert(Context.SourceBlock == Forwarder);
        assert(Context.BodyBlock == Forwarder);
        assert(Context.SyntheticSource == 10);
        assert(Context.SyntheticTarget == 2);
        assert(Result == PayloadMaterializeResult::Committed);
        assert(Payloads.empty());
        SawCommit = true;
      });

  std::optional<DuplicatedRegion> CopyRegion =
      Cfg.duplicateRegion({Forwarder});
  assert(CopyRegion.has_value());

  BlockId CopyId = CopyRegion->copyOf(Forwarder);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Id != Forwarder);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Forwarder);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->Successors == std::vector<BlockId>{2});
  assert(Copy->SyntheticSource == 10);
  assert(Copy->SyntheticTarget == 2);
  assert(Cfg.materializeBlockBody(CopyId));
  Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Forwarder);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->Statements.empty());
  assert(Copy->Successors == std::vector<BlockId>{2});
  assert(Copy->SyntheticSource == 10);
  assert(Copy->SyntheticTarget == 2);
  assert(SawCommit);
}

void testStructuredCFGDuplicateDephicationEdgeCopiesMetadata() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));
  Cfg.addBlock(block(3, {}));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({1, 4, 3});
  assert(CopyRegion.has_value());

  BlockId CopyIncoming = CopyRegion->copyOf(1);
  BlockId CopyEdge = CopyRegion->copyOf(4);
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyIncoming != InvalidBlockId);
  assert(CopyEdge != InvalidBlockId);
  assert(CopyMerge != InvalidBlockId);

  const std::vector<DephicationIncoming> &Incomings =
      Cfg.dephicationIncomings();
  assert(Incomings.size() == 2);
  assert(Incomings[0].IncomingBlock == 1);
  assert(Incomings[0].MergeBlock == 3);
  assert(Incomings[0].EdgeBlock == 4);
  assert(Incomings[0].Target == VVar);
  assert(Incomings[0].SourceTarget == VVar);
  assert(Incomings[0].SourceIncomingBlock == 1);
  assert(Incomings[0].SourceMergeBlock == 3);
  assert(Incomings[0].SourceEdgeBlock == 4);
  assert(Incomings[0].Assignment.Id == 40);
  assert(Incomings[1].IncomingBlock == CopyIncoming);
  assert(Incomings[1].MergeBlock == CopyMerge);
  assert(Incomings[1].EdgeBlock == CopyEdge);
  assert(Incomings[1].Target != VVar);
  assert(Incomings[1].SourceTarget == VVar);
  assert(Incomings[1].SourceIncomingBlock == 1);
  assert(Incomings[1].SourceMergeBlock == 3);
  assert(Incomings[1].SourceEdgeBlock == 4);
  assert(Incomings[1].Assignment.Id == 40);

  const std::vector<DephicationVVar> &VVarsAfterCopy = Cfg.dephicationVVars();
  assert(VVarsAfterCopy.size() == 2);
  assert(VVarsAfterCopy[0].Id == VVar);
  assert(VVarsAfterCopy[0].MergeBlock == 3);
  VVarId CopyVVar = VVarsAfterCopy[1].Id;
  assert(VVarsAfterCopy[1].MergeBlock == CopyMerge);
  assert(VVarsAfterCopy[1].Name == "x");

  bool SawDephicationAssignment = false;
  Cfg.setPayloadMaterializeHook(
      [CopyEdge, CopyMerge, CopyVVar, VVar,
       &SawDephicationAssignment](const PayloadMaterializeContext &Context,
         PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        assert(Context.DephicationVVarCopies.size() == 1);
        assert(Context.DephicationVVarCopies.begin()->first == VVar);
        assert(Context.DephicationVVarCopies.begin()->second == CopyVVar);
        assert(Context.DephicationVVars.size() == 1);
        assert(Context.DephicationVVars.front().Name == "x");
        assert(Context.DephicationVVars.front().MergeBlock == CopyMerge);
        assert(Context.DephicationIncomings.size() == 1);
        assert(Context.DephicationIncomings.front().Target == CopyVVar);
        assert(Context.DephicationIncomings.front().EdgeBlock == CopyEdge);
        if (Kind == PayloadMaterializeKind::DephicationAssignment) {
          SawDephicationAssignment = true;
          assert(Context.CurrentDephicationIncoming.has_value());
          assert(Context.CurrentDephicationIncoming->Target == CopyVVar);
          assert(Context.CurrentDephicationIncoming->Assignment.Id ==
                 Payload.Id);
          return PayloadRef{Payload.Id + 1000};
        }
        assert(!Context.CurrentDephicationIncoming.has_value());
        return Payload;
      });
  assert(Cfg.materializeBlockBody(CopyEdge));
  assert(SawDephicationAssignment);

  const CFGBlock *CopyEdgeBlock = Cfg.getBlock(CopyEdge);
  assert(CopyEdgeBlock != nullptr);
  assert(CopyEdgeBlock->Statements.size() == 1);
  assert(CopyEdgeBlock->Statements.front().Id == 1040);
  assert(Cfg.dephicationIncomings()[1].Assignment.Id == 1040);
}

void testStructuredCFGQueriesDephicationEdgeContext() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));
  Cfg.addBlock(block(3, {}));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  DephicationEdgeContext OriginalContext = Cfg.dephicationEdgeContext(4);
  assert(OriginalContext.Incomings.size() == 1);
  assert(OriginalContext.VVars.size() == 1);
  assert(OriginalContext.VVars.front().Id == VVar);
  assert(OriginalContext.VVarCopies.empty());

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({1, 4, 3});
  assert(CopyRegion.has_value());
  BlockId CopyEdge = CopyRegion->copyOf(4);
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyEdge != InvalidBlockId);
  assert(CopyMerge != InvalidBlockId);

  DephicationEdgeContext CopyContext = Cfg.dephicationEdgeContext(CopyEdge);
  assert(CopyContext.Incomings.size() == 1);
  assert(CopyContext.VVars.size() == 1);
  assert(CopyContext.VVars.front().MergeBlock == CopyMerge);
  assert(CopyContext.VVarCopies.size() == 1);
  assert(CopyContext.VVarCopies.begin()->first == VVar);
  assert(CopyContext.VVarCopies.begin()->second ==
         CopyContext.VVars.front().Id);

  assert(Cfg.removeBlock(CopyMerge));
  DephicationEdgeContext RetiredContext = Cfg.dephicationEdgeContext(CopyEdge);
  assert(RetiredContext.Incomings.empty());
  assert(RetiredContext.VVars.empty());
  assert(RetiredContext.VVarCopies.empty());
}

void testStructuredCFGQueriesCopiedDephicationEdgeWithoutCopiedMerge() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));
  Cfg.addBlock(block(3, {}));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({1, 4});
  assert(CopyRegion.has_value());
  BlockId CopyEdge = CopyRegion->copyOf(4);
  assert(CopyEdge != InvalidBlockId);

  DephicationEdgeContext CopyContext = Cfg.dephicationEdgeContext(CopyEdge);
  assert(CopyContext.Incomings.size() == 1);
  assert(CopyContext.VVars.size() == 1);
  assert(CopyContext.VVars.front().Id == VVar);
  assert(CopyContext.VVarCopies.empty());
}

void testStructuredCFGMaterializeCopiedMergeReportsDephicationVVarCopy() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));

  CFGBlock Merge = block(3, {});
  Merge.Statements.push_back({41});
  Cfg.addBlock(std::move(Merge));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({1, 4, 3});
  assert(CopyRegion.has_value());
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyMerge != InvalidBlockId);

  const std::vector<DephicationVVar> &VVars = Cfg.dephicationVVars();
  assert(VVars.size() == 2);
  VVarId CopyVVar = VVars[1].Id;
  assert(CopyVVar != VVar);
  assert(VVars[1].SourceId == VVar);
  assert(VVars[1].MergeBlock == CopyMerge);

  DephicationEdgeContext CopyMergeContext =
      Cfg.dephicationBlockContext(CopyMerge);
  assert(CopyMergeContext.Incomings.empty());
  assert(CopyMergeContext.VVars.size() == 1);
  assert(CopyMergeContext.VVars.front().Id == CopyVVar);
  assert(CopyMergeContext.VVarCopies.size() == 1);
  assert(CopyMergeContext.VVarCopies.begin()->first == VVar);
  assert(CopyMergeContext.VVarCopies.begin()->second == CopyVVar);

  bool SawCopiedVVar = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        assert(Context.CopyBlock == CopyMerge);
        assert(Context.DephicationIncomings.empty());
        assert(Context.DephicationVVars.size() == 1);
        assert(Context.DephicationVVars.front().Id == CopyVVar);
        assert(Context.DephicationVVarCopies.size() == 1);
        assert(Context.DephicationVVarCopies.begin()->first == VVar);
        assert(Context.DephicationVVarCopies.begin()->second == CopyVVar);
        SawCopiedVVar = true;
        return PayloadRef{Payload.Id + 1000};
      });

  assert(Cfg.materializeBlockBody(CopyMerge));
  assert(SawCopiedVVar);

  const CFGBlock *CopyMergeBlock = Cfg.getBlock(CopyMerge);
  assert(CopyMergeBlock != nullptr);
  assert(CopyMergeBlock->Statements.size() == 1);
  assert(CopyMergeBlock->Statements.front().Id == 1041);
}

void testStructuredCFGDuplicateCopiedDephicationEdgeKeepsIdentityChain() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));

  CFGBlock Merge = block(3, {});
  Merge.Statements.push_back({41});
  Cfg.addBlock(std::move(Merge));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion1 = Cfg.duplicateRegion({1, 4, 3});
  assert(CopyRegion1.has_value());
  BlockId CopyIncoming1 = CopyRegion1->copyOf(1);
  BlockId CopyEdge1 = CopyRegion1->copyOf(4);
  BlockId CopyMerge1 = CopyRegion1->copyOf(3);
  assert(CopyIncoming1 != InvalidBlockId);
  assert(CopyEdge1 != InvalidBlockId);
  assert(CopyMerge1 != InvalidBlockId);

  assert(Cfg.materializeBlockBody(CopyEdge1));
  const std::vector<DephicationVVar> &VVarsAfterFirstCopy = Cfg.dephicationVVars();
  assert(VVarsAfterFirstCopy.size() == 2);
  VVarId CopyVVar1 = VVarsAfterFirstCopy[1].Id;
  assert(VVarsAfterFirstCopy[1].SourceId == VVar);
  assert(VVarsAfterFirstCopy[1].MergeBlock == CopyMerge1);

  std::optional<DuplicatedRegion> CopyRegion2 =
      Cfg.duplicateRegion({CopyIncoming1, CopyEdge1, CopyMerge1});
  assert(CopyRegion2.has_value());
  BlockId CopyIncoming2 = CopyRegion2->copyOf(CopyIncoming1);
  BlockId CopyEdge2 = CopyRegion2->copyOf(CopyEdge1);
  BlockId CopyMerge2 = CopyRegion2->copyOf(CopyMerge1);
  assert(CopyIncoming2 != InvalidBlockId);
  assert(CopyEdge2 != InvalidBlockId);
  assert(CopyMerge2 != InvalidBlockId);

  const std::vector<DephicationVVar> &VVarsAfterSecondCopy =
      Cfg.dephicationVVars();
  assert(VVarsAfterSecondCopy.size() == 3);
  VVarId CopyVVar2 = VVarsAfterSecondCopy[2].Id;
  assert(VVarsAfterSecondCopy[2].SourceId == VVar);
  assert(VVarsAfterSecondCopy[2].MergeBlock == CopyMerge2);

  const std::vector<DephicationIncoming> &Incomings = Cfg.dephicationIncomings();
  assert(Incomings.size() == 3);
  assert(Incomings[2].IncomingBlock == CopyIncoming2);
  assert(Incomings[2].MergeBlock == CopyMerge2);
  assert(Incomings[2].EdgeBlock == CopyEdge2);
  assert(Incomings[2].SourceIncomingBlock == CopyIncoming1);
  assert(Incomings[2].SourceMergeBlock == CopyMerge1);
  assert(Incomings[2].SourceEdgeBlock == CopyEdge1);
  assert(Incomings[2].SourceTarget == CopyVVar1);
  assert(Incomings[2].Target == CopyVVar2);

  DephicationEdgeContext CopyContext = Cfg.dephicationEdgeContext(CopyEdge2);
  assert(CopyContext.Incomings.size() == 1);
  assert(CopyContext.VVars.size() == 1);
  assert(CopyContext.VVars.front().Id == CopyVVar2);
  assert(CopyContext.VVarCopies.size() == 1);
  assert(CopyContext.VVarCopies.begin()->first == CopyVVar1);
  assert(CopyContext.VVarCopies.begin()->second == CopyVVar2);
}

void testStructuredCFGRedirectDephicationEdgeRematerializesAssignment() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));

  CFGBlock Merge = block(3, {});
  Merge.Statements.push_back({41});
  Cfg.addBlock(std::move(Merge));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({3});
  assert(CopyRegion.has_value());
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyMerge != InvalidBlockId);

  const std::vector<DephicationVVar> &VVars = Cfg.dephicationVVars();
  assert(VVars.size() == 2);
  VVarId CopyVVar = VVars[1].Id;
  assert(CopyVVar != VVar);

  bool SawRedirectedAssignment = false;
  Cfg.setPayloadMaterializeHook(
      [CopyVVar, VVar, &SawRedirectedAssignment](
          const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::DephicationAssignment) {
          return Payload;
        }
        assert(Context.CurrentDephicationIncoming.has_value());
        assert(Context.CurrentDephicationIncoming->SourceTarget == VVar);
        assert(Context.CurrentDephicationIncoming->Target == CopyVVar);
        assert(Context.DephicationVVarCopies.size() == 1);
        assert(Context.DephicationVVarCopies.begin()->first == VVar);
        assert(Context.DephicationVVarCopies.begin()->second == CopyVVar);
        SawRedirectedAssignment = true;
        return PayloadRef{Payload.Id + 1000};
      });

  assert(Cfg.redirectDephicationIncomingTarget(4, 3, CopyMerge));
  assert(SawRedirectedAssignment);

  const CFGBlock *EdgeBlock = Cfg.getBlock(4);
  assert(EdgeBlock != nullptr);
  assert(EdgeBlock->BodyMaterialized);
  assert(EdgeBlock->BodyBlock == 4);
  assert(EdgeBlock->Statements.size() == 1);
  assert(EdgeBlock->Statements.front().Id == 1040);

  const DephicationIncoming &Incoming = Cfg.dephicationIncomings().front();
  assert(Incoming.MergeBlock == CopyMerge);
  assert(Incoming.Target == CopyVVar);
  assert(Incoming.Assignment.Id == 1040);
}

void testStructuredCFGRedirectDephicationEdgeFailureRollsBack() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));

  CFGBlock Merge = block(3, {});
  Merge.Statements.push_back({41});
  Cfg.addBlock(std::move(Merge));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({3});
  assert(CopyRegion.has_value());
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyMerge != InvalidBlockId);

  bool SawRedirectedAssignment = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        (void)Payload;
        if (Kind != PayloadMaterializeKind::DephicationAssignment) {
          return Payload;
        }
        assert(Context.CurrentDephicationIncoming.has_value());
        assert(Context.CurrentDephicationIncoming->SourceTarget == VVar);
        SawRedirectedAssignment = true;
        return std::nullopt;
      });

  assert(!Cfg.redirectDephicationIncomingTarget(4, 3, CopyMerge));
  assert(SawRedirectedAssignment);

  const CFGBlock *EdgeBlock = Cfg.getBlock(4);
  assert(EdgeBlock != nullptr);
  assert(EdgeBlock->BodyMaterialized);
  assert(EdgeBlock->BodyBlock == 4);
  assert(EdgeBlock->Statements.size() == 1);
  assert(EdgeBlock->Statements.front().Id == 40);
  assert(EdgeBlock->SyntheticTarget == 3);

  const DephicationIncoming &Incoming = Cfg.dephicationIncomings().front();
  assert(Incoming.MergeBlock == 3);
  assert(Incoming.Target == VVar);
  assert(Incoming.Assignment.Id == 40);
}

void testStructuredCFGRemoveCopiedDephicationMergeRetiresVVar() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));
  Cfg.addBlock(block(3, {}));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  std::optional<DuplicatedRegion> CopyRegion = Cfg.duplicateRegion({1, 4, 3});
  assert(CopyRegion.has_value());
  BlockId CopyMerge = CopyRegion->copyOf(3);
  assert(CopyMerge != InvalidBlockId);
  assert(Cfg.dephicationVVars().size() == 2);

  assert(Cfg.removeBlock(CopyMerge));
  assert(Cfg.dephicationIncomings().size() == 1);
  assert(Cfg.dephicationVVars().size() == 2);
  assert(Cfg.dephicationVVars()[0].Id == VVar);
  assert(Cfg.dephicationVVars()[0].Retired == false);
  assert(Cfg.dephicationVVars()[1].Retired == true);

  const std::vector<DephicationVVar> &AllVVars = Cfg.dephicationVVars();
  assert(AllVVars.size() == 2);
  assert(AllVVars[0].Id == VVar);
  assert(AllVVars[0].Retired == false);
  assert(AllVVars[1].Retired == true);
}

void testStructuredCFGRemoveBlockMaintainsDephicationMetadata() {
  StructuredCFG Cfg;
  CFGBlock Body = block(4, {3});
  Body.Statements.push_back({40});
  Cfg.addBlock(std::move(Body));

  CFGBlock Copy = block(5, {3});
  Copy.Origin = CFGBlockOrigin::Copied;
  Copy.CopiedFromBlock = 4;
  Copy.BodyBlock = 4;
  Copy.BodyMaterialized = false;
  Cfg.addBlock(std::move(Copy));
  Cfg.addBlock(block(3, {}));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  CFGBlock *CopyBlock = Cfg.getBlock(5);
  assert(CopyBlock != nullptr);
  CopyBlock->Cases.push_back({{}, 3});
  assert(!Cfg.removeBlock(4));
  assert(Cfg.getBlock(4) != nullptr);
  assert(Cfg.dephicationIncomings().size() == 1);
  assert(Cfg.dephicationIncomings().front().EdgeBlock == 4);
  assert(Cfg.dephicationIncomings().front().SourceEdgeBlock == 4);

  CopyBlock->Cases.clear();
  assert(Cfg.removeBlock(4));
  assert(Cfg.getBlock(4) == nullptr);
  assert(Cfg.dephicationIncomings().empty());
}

void testStructuredCFGDuplicateSyntheticForwarderReportsTargets() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  BlockId Forwarder = Cfg.createSyntheticForwarder(
      10, 2, CFGBlockCreator::SAILRDeoptimization);
  assert(Forwarder != InvalidBlockId);
  Cfg.addBlock(block(2, {}));

  bool SawForwarder = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        assert(Kind == PayloadMaterializeKind::Statement ||
               Kind == PayloadMaterializeKind::Condition ||
               Kind == PayloadMaterializeKind::SwitchCaseValue);
        assert(Context.SourceBlock == Forwarder);
        assert(Context.BodyBlock == Forwarder);
        assert(Context.SyntheticSource == 10);
        assert(Context.SyntheticTarget == 2);
        assert(Context.CopyBlock != InvalidBlockId);
        SawForwarder = true;
        return Payload;
      });

  std::optional<DuplicatedRegion> CopyRegion =
      Cfg.duplicateRegion({Forwarder});
  assert(CopyRegion.has_value());

  BlockId CopyId = CopyRegion->copyOf(Forwarder);
  assert(Cfg.materializeBlockBody(CopyId));
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Forwarder);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->Successors == std::vector<BlockId>{2});
  assert(Copy->SyntheticSource == 10);
  assert(Copy->SyntheticTarget == 2);
  assert(SawForwarder);
}

void testStructuredCFGDuplicateSyntheticGotoReportsTargets() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  BlockId Goto = Cfg.createSyntheticGoto(10, 2, CFGBlockCreator::SAILRDeoptimization);
  assert(Goto != InvalidBlockId);
  Cfg.addBlock(block(2, {}));

  bool SawGoto = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        assert(Kind == PayloadMaterializeKind::Statement ||
               Kind == PayloadMaterializeKind::Condition ||
               Kind == PayloadMaterializeKind::SwitchCaseValue);
        assert(Context.SourceBlock == Goto);
        assert(Context.BodyBlock == Goto);
        assert(Context.SyntheticSource == 10);
        assert(Context.SyntheticTarget == 2);
        assert(Context.CopyBlock != InvalidBlockId);
        SawGoto = true;
        return Payload;
      });

  std::optional<DuplicatedRegion> CopyRegion =
      Cfg.duplicateRegion({Goto});
  assert(CopyRegion.has_value());

  BlockId CopyId = CopyRegion->copyOf(Goto);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Goto);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyBlock == Goto);
  assert(Copy->Successors.empty());
  assert(Copy->SyntheticSource == 10);
  assert(Copy->SyntheticTarget == 2);
  assert(Cfg.materializeBlockBody(CopyId));
  Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->Successors.empty());
  assert(Copy->SyntheticSource == 10);
  assert(Copy->SyntheticTarget == 2);
  assert(SawGoto);
}

void testStructuredCFGDuplicateSyntheticGotoKeepsCopyIdentityChain() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  BlockId Goto = Cfg.createSyntheticGoto(30, 2, CFGBlockCreator::SAILRDeoptimization);
  assert(Goto != InvalidBlockId);
  Cfg.addBlock(block(2, {}));

  std::optional<DuplicatedRegion> CopyRegion =
      Cfg.duplicateRegion({Goto});
  assert(CopyRegion.has_value());

  BlockId CopyId = CopyRegion->copyOf(Goto);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Goto);
  assert(Copy->CopiedFromBlock == Goto);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyBlock == Goto);
  assert(Copy->SyntheticSource == 30);
  assert(Copy->SyntheticTarget == 2);

  assert(Cfg.materializeBlockBody(CopyId));
  Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Goto);
  assert(Copy->CopiedFromBlock == Goto);
  assert(Copy->CopyKind == CFGBlockCopyKind::RegionCopy);
  assert(Copy->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->SyntheticSource == 30);
  assert(Copy->SyntheticTarget == 2);
}

void testStructuredCFGMaterializeCopiedSyntheticGotoKeepsIdentity() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  BlockId Goto = Cfg.createSyntheticGoto(30, 2, CFGBlockCreator::SAILRDeoptimization);
  assert(Goto != InvalidBlockId);
  Cfg.addBlock(block(2, {}));

  bool SawCommit = false;
  Cfg.setPayloadMaterializeResultHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeResult Result,
          const std::vector<PayloadRef> &Payloads) {
        assert(Context.SourceBlock == Goto);
        assert(Context.BodyBlock == Goto);
        assert(Context.CopyBlock != InvalidBlockId);
        assert(Context.CopiedFromBlock == Goto);
        assert(Context.SyntheticSource == 30);
        assert(Context.SyntheticTarget == 2);
        assert(Context.CopyKind == CFGBlockCopyKind::RegionCopy);
        assert(Context.CreatedBy == CFGBlockCreator::SAILRDeoptimization);
        assert(Result == PayloadMaterializeResult::Committed);
        assert(Payloads.empty());
        SawCommit = true;
      });

  std::optional<DuplicatedRegion> CopyRegion =
      Cfg.duplicateRegion({Goto});
  assert(CopyRegion.has_value());

  BlockId CopyId = CopyRegion->copyOf(Goto);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Goto);
  assert(Copy->CopiedFromBlock == Goto);
  assert(Copy->BodyBlock == Goto);
  assert(Copy->SyntheticSource == 30);
  assert(Copy->SyntheticTarget == 2);

  assert(Cfg.materializeBlockBody(CopyId));
  Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Origin == CFGBlockOrigin::Copied);
  assert(Copy->SourceBlock == Goto);
  assert(Copy->CopiedFromBlock == Goto);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == CopyId);
  assert(Copy->SyntheticSource == 30);
  assert(Copy->SyntheticTarget == 2);
  assert(SawCommit);
}

void testStructuredCFGDuplicateRegionRollsBackOnMissingBlock() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  assert(!Cfg.duplicateRegion({0, 99}).has_value());
  assert(Cfg.blocks().size() == 1);
  const CFGBlock *Original = Cfg.getBlock(0);
  assert(Original != nullptr);
  assert(Original->Origin == CFGBlockOrigin::Original);
  assert(Original->SourceBlock == 0);
  assert(Original->CopyKind == CFGBlockCopyKind::None);
  assert(Original->CreatedBy == CFGBlockCreator::Input);
  assert(Original->BodyBlock == 0);
  assert(Original->BodyMaterialized);
  assert(Original->Successors == std::vector<BlockId>{1});
}

void testStructuredCFGCreateSyntheticBlock() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(10, {}));

  BlockId Synthetic = Cfg.createSyntheticBlock({10});
  const CFGBlock *Block = Cfg.getBlock(Synthetic);
  assert(Block != nullptr);
  assert(Block->Id == Synthetic);
  assert(Block->Origin == CFGBlockOrigin::Synthetic);
  assert(Block->SourceBlock == Synthetic);
  assert(Block->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(Block->CreatedBy == CFGBlockCreator::StructuredCFG);
  assert(Block->BodyMaterialized);
  assert(Block->BodyBlock == Synthetic);
  assert(Block->Statements.empty());
  assert(Block->Terminator == TerminatorKind::Fallthrough);
  assert(Block->Successors == std::vector<BlockId>{10});
  assert(Block->SyntheticSource == InvalidBlockId);
  assert(Block->SyntheticTarget == InvalidBlockId);

  BlockId Forwarder = Cfg.createSyntheticForwarder(20, 10);
  const CFGBlock *ForwarderBlock = Cfg.getBlock(Forwarder);
  assert(ForwarderBlock != nullptr);
  assert(ForwarderBlock->Origin == CFGBlockOrigin::Synthetic);
  assert(ForwarderBlock->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(ForwarderBlock->Successors == std::vector<BlockId>{10});
  assert(ForwarderBlock->SyntheticSource == 20);
  assert(ForwarderBlock->SyntheticTarget == 10);

  BlockId Goto = Cfg.createSyntheticGoto(30, 10);
  const CFGBlock *GotoBlock = Cfg.getBlock(Goto);
  assert(GotoBlock != nullptr);
  assert(GotoBlock->Origin == CFGBlockOrigin::Synthetic);
  assert(GotoBlock->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(GotoBlock->Successors.empty());
  assert(GotoBlock->SyntheticSource == 30);
  assert(GotoBlock->SyntheticTarget == 10);

  BlockId GotoEdge = Cfg.createSyntheticGotoEdge(40, 10);
  const CFGBlock *GotoEdgeBlock = Cfg.getBlock(GotoEdge);
  assert(GotoEdgeBlock != nullptr);
  assert(GotoEdgeBlock->Origin == CFGBlockOrigin::Synthetic);
  assert(GotoEdgeBlock->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(GotoEdgeBlock->Successors == std::vector<BlockId>{10});
  assert(GotoEdgeBlock->SyntheticSource == 40);
  assert(GotoEdgeBlock->SyntheticTarget == 10);
}

void testCrossJumpReverterDuplicatesLinearGotoTarget() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({7});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {4});
  Tail.Statements.push_back({8});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  StructuringOptimizationOptions Options = CrossJumpReverter::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.StrictlyLessGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  CrossJumpReverter Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  assert(Result.Output.getBlock(1) == nullptr);
  assert(Result.Output.getBlock(2) == nullptr);
  assert(Result.Output.getBlock(4) == nullptr);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block3 = Result.Output.getBlock(3);
  assert(Block0 != nullptr && Block0->Successors.size() == 1);
  assert(Block3 != nullptr && Block3->Successors.size() == 1);
  assert(Block0->Successors.front() != 1);
  assert(Block3->Successors.front() != 1);
  assert(Block0->Successors.front() != Block3->Successors.front());

  const CFGBlock *Copy0 = Result.Output.getBlock(Block0->Successors.front());
  const CFGBlock *Copy3 = Result.Output.getBlock(Block3->Successors.front());
  assert(Copy0 != nullptr && Copy3 != nullptr);
  assert(Copy0->Successors.size() == 1);
  assert(Copy3->Successors.size() == 1);
  assert(Copy0->Successors.front() != 2);
  assert(Copy3->Successors.front() != 2);
  assert(Copy0->Successors.front() != Copy3->Successors.front());
  assert(Copy0->BodyBlock == Copy0->Id);
  assert(Copy3->BodyBlock == Copy3->Id);
  assert(hasSinglePayload(Copy0->Statements, 7));
  assert(hasSinglePayload(Copy3->Statements, 7));

  const CFGBlock *TailCopy0 = Result.Output.getBlock(Copy0->Successors.front());
  const CFGBlock *TailCopy3 = Result.Output.getBlock(Copy3->Successors.front());
  assert(TailCopy0 != nullptr && TailCopy3 != nullptr);
  assert(TailCopy0->Successors.size() == 1);
  assert(TailCopy3->Successors.size() == 1);
  assert(TailCopy0->Successors.front() != 4);
  assert(TailCopy3->Successors.front() != 4);
  assert(TailCopy0->Successors.front() != TailCopy3->Successors.front());
  assert(TailCopy0->BodyBlock == TailCopy0->Id);
  assert(TailCopy3->BodyBlock == TailCopy3->Id);
  assert(hasSinglePayload(TailCopy0->Statements, 8));
  assert(hasSinglePayload(TailCopy3->Statements, 8));

  const CFGBlock *ExitCopy0 =
      Result.Output.getBlock(TailCopy0->Successors.front());
  const CFGBlock *ExitCopy3 =
      Result.Output.getBlock(TailCopy3->Successors.front());
  assert(ExitCopy0 != nullptr && ExitCopy3 != nullptr);
  assert(ExitCopy0->Successors.empty());
  assert(ExitCopy3->Successors.empty());
}

void testCrossJumpReverterCommitsCopyAtomically() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({27});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {});
  Tail.Statements.push_back({28});
  Cfg.addBlock(std::move(Tail));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source0;
  Source0.Kind = StructuredNodeKind::BasicBlock;
  Source0.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source0)));

  StructuredNode Goto0;
  Goto0.Kind = StructuredNodeKind::Goto;
  Goto0.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto0)));

  StructuredNode Source3;
  Source3.Kind = StructuredNodeKind::BasicBlock;
  Source3.Block = 3;
  Root.Children.push_back(Tree.addNode(std::move(Source3)));

  StructuredNode Goto3;
  Goto3.Kind = StructuredNodeKind::Goto;
  Goto3.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto3)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(1) == nullptr);
  assert(Cfg.getBlock(2) == nullptr);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block0 != nullptr && Block3 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block3->Successors.size() == 1);
  assert(Block0->Successors.front() != 1);
  assert(Block3->Successors.front() != 1);
  assert(hasSinglePayload(Cfg.getBlock(Block0->Successors.front())->Statements,
                          27));
}

void testCrossJumpReverterCopiesConnectedPredsOnce() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {3, 1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({17});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {});
  Tail.Statements.push_back({18});
  Cfg.addBlock(std::move(Tail));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source0;
  Source0.Kind = StructuredNodeKind::BasicBlock;
  Source0.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source0)));

  StructuredNode Goto0;
  Goto0.Kind = StructuredNodeKind::Goto;
  Goto0.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto0)));

  StructuredNode Source3;
  Source3.Kind = StructuredNodeKind::BasicBlock;
  Source3.Block = 3;
  Root.Children.push_back(Tree.addNode(std::move(Source3)));

  StructuredNode Goto3;
  Goto3.Kind = StructuredNodeKind::Goto;
  Goto3.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto3)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(1) == nullptr);
  assert(Cfg.getBlock(2) == nullptr);

  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block0 != nullptr && Block3 != nullptr);
  assert(Block0->Successors.size() == 2);
  assert(Block3->Successors.size() == 1);
  assert(Block0->Successors[0] == 3);
  assert(Block0->Successors[1] == Block3->Successors.front());

  const CFGBlock *CopyHead = Cfg.getBlock(Block3->Successors.front());
  assert(CopyHead != nullptr);
  assert(CopyHead->Successors.size() == 1);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(hasSinglePayload(CopyHead->Statements, 17));

  const CFGBlock *CopyTail = Cfg.getBlock(CopyHead->Successors.front());
  assert(CopyTail != nullptr);
  assert(CopyTail->Successors.empty());
  assert(CopyTail->BodyBlock == CopyTail->Id);
  assert(hasSinglePayload(CopyTail->Statements, 18));
}

void testCrossJumpReverterKeepsPredSensitiveCopiesSeparate() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {3, 1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({17});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {});
  Tail.Statements.push_back({18});
  Cfg.addBlock(std::move(Tail));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        assert(Context.OriginalPredecessor != InvalidBlockId);
        assert(Context.NewPredecessor != InvalidBlockId);
        assert(Context.OriginalPredecessors.size() == 1);
        assert(Context.NewPredecessors.size() == 1);
        return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
      },
      /*SupportsPredecessorRewrite=*/true);

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source0;
  Source0.Kind = StructuredNodeKind::BasicBlock;
  Source0.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source0)));

  StructuredNode Goto0;
  Goto0.Kind = StructuredNodeKind::Goto;
  Goto0.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto0)));

  StructuredNode Source3;
  Source3.Kind = StructuredNodeKind::BasicBlock;
  Source3.Block = 3;
  Root.Children.push_back(Tree.addNode(std::move(Source3)));

  StructuredNode Goto3;
  Goto3.Kind = StructuredNodeKind::Goto;
  Goto3.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto3)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(1) == nullptr);
  assert(Cfg.getBlock(2) == nullptr);

  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block0 != nullptr && Block3 != nullptr);
  assert(Block0->Successors.size() == 2);
  assert(Block3->Successors.size() == 1);
  assert(Block0->Successors.front() == 3);
  assert(Block0->Successors.back() != Block3->Successors.front());

  const CFGBlock *Copy0 = Cfg.getBlock(Block0->Successors.back());
  const CFGBlock *Copy3 = Cfg.getBlock(Block3->Successors.front());
  assert(Copy0 != nullptr && Copy3 != nullptr);
  assert(Copy0->SourceBlock == 1);
  assert(Copy3->SourceBlock == 1);
  assert(hasSinglePayload(Copy0->Statements, 17 + 0 * 1000));
  assert(hasSinglePayload(Copy3->Statements, 17 + 3 * 1000));

  const CFGBlock *Tail0 = Cfg.getBlock(Copy0->Successors.front());
  const CFGBlock *Tail3 = Cfg.getBlock(Copy3->Successors.front());
  assert(Tail0 != nullptr && Tail3 != nullptr);
  assert(Tail0->SourceBlock == 2);
  assert(Tail3->SourceBlock == 2);
  assert(hasSinglePayload(Tail0->Statements, 18 + Copy0->Id * 1000));
  assert(hasSinglePayload(Tail3->Statements, 18 + Copy3->Id * 1000));
}

void testCrossJumpReverterSkipsAmbiguousSwitchCaseDefaultTarget() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {1});
  Switch.Cases.push_back({{}, 1});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({51});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {4});
  Tail.Statements.push_back({52});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(4, {}));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source;
  Source.Kind = StructuredNodeKind::BasicBlock;
  Source.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source)));

  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *SwitchBlock = Cfg.getBlock(0);
  assert(SwitchBlock != nullptr);
  assert(SwitchBlock->Successors == std::vector<BlockId>{1});
  assert(SwitchBlock->Cases.size() == 1);
  assert(SwitchBlock->Cases.front().Target == 1);
}

void testCrossJumpReverterRedirectsSwitchCasesOnly() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9});
  Switch.Cases.push_back({{}, 1});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({51});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {4});
  Tail.Statements.push_back({52});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(9, {}));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source;
  Source.Kind = StructuredNodeKind::BasicBlock;
  Source.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source)));

  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *SwitchBlock = Cfg.getBlock(0);
  assert(SwitchBlock != nullptr);
  assert(SwitchBlock->Successors == std::vector<BlockId>{9});
  assert(SwitchBlock->Cases.size() == 1);
  assert(SwitchBlock->Cases.front().Target != 1);
  assert(Cfg.getBlock(1) == nullptr);
  assert(Cfg.getBlock(2) == nullptr);

  const CFGBlock *CopyHead = Cfg.getBlock(SwitchBlock->Cases.front().Target);
  assert(CopyHead != nullptr);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(hasSinglePayload(CopyHead->Statements, 51));
  assert(CopyHead->Successors.size() == 1);
  assert(CopyHead->Successors.front() != 2);

  const CFGBlock *CopyTail = Cfg.getBlock(CopyHead->Successors.front());
  assert(CopyTail != nullptr);
  assert(CopyTail->BodyBlock == CopyTail->Id);
  assert(hasSinglePayload(CopyTail->Statements, 52));
  assert(CopyTail->Successors.size() == 1);
  assert(CopyTail->Successors.front() != 4);

  const CFGBlock *CopyExit = Cfg.getBlock(CopyTail->Successors.front());
  assert(CopyExit != nullptr);
  assert(CopyExit->Successors.empty());
}

void testCrossJumpReverterUsesSwitchCaseGotoKind() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {1});
  Switch.Cases.push_back({{}, 1});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({61});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {4});
  Tail.Statements.push_back({62});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(4, {}));

  StructuringEvaluation Current;
  Current.Gotos =
      GotoManager::fromGotos({StructuredGoto{
          0, 1, InvalidNodeId, StructuredGotoEdgeKind::SwitchCase}});

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *SwitchBlock = Cfg.getBlock(0);
  assert(SwitchBlock != nullptr);
  assert(SwitchBlock->Successors == std::vector<BlockId>{1});
  assert(SwitchBlock->Cases.size() == 1);
  assert(SwitchBlock->Cases.front().Target != 1);
  assert(Cfg.getBlock(1) != nullptr);

  const CFGBlock *CopyHead = Cfg.getBlock(SwitchBlock->Cases.front().Target);
  assert(CopyHead != nullptr);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(hasSinglePayload(CopyHead->Statements, 61));
}

void testCrossJumpReverterUsesSwitchDefaultGotoKind() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {1, 1}));

  CFGBlock Target = block(1, {2});
  Target.Statements.push_back({71});
  Cfg.addBlock(std::move(Target));

  CFGBlock Tail = block(2, {4});
  Tail.Statements.push_back({72});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(4, {}));

  StructuredTree Tree;

  StructuredNode DefaultGoto;
  DefaultGoto.Kind = StructuredNodeKind::Goto;
  DefaultGoto.Target = 1;
  NodeId DefaultId = Tree.addNode(std::move(DefaultGoto));

  StructuredNode CaseBody;
  CaseBody.Kind = StructuredNodeKind::BasicBlock;
  CaseBody.Block = 1;
  NodeId CaseId = Tree.addNode(std::move(CaseBody));

  StructuredNode SwitchNode;
  SwitchNode.Kind = StructuredNodeKind::Switch;
  SwitchNode.Block = 0;
  SwitchNode.Default = DefaultId;
  SwitchNode.StructuredCases.push_back({{}, 1, CaseId});
  Tree.setRoot(Tree.addNode(std::move(SwitchNode)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestCrossJumpReverter Pass(CrossJumpReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *SwitchBlock = Cfg.getBlock(0);
  assert(SwitchBlock != nullptr);
  assert(SwitchBlock->Successors.size() == 2);
  assert(SwitchBlock->Successors.front() != 1);
  assert(SwitchBlock->Successors[1] == 1);
  assert(SwitchBlock->Cases.size() == 1);
  assert(SwitchBlock->Cases.front().Target == 1);
  assert(Cfg.getBlock(1) != nullptr);

  const CFGBlock *CopyHead = Cfg.getBlock(SwitchBlock->Successors.front());
  assert(CopyHead != nullptr);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(hasSinglePayload(CopyHead->Statements, 71));
}

void testDuplicationReverterMergesExactDuplicateBlocks() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  CFGBlock *Block1 = Cfg.getBlock(1);
  CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block1 != nullptr && Block3 != nullptr);
  Block1->Statements.push_back({7});
  Block3->Statements.push_back({7});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(3) == nullptr);

  const CFGBlock *NewBlock0 = Cfg.getBlock(0);
  const CFGBlock *NewBlock2 = Cfg.getBlock(2);
  const CFGBlock *NewBlock1 = Cfg.getBlock(1);
  assert(NewBlock0 != nullptr && NewBlock2 != nullptr && NewBlock1 != nullptr);
  assert(NewBlock0->Successors == std::vector<BlockId>{1});
  assert(NewBlock2->Successors == std::vector<BlockId>{1});
  assert(NewBlock1->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(NewBlock1->Statements, 7));
}

void testDuplicationReverterMatchesTrueAGraphDeduplication() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  CFGBlock *Block1 = Cfg.getBlock(1);
  CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block1 != nullptr && Block3 != nullptr);
  Block1->Statements.push_back({71});
  Block3->Statements.push_back({71});
  Block1->BodyMaterialized = true;
  Block3->BodyMaterialized = true;

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(3) == nullptr);

  const CFGBlock *Merged = Cfg.getBlock(1);
  assert(Merged != nullptr);
  assert(Merged->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(Merged->Statements, 71));
}

void testDuplicationReverterKeepsProgrammerWrittenDuplication() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));
  Cfg.addBlock(block(1, {3}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {}));

  CFGBlock *Block1 = Cfg.getBlock(1);
  CFGBlock *Block2 = Cfg.getBlock(2);
  assert(Block1 != nullptr && Block2 != nullptr);
  Block1->Statements.push_back({81});
  Block2->Statements.push_back({81});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(2) != nullptr);
  std::vector<BlockId> ExpectedSuccs = {1, 2};
  assert(Cfg.getBlock(0)->Successors == ExpectedSuccs);
}

void testDuplicationReverterMergesDuplicatedTailProxyShape() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  CFGBlock *Block1 = Cfg.getBlock(1);
  CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block1 != nullptr && Block3 != nullptr);
  Block1->Statements.push_back({71});
  Block3->Statements.push_back({71});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(3) == nullptr);
  const CFGBlock *Merged = Cfg.getBlock(1);
  assert(Merged != nullptr);
  assert(Merged->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(Merged->Statements, 71));
}

void testDuplicationReverterCommitsMergeAtomically() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {}));

  CFGBlock *Block1 = Cfg.getBlock(1);
  CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block1 != nullptr && Block3 != nullptr);
  Block1->Statements.push_back({8});
  Block3->Statements.push_back({8});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(3) == nullptr);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(0) != nullptr);
  assert(Cfg.getBlock(2) != nullptr);
  assert(Cfg.getBlock(0)->Successors == std::vector<BlockId>{1});
  assert(Cfg.getBlock(2)->Successors == std::vector<BlockId>{1});
  assert(hasSinglePayload(Cfg.getBlock(1)->Statements, 8));
}

void testDuplicationReverterRedirectsSwitchPredecessorCases() {
  StructuredCFG Cfg;

  CFGBlock Switch = switchBlock(0, {9});
  Switch.Cases.push_back({{}, 2});
  Cfg.addBlock(std::move(Switch));

  CFGBlock Keep = block(1, {4});
  Keep.Statements.push_back({7});
  Cfg.addBlock(std::move(Keep));

  CFGBlock Drop = block(2, {4});
  Drop.Statements.push_back({7});
  Cfg.addBlock(std::move(Drop));

  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(9, {}));

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(2) == nullptr);

  const CFGBlock *SwitchBlock = Cfg.getBlock(0);
  assert(SwitchBlock != nullptr);
  assert(SwitchBlock->Successors == std::vector<BlockId>{9});
  assert(SwitchBlock->Cases.size() == 1);
  assert(SwitchBlock->Cases.front().Target == 1);
}

void testDuplicationReverterSkipsWhenDropCannotBeRemoved() {
  StructuredCFG Cfg;

  Cfg.addBlock(block(0, {1}));
  CFGBlock Keep = block(1, {4});
  Keep.Statements.push_back({7});
  Cfg.addBlock(std::move(Keep));

  Cfg.addBlock(block(2, {3}));
  CFGBlock Drop = switchBlock(3, {4, 5});
  Drop.Statements.push_back({7});
  Drop.Cases.front().Value = {71};
  Cfg.addBlock(std::move(Drop));

  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(5, {}));

  CFGBlock BadCopy = switchBlock(20, {4});
  BadCopy.Origin = CFGBlockOrigin::Copied;
  BadCopy.SourceBlock = 3;
  BadCopy.CopyKind = CFGBlockCopyKind::RegionCopy;
  BadCopy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  BadCopy.BodyBlock = 3;
  BadCopy.BodyMaterialized = false;
  Cfg.addBlock(std::move(BadCopy));

  CFGBlock *KeepBlock = Cfg.getBlock(1);
  CFGBlock *DropBlock = Cfg.getBlock(3);
  assert(KeepBlock != nullptr && DropBlock != nullptr);
  KeepBlock->Terminator = DropBlock->Terminator;
  KeepBlock->Condition = DropBlock->Condition;
  KeepBlock->Successors = DropBlock->Successors;
  KeepBlock->Cases = DropBlock->Cases;

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(3) != nullptr);
  assert(Cfg.getBlock(2)->Successors == std::vector<BlockId>{3});
  const CFGBlock *BadCopyBlock = Cfg.getBlock(20);
  assert(BadCopyBlock != nullptr);
  assert(!BadCopyBlock->BodyMaterialized);
  assert(BadCopyBlock->BodyBlock == 3);
}

void testDuplicationReverterKeepsSyntheticIdentitySeparate() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {3}));
  BlockId Synthetic = Cfg.createSyntheticBlock({4});
  assert(Synthetic == 3);
  Cfg.addBlock(block(4, {}));

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *Original = Cfg.getBlock(1);
  const CFGBlock *Forwarder = Cfg.getBlock(Synthetic);
  assert(Original != nullptr && Forwarder != nullptr);
  assert(Original->Origin == CFGBlockOrigin::Original);
  assert(Forwarder->Origin == CFGBlockOrigin::Synthetic);
  assert(Original->Successors == std::vector<BlockId>{4});
  assert(Forwarder->Successors == std::vector<BlockId>{4});
}

void testDuplicationReverterKeepsCopiedSourcesSeparate() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Source1 = block(1, {5});
  Source1.Statements.push_back({7});
  Cfg.addBlock(std::move(Source1));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Source3 = block(3, {5});
  Source3.Statements.push_back({7});
  Cfg.addBlock(std::move(Source3));

  BlockId Copy1 = Cfg.duplicateBlock(1, {5});
  BlockId Copy3 = Cfg.duplicateBlock(3, {5});
  assert(Copy1 != InvalidBlockId && Copy3 != InvalidBlockId);
  assert(Cfg.materializeBlockBody(Copy1));
  assert(Cfg.materializeBlockBody(Copy3));

  CFGBlock *Pred0 = Cfg.getBlock(0);
  CFGBlock *Pred2 = Cfg.getBlock(2);
  assert(Pred0 != nullptr && Pred2 != nullptr);
  Pred0->Successors = {Copy1};
  Pred2->Successors = {Copy3};

  Cfg.addBlock(block(5, {}));

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *FirstCopy = Cfg.getBlock(Copy1);
  const CFGBlock *SecondCopy = Cfg.getBlock(Copy3);
  assert(FirstCopy != nullptr && SecondCopy != nullptr);
  assert(FirstCopy->Origin == CFGBlockOrigin::Copied);
  assert(SecondCopy->Origin == CFGBlockOrigin::Copied);
  assert(FirstCopy->SourceBlock == 1);
  assert(SecondCopy->SourceBlock == 3);
  assert(FirstCopy->Successors == std::vector<BlockId>{5});
  assert(SecondCopy->Successors == std::vector<BlockId>{5});
}

void testDuplicationReverterFiltersFutureIrreducibleGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  CFGBlock CycleA = block(3, {4});
  CycleA.Terminator = TerminatorKind::Fallthrough;
  Cfg.addBlock(std::move(CycleA));

  CFGBlock CycleB = block(4, {3});
  CycleB.Terminator = TerminatorKind::Fallthrough;
  Cfg.addBlock(std::move(CycleB));

  StructuringEvaluation Initial;
  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({
      {0, 1, 10},
      {0, 3, 11},
  });

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  GotoManager Filtered = Pass.getNewGotos(Cfg, Initial, Current);

  assert(Filtered.size() == 1);
  assert(Filtered.isGotoEdge(0, 1));
  assert(!Filtered.isGotoEdge(0, 3));
}

void testDuplicationReverterKeepsGotosWithinEndpointCutoff() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {3}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {6}));
  Cfg.addBlock(block(6, {7}));
  Cfg.addBlock(block(7, {}));

  StructuringEvaluation Initial;
  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({
      {0, 1, 10},
      {0, 2, 11},
  });

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  GotoManager Filtered = Pass.getNewGotos(Cfg, Initial, Current);

  assert(Filtered.size() == 1);
  assert(!Filtered.isGotoEdge(0, 1));
  assert(Filtered.isGotoEdge(0, 2));
}

void testDuplicationReverterKeepsValidEndGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(2, {}));

  StructuringEvaluation Initial;
  Initial.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}, StructuredGoto{1, 2}});

  StructuringEvaluation Current = Initial;

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  GotoManager Filtered = Pass.getNewGotos(Cfg, Initial, Current);

  assert(Filtered.size() == 2);
  assert(Filtered.isGotoEdge(0, 1));
  assert(Filtered.isGotoEdge(1, 2));
}

void testDuplicationReverterSeparatesWrittenAndMergeableDuplication() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));

  CFGBlock Mergeable1 = block(1, {3});
  Mergeable1.Statements.push_back({91});
  Cfg.addBlock(std::move(Mergeable1));

  CFGBlock Mergeable2 = block(2, {3});
  Mergeable2.Statements.push_back({91});
  Cfg.addBlock(std::move(Mergeable2));

  Cfg.addBlock(block(3, {}));

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(2) != nullptr);
  std::vector<BlockId> ExpectedSuccs = {1, 2};
  assert(Cfg.getBlock(0)->Successors == ExpectedSuccs);
}

void testDuplicationReverterKeepsPayloadDivergentDuplicateBranches() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1, 2}));

  CFGBlock ThenBlock = block(1, {3});
  ThenBlock.Statements.push_back({101});
  Cfg.addBlock(std::move(ThenBlock));

  CFGBlock ElseBlock = block(2, {3});
  ElseBlock.Statements.push_back({102});
  Cfg.addBlock(std::move(ElseBlock));

  Cfg.addBlock(block(3, {}));

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(2) != nullptr);
  assert(hasSinglePayload(Cfg.getBlock(1)->Statements, 101));
  assert(hasSinglePayload(Cfg.getBlock(2)->Statements, 102));
}

void testDuplicationReverterExtractsGotoRelatedCommonStatementTail() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Left = block(1, {5});
  Left.Statements.push_back({101});
  Left.Statements.push_back({201});
  Cfg.addBlock(std::move(Left));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Right = block(3, {5});
  Right.Statements.push_back({102});
  Right.Statements.push_back({201});
  Cfg.addBlock(std::move(Right));

  Cfg.addBlock(block(5, {}));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *LeftAfter = Cfg.getBlock(1);
  const CFGBlock *RightAfter = Cfg.getBlock(3);
  assert(LeftAfter != nullptr && RightAfter != nullptr);
  assert(hasSinglePayload(LeftAfter->Statements, 101));
  assert(hasSinglePayload(RightAfter->Statements, 102));
  assert(LeftAfter->Successors.size() == 1);
  assert(LeftAfter->Successors == RightAfter->Successors);

  const CFGBlock *Tail = Cfg.getBlock(LeftAfter->Successors.front());
  assert(Tail != nullptr);
  assert(Tail->Origin == CFGBlockOrigin::Synthetic);
  assert(Tail->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(hasSinglePayload(Tail->Statements, 201));
  assert(Tail->Successors == std::vector<BlockId>{5});
}

void testDuplicationReverterKeepsCommonStatementTailWithoutGotoHint() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Left = block(1, {5});
  Left.Statements.push_back({101});
  Left.Statements.push_back({201});
  Cfg.addBlock(std::move(Left));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Right = block(3, {5});
  Right.Statements.push_back({102});
  Right.Statements.push_back({201});
  Cfg.addBlock(std::move(Right));

  Cfg.addBlock(block(5, {}));

  StructuringEvaluation Current;

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(1)->Successors == std::vector<BlockId>{5});
  assert(Cfg.getBlock(3)->Successors == std::vector<BlockId>{5});
  assert(Cfg.getBlock(1)->Statements.size() == 2);
  assert(Cfg.getBlock(3)->Statements.size() == 2);
}

void testDuplicationReverterSkipsCommonTailWhenRegionIsNotLinear() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Left = block(1, {5});
  Left.Statements.push_back({101});
  Left.Statements.push_back({201});
  Cfg.addBlock(std::move(Left));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Right = branchBlock(3, {5, 1});
  Right.Statements.push_back({102});
  Right.Statements.push_back({201});
  Cfg.addBlock(std::move(Right));

  Cfg.addBlock(block(5, {}));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(1)->Successors == std::vector<BlockId>{5});
  assert(Cfg.getBlock(3)->Terminator == TerminatorKind::Branch);
  assert(Cfg.getBlock(3)->Successors == std::vector<BlockId>({5, 1}));
}

void testDuplicationReverterMergesGotoRelatedLinearRegionTail() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Left0 = block(1, {4});
  Left0.Statements.push_back({101});
  Cfg.addBlock(std::move(Left0));

  CFGBlock Left1 = block(4, {5});
  Left1.Statements.push_back({201});
  Left1.Statements.push_back({301});
  Cfg.addBlock(std::move(Left1));

  CFGBlock Left2 = block(5, {8});
  Left2.Statements.push_back({401});
  Cfg.addBlock(std::move(Left2));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Right0 = block(3, {6});
  Right0.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right0.Statements.push_back({102});
  Cfg.addBlock(std::move(Right0));

  CFGBlock Right1 = block(6, {7});
  Right1.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right1.Statements.push_back({201});
  Right1.Statements.push_back({301});
  Cfg.addBlock(std::move(Right1));

  CFGBlock Right2 = block(7, {8});
  Right2.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right2.Statements.push_back({401});
  Cfg.addBlock(std::move(Right2));

  Cfg.addBlock(block(8, {}));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *Left0After = Cfg.getBlock(1);
  const CFGBlock *Left1After = Cfg.getBlock(4);
  const CFGBlock *Right0After = Cfg.getBlock(3);
  assert(Left0After != nullptr);
  assert(Left1After != nullptr);
  assert(Right0After != nullptr);
  assert(hasSinglePayload(Left0After->Statements, 101));
  assert(hasSinglePayload(Right0After->Statements, 102));
  assert(Left1After->Statements.size() == 2);
  assert(Left1After->Statements[0].Id == 201);
  assert(Left1After->Statements[1].Id == 301);
  assert(Left1After->Successors == std::vector<BlockId>{5});
  assert(Cfg.getBlock(3)->Successors == std::vector<BlockId>{4});
  assert(Cfg.getBlock(6) == nullptr);
  assert(Cfg.getBlock(7) == nullptr);
  assert(Cfg.getBlock(5) != nullptr);
  assert(Cfg.getBlock(5)->Successors == std::vector<BlockId>{8});
}

void testDuplicationReverterSkipsGotoRelatedLinearTailWithoutHint() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Left0 = block(1, {4});
  Left0.Statements.push_back({101});
  Cfg.addBlock(std::move(Left0));

  CFGBlock Left1 = block(4, {5});
  Left1.Statements.push_back({201});
  Left1.Statements.push_back({301});
  Cfg.addBlock(std::move(Left1));

  CFGBlock Left2 = block(5, {8});
  Left2.Statements.push_back({401});
  Cfg.addBlock(std::move(Left2));

  Cfg.addBlock(block(2, {3}));

  CFGBlock Right0 = block(3, {6});
  Right0.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right0.Statements.push_back({102});
  Cfg.addBlock(std::move(Right0));

  CFGBlock Right1 = block(6, {7});
  Right1.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right1.Statements.push_back({201});
  Right1.Statements.push_back({301});
  Cfg.addBlock(std::move(Right1));

  CFGBlock Right2 = block(7, {8});
  Right2.CreatedBy = CFGBlockCreator::StructuredCFG;
  Right2.Statements.push_back({401});
  Cfg.addBlock(std::move(Right2));

  Cfg.addBlock(block(8, {}));

  StructuringEvaluation Current;

  TestDuplicationReverter Pass(DuplicationReverter::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(6) != nullptr);
  assert(Cfg.getBlock(4)->Successors == std::vector<BlockId>{5});
  assert(Cfg.getBlock(6)->Successors == std::vector<BlockId>{7});
}

void testReturnDuplicatorLowDuplicatesGotoReturnTarget() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Head = block(2, {3});
  Head.Statements.push_back({5});
  Cfg.addBlock(std::move(Head));

  CFGBlock Ret = block(3, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({9});
  Cfg.addBlock(std::move(Ret));

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  assert(Result.Output.getBlock(2) == nullptr);
  assert(Result.Output.getBlock(3) == nullptr);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block1 = Result.Output.getBlock(1);
  assert(Block0 != nullptr && Block0->Successors.size() == 1);
  assert(Block1 != nullptr && Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != 2);
  assert(Block1->Successors.front() != 2);
  assert(Block0->Successors.front() != Block1->Successors.front());

  const CFGBlock *CopyHead0 = Result.Output.getBlock(Block0->Successors.front());
  const CFGBlock *CopyHead1 = Result.Output.getBlock(Block1->Successors.front());
  assert(CopyHead0 != nullptr && CopyHead1 != nullptr);
  assert(CopyHead0->Successors.size() == 1);
  assert(CopyHead1->Successors.size() == 1);
  assert(CopyHead0->BodyBlock == CopyHead0->Id);
  assert(CopyHead1->BodyBlock == CopyHead1->Id);
  assert(hasSinglePayload(CopyHead0->Statements, 5));
  assert(hasSinglePayload(CopyHead1->Statements, 5));

  const CFGBlock *CopyRet0 = Result.Output.getBlock(CopyHead0->Successors.front());
  const CFGBlock *CopyRet1 = Result.Output.getBlock(CopyHead1->Successors.front());
  assert(CopyRet0 != nullptr && CopyRet1 != nullptr);
  assert(CopyRet0->Terminator == TerminatorKind::Return);
  assert(CopyRet1->Terminator == TerminatorKind::Return);
  assert(CopyRet0->BodyBlock == CopyRet0->Id);
  assert(CopyRet1->BodyBlock == CopyRet1->Id);
  assert(hasSinglePayload(CopyRet0->Statements, 9));
  assert(hasSinglePayload(CopyRet1->Statements, 9));
}

void testReturnDuplicatorLowSkipsLargeFunction() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Ret = block(2, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({9});
  Cfg.addBlock(std::move(Ret));

  Cfg.addBlock(block(3, {}));

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions(),
                               /*MaxFunctionBlocks=*/3);
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(0)->Successors == std::vector<BlockId>{2});
  assert(Cfg.getBlock(1)->Successors == std::vector<BlockId>{2});
  assert(Cfg.getBlock(2) != nullptr);
}

void testReturnDuplicatorLowCopiesConnectedPredsOnce() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({13});
  Cfg.addBlock(std::move(Ret));

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  assert(Result.Output.getBlock(1) == nullptr);
  assert(Result.Output.blocks().size() == 3);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block2 = Result.Output.getBlock(2);
  assert(Block0 != nullptr && Block2 != nullptr);
  assert(Block0->Successors.size() == 2);
  assert(Block0->Successors.front() == 2 || Block0->Successors.back() == 2);
  BlockId CopyId = Block0->Successors.front() == 2 ? Block0->Successors.back()
                                                   : Block0->Successors.front();
  assert(Block2->Successors == std::vector<BlockId>{CopyId});

  const CFGBlock *Copy = Result.Output.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Terminator == TerminatorKind::Return);
  assert(Copy->BodyBlock == Copy->Id);
  assert(hasSinglePayload(Copy->Statements, 13));
}

void testReturnDuplicatorLowExpandsGotoPredToConnectedComponent() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({14});
  Cfg.addBlock(std::move(Ret));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(1) != nullptr);

  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  const CFGBlock *Block4 = Cfg.getBlock(4);
  assert(Block0 != nullptr && Block2 != nullptr && Block3 != nullptr &&
         Block4 != nullptr);
  assert(Block0->Successors.size() == 2);
  assert(Block0->Successors.front() == 2 || Block0->Successors.back() == 2);
  BlockId CopyId = Block0->Successors.front() == 2 ? Block0->Successors.back()
                                                   : Block0->Successors.front();
  assert(CopyId != 1);
  assert(Block2->Successors == std::vector<BlockId>{CopyId});
  assert(Block3->Successors == std::vector<BlockId>{1});
  assert(Block4->Successors == std::vector<BlockId>{1});

  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Terminator == TerminatorKind::Return);
  assert(Copy->BodyBlock == Copy->Id);
  assert(hasSinglePayload(Copy->Statements, 14));
}

void testReturnDuplicatorLowUsesSwitchCaseGotoSource() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {3, 1, 2}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {}));

  CFGBlock Branch = branchBlock(4, {5, 6});
  Branch.Condition = {40};
  Cfg.addBlock(std::move(Branch));

  CFGBlock ThenRet = block(5, {});
  ThenRet.Terminator = TerminatorKind::Return;
  ThenRet.Statements.push_back({50});
  Cfg.addBlock(std::move(ThenRet));

  CFGBlock ElseRet = block(6, {});
  ElseRet.Terminator = TerminatorKind::Return;
  ElseRet.Statements.push_back({60});
  Cfg.addBlock(std::move(ElseRet));

  StructuringEvaluation Current;
  Current.Gotos =
      GotoManager::fromGotos({StructuredGoto{
          1, 4, InvalidNodeId, StructuredGotoEdgeKind::SwitchCase}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Case1 = Cfg.getBlock(1);
  const CFGBlock *Case2 = Cfg.getBlock(2);
  assert(Case1 != nullptr && Case2 != nullptr);
  assert(Case1->Successors.size() == 1);
  assert(Case2->Successors.size() == 1);
  assert(Case1->Successors.front() != 4);
  assert(Case2->Successors.front() == 4);

  const CFGBlock *CopyBranch = Cfg.getBlock(Case1->Successors.front());
  assert(CopyBranch != nullptr);
  assert(CopyBranch->SourceBlock == 4);
  assert(CopyBranch->BodyMaterialized);
  assert(CopyBranch->BodyBlock == CopyBranch->Id);
  assert(CopyBranch->Terminator == TerminatorKind::Branch);
}

void testReturnDuplicatorLowNormalizesCopiedGotoTargets() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  CFGBlock Target = block(1, {});
  Target.Terminator = TerminatorKind::Return;
  Cfg.addBlock(std::move(Target));

  BlockId Copy = Cfg.duplicateBlock(1, {});
  assert(Copy != InvalidBlockId);

  StructuringEvaluation Initial;
  Initial.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, Copy}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  GotoManager Filtered = Pass.getNewGotos(Cfg, Initial, Current);

  assert(Filtered.size() == 1);
  assert(Filtered.isGotoEdge(0, 1));
  assert(!Filtered.isGotoEdge(0, Copy));
}

void testReturnDuplicatorLowAcceptsCopiedSwitchCaseReturnRegion() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {3, 1, 2}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {}));

  CFGBlock Branch = branchBlock(4, {5, 6});
  Branch.Condition = {40};
  Cfg.addBlock(std::move(Branch));

  CFGBlock ThenRet = block(5, {});
  ThenRet.Terminator = TerminatorKind::Return;
  ThenRet.Statements.push_back({50});
  Cfg.addBlock(std::move(ThenRet));

  CFGBlock ElseRet = block(6, {});
  ElseRet.Terminator = TerminatorKind::Return;
  ElseRet.Statements.push_back({60});
  Cfg.addBlock(std::move(ElseRet));

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = true;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  const CFGBlock *Case1 = Result.Output.getBlock(1);
  const CFGBlock *Case2 = Result.Output.getBlock(2);
  assert(Case1 != nullptr && Case2 != nullptr);
  assert(Case1->Successors.size() == 1);
  assert(Case2->Successors.size() == 1);
  assert(Case1->Successors.front() != 4);
  assert(Case2->Successors.front() != 4);
  assert(Case1->Successors.front() != Case2->Successors.front());
  assert(Result.Output.getBlock(4) == nullptr);
}

void testReturnDuplicatorLowReportsGroupedPredecessorRewrite() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({15});
  Cfg.addBlock(std::move(Ret));

  bool SawGroupedPreds = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement) {
          assert(Context.OriginalPredecessors ==
                 std::vector<BlockId>({0, 2}));
          assert(Context.NewPredecessors.size() == 2);
          SawGroupedPreds = true;
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(SawGroupedPreds);
  assert(Cfg.getBlock(1) == nullptr);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  assert(Block0 != nullptr && Block2 != nullptr);
  assert(Block0->Successors.size() == 2);
  BlockId CopyId = Block0->Successors.front() == 2 ? Block0->Successors.back()
                                                   : Block0->Successors.front();
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Terminator == TerminatorKind::Return);
}

void testReturnDuplicatorLowCopiesGroupedReturnPredsWithPayloadRewrite() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({15});
  Cfg.addBlock(std::move(Ret));

  std::vector<std::vector<BlockId>> SeenOriginalGroups;
  std::vector<std::vector<BlockId>> SeenNewGroups;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement) {
          assert(Context.OriginalPredecessors == Context.NewPredecessors);
          SeenOriginalGroups.push_back(Context.OriginalPredecessors);
          SeenNewGroups.push_back(Context.NewPredecessors);
          return PayloadRef{Payload.Id + 100};
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(SeenOriginalGroups.size() == 2);
  assert(SeenNewGroups.size() == 2);
  assert((SeenOriginalGroups[0] == std::vector<BlockId>({0, 2}) ||
          SeenOriginalGroups[1] == std::vector<BlockId>({0, 2})));
  assert((SeenOriginalGroups[0] == std::vector<BlockId>({3}) ||
          SeenOriginalGroups[1] == std::vector<BlockId>({3})));
  assert(SeenOriginalGroups == SeenNewGroups);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block0 != nullptr && Block2 != nullptr && Block3 != nullptr);
  assert(Block0->Successors.size() == 2);
  assert(Block0->Successors.front() == 2 || Block0->Successors.back() == 2);
  assert(Block2->Successors.size() == 1);
  assert(Block3->Successors.size() == 1);
  BlockId Copy0Id = Block0->Successors.front() == 2 ? Block0->Successors.back()
                                                    : Block0->Successors.front();
  const CFGBlock *Copy0 = Cfg.getBlock(Copy0Id);
  const CFGBlock *Copy2 = Cfg.getBlock(Block2->Successors.front());
  const CFGBlock *Copy3 = Cfg.getBlock(Block3->Successors.front());
  assert(Copy0 != nullptr && Copy2 != nullptr && Copy3 != nullptr);
  assert(Copy0->Id == Copy2->Id);
  assert(Copy0->Id != Copy3->Id);
  assert(Copy0->BodyBlock == Copy0->Id);
  assert(Copy2->BodyBlock == Copy2->Id);
  assert(Copy3->BodyBlock == Copy3->Id);
  assert(hasSinglePayload(Copy0->Statements, 115));
  assert(hasSinglePayload(Copy2->Statements, 115));
  assert(hasSinglePayload(Copy3->Statements, 115));
}

void testReturnDuplicatorLowRollsBackGroupedPredecessorFailure() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({15});
  Cfg.addBlock(std::move(Ret));

  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        if (Context.OriginalPredecessors == std::vector<BlockId>({0, 2})) {
          return std::nullopt;
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block2 != nullptr && Block3 != nullptr &&
         Block1 != nullptr);
  assert(Block0->Successors == std::vector<BlockId>({1, 2}));
  assert(Block2->Successors == std::vector<BlockId>({1}));
  assert(Block3->Successors == std::vector<BlockId>({1}));
  assert(Block1->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(Block1->Statements, 15));
}

void testReturnDuplicatorLowSkipsPartialGroupedCopyOnFailure() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 2}));
  Cfg.addBlock(block(2, {1}));
  Cfg.addBlock(block(3, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({15});
  Cfg.addBlock(std::move(Ret));

  bool SawGoodGroup = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement) {
          if (Context.OriginalPredecessors == std::vector<BlockId>({0, 2})) {
            SawGoodGroup = true;
            return PayloadRef{Payload.Id + 100};
          }
          return std::nullopt;
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 1}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(SawGoodGroup);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(0)->Successors == std::vector<BlockId>({1, 2}));
  assert(Cfg.getBlock(2)->Successors == std::vector<BlockId>({1}));
  assert(Cfg.getBlock(3)->Successors == std::vector<BlockId>({1}));
}

void testReturnDuplicatorLowCommitsCopyAtomically() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(2, {1}));

  CFGBlock Ret = block(1, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({23});
  Cfg.addBlock(std::move(Ret));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;
  StructuredNode Source0;
  Source0.Kind = StructuredNodeKind::BasicBlock;
  Source0.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source0)));
  StructuredNode Goto0;
  Goto0.Kind = StructuredNodeKind::Goto;
  Goto0.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto0)));
  StructuredNode Source2;
  Source2.Kind = StructuredNodeKind::BasicBlock;
  Source2.Block = 2;
  Root.Children.push_back(Tree.addNode(std::move(Source2)));
  StructuredNode Goto2;
  Goto2.Kind = StructuredNodeKind::Goto;
  Goto2.Target = 1;
  Root.Children.push_back(Tree.addNode(std::move(Goto2)));
  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  assert(Block0 != nullptr && Block2 != nullptr);
  assert(Block0->Successors.front() != 1);
  assert(Block2->Successors.front() != 1);
  assert(Cfg.getBlock(1) == nullptr);
  const CFGBlock *Copy0 = Cfg.getBlock(Block0->Successors.front());
  const CFGBlock *Copy2 = Cfg.getBlock(Block2->Successors.front());
  assert(Copy0 != nullptr && Copy2 != nullptr);
  assert(Copy0->Terminator == TerminatorKind::Return);
  assert(Copy2->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(Copy0->Statements, 23));
  assert(hasSinglePayload(Copy2->Statements, 23));
}

void testReturnDuplicatorLowUsesParentGotoSource() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(block(3, {2}));

  CFGBlock Ret = block(2, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({15});
  Cfg.addBlock(std::move(Ret));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode Source;
  Source.Kind = StructuredNodeKind::BasicBlock;
  Source.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(Source)));

  StructuredNode Goto;
  Goto.Kind = StructuredNodeKind::Goto;
  Goto.Target = 2;
  Root.Children.push_back(Tree.addNode(std::move(Goto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  const CFGBlock *Block3 = Cfg.getBlock(3);
  assert(Block1 != nullptr && Block3 != nullptr);
  assert(Block1->Successors.size() == 1);
  assert(Block3->Successors == std::vector<BlockId>{2});
  assert(Block1->Successors.front() != 2);

  const CFGBlock *Copy = Cfg.getBlock(Block1->Successors.front());
  assert(Copy != nullptr);
  assert(Copy->Terminator == TerminatorKind::Return);
  assert(Copy->SourceBlock == 2);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == Copy->Id);
  assert(hasSinglePayload(Copy->Statements, 15));

  const CFGBlock *OriginalRet = Cfg.getBlock(2);
  assert(OriginalRet != nullptr);
  assert(hasSinglePayload(OriginalRet->Statements, 15));
}

void testReturnDuplicatorLowSkipsBranchParentGotoSource() {
  StructuredCFG Cfg;
  Cfg.addBlock(branchBlock(0, {1, 5}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(5, {}));

  CFGBlock Ret = block(4, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({16});
  Cfg.addBlock(std::move(Ret));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode BranchParent;
  BranchParent.Kind = StructuredNodeKind::BasicBlock;
  BranchParent.Block = 0;
  Root.Children.push_back(Tree.addNode(std::move(BranchParent)));

  StructuredNode BranchGoto;
  BranchGoto.Kind = StructuredNodeKind::Goto;
  BranchGoto.Target = 4;
  Root.Children.push_back(Tree.addNode(std::move(BranchGoto)));

  StructuredNode DirectSource;
  DirectSource.Kind = StructuredNodeKind::BasicBlock;
  DirectSource.Block = 2;
  Root.Children.push_back(Tree.addNode(std::move(DirectSource)));

  StructuredNode DirectGoto;
  DirectGoto.Kind = StructuredNodeKind::Goto;
  DirectGoto.Target = 4;
  Root.Children.push_back(Tree.addNode(std::move(DirectGoto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  const CFGBlock *Block2 = Cfg.getBlock(2);
  const CFGBlock *OriginalRet = Cfg.getBlock(4);
  assert(Block1 != nullptr && Block2 != nullptr && OriginalRet != nullptr);
  assert(Block1->Successors == std::vector<BlockId>{4});
  assert(Block2->Successors.size() == 1);
  assert(Block2->Successors.front() != 4);

  const CFGBlock *Copy = Cfg.getBlock(Block2->Successors.front());
  assert(Copy != nullptr);
  assert(Copy->Terminator == TerminatorKind::Return);
  assert(Copy->SourceBlock == 4);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == Copy->Id);
  assert(hasSinglePayload(Copy->Statements, 16));
  assert(hasSinglePayload(OriginalRet->Statements, 16));
}

void testReturnDuplicatorLowCopiesReturnRegionWithDephicationVVars() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Head = block(2, {3});
  Head.Statements.push_back({40});
  Cfg.addBlock(std::move(Head));

  CFGBlock Ret = block(3, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({41});
  Cfg.addBlock(std::move(Ret));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 2, 3, 2, {40}, "x");

  VVarId CopiedVVar = InvalidVVarId;
  bool SawDephicationAssignment = false;
  bool SawCopiedReturnStatement = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t) -> std::optional<PayloadRef> {
        assert(Context.DephicationVVarCopies.size() == 1);
        auto CopyIt = Context.DephicationVVarCopies.begin();
        assert(CopyIt->first == VVar);
        if (CopiedVVar == InvalidVVarId) {
          CopiedVVar = CopyIt->second;
        } else {
          assert(CopyIt->second == CopiedVVar);
        }
        assert(Context.DephicationVVars.size() == 1);
        assert(Context.DephicationVVars.front().Id == CopiedVVar);

        if (Kind == PayloadMaterializeKind::DephicationAssignment) {
          SawDephicationAssignment = true;
          assert(Context.CurrentDephicationIncoming.has_value());
          assert(Context.CurrentDephicationIncoming->Target == CopiedVVar);
          assert(Context.CurrentDephicationIncoming->Assignment.Id ==
                 Payload.Id);
          return PayloadRef{Payload.Id + 1000};
        }

        if (Kind == PayloadMaterializeKind::Statement) {
          assert(!Context.CurrentDephicationIncoming.has_value());
          if (Payload.Id == 41) {
            SawCopiedReturnStatement = true;
          }
          return PayloadRef{Payload.Id + 1000};
        }

        assert(!Context.CurrentDephicationIncoming.has_value());
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 2}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(SawDephicationAssignment);
  assert(SawCopiedReturnStatement);
  assert(Cfg.getBlock(2) != nullptr);
  assert(Cfg.getBlock(3) != nullptr);

  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors == std::vector<BlockId>{2});
  assert(Block0->Successors.front() != 2);

  const CFGBlock *CopyHead = Cfg.getBlock(Block0->Successors.front());
  const CFGBlock *CopyRet = nullptr;
  if (CopyHead != nullptr && !CopyHead->Successors.empty()) {
    CopyRet = Cfg.getBlock(CopyHead->Successors.front());
  }
  assert(CopyHead != nullptr && CopyRet != nullptr);
  assert(CopyHead->Terminator == TerminatorKind::Fallthrough);
  assert(CopyRet->Terminator == TerminatorKind::Return);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(CopyRet->BodyBlock == CopyRet->Id);
  assert(hasSinglePayload(CopyHead->Statements, 1040));
  assert(hasSinglePayload(CopyRet->Statements, 1041));
}

void testReturnDuplicatorLowCopiesTerminalForkRegion() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {3, 4}));

  CFGBlock Ret = block(3, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({17});
  Cfg.addBlock(std::move(Ret));

  CFGBlock Trap = block(4, {});
  Trap.Terminator = TerminatorKind::Unreachable;
  Trap.Statements.push_back({19});
  Cfg.addBlock(std::move(Trap));

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  assert(Result.Output.getBlock(2) == nullptr);
  assert(Result.Output.getBlock(3) == nullptr);
  assert(Result.Output.getBlock(4) == nullptr);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block1 = Result.Output.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != Block1->Successors.front());

  const CFGBlock *CopyHead0 = Result.Output.getBlock(Block0->Successors.front());
  const CFGBlock *CopyHead1 = Result.Output.getBlock(Block1->Successors.front());
  assert(CopyHead0 != nullptr && CopyHead1 != nullptr);
  assert(CopyHead0->Terminator == TerminatorKind::Branch);
  assert(CopyHead1->Terminator == TerminatorKind::Branch);
  assert(CopyHead0->Successors.size() == 2);
  assert(CopyHead1->Successors.size() == 2);

  const CFGBlock *CopyRet0 = Result.Output.getBlock(CopyHead0->Successors[0]);
  const CFGBlock *CopyTrap0 = Result.Output.getBlock(CopyHead0->Successors[1]);
  const CFGBlock *CopyRet1 = Result.Output.getBlock(CopyHead1->Successors[0]);
  const CFGBlock *CopyTrap1 = Result.Output.getBlock(CopyHead1->Successors[1]);
  assert(CopyRet0 != nullptr && CopyTrap0 != nullptr);
  assert(CopyRet1 != nullptr && CopyTrap1 != nullptr);
  assert(CopyRet0->Terminator == TerminatorKind::Return);
  assert(CopyRet1->Terminator == TerminatorKind::Return);
  assert(CopyTrap0->Terminator == TerminatorKind::Unreachable);
  assert(CopyTrap1->Terminator == TerminatorKind::Unreachable);
  assert(hasSinglePayload(CopyRet0->Statements, 17));
  assert(hasSinglePayload(CopyRet1->Statements, 17));
  assert(hasSinglePayload(CopyTrap0->Statements, 19));
  assert(hasSinglePayload(CopyTrap1->Statements, 19));
}

void testReturnDuplicatorLowCopiesReturnTailForkRegion() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));
  Cfg.addBlock(branchBlock(2, {3, 5}));

  CFGBlock Tail = block(3, {4});
  Tail.Statements.push_back({20});
  Cfg.addBlock(std::move(Tail));

  CFGBlock Ret = block(4, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({21});
  Cfg.addBlock(std::move(Ret));

  CFGBlock Trap = block(5, {});
  Trap.Terminator = TerminatorKind::Unreachable;
  Trap.Statements.push_back({22});
  Cfg.addBlock(std::move(Trap));

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  assert(Result.Output.getBlock(2) == nullptr);
  assert(Result.Output.getBlock(3) == nullptr);
  assert(Result.Output.getBlock(4) == nullptr);
  assert(Result.Output.getBlock(5) == nullptr);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block1 = Result.Output.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != Block1->Successors.front());

  const CFGBlock *CopyHead0 = Result.Output.getBlock(Block0->Successors.front());
  const CFGBlock *CopyHead1 = Result.Output.getBlock(Block1->Successors.front());
  assert(CopyHead0 != nullptr && CopyHead1 != nullptr);
  assert(CopyHead0->Terminator == TerminatorKind::Branch);
  assert(CopyHead1->Terminator == TerminatorKind::Branch);
  assert(CopyHead0->Successors.size() == 2);
  assert(CopyHead1->Successors.size() == 2);

  const CFGBlock *CopyTail0 = Result.Output.getBlock(CopyHead0->Successors[0]);
  const CFGBlock *CopyTrap0 = Result.Output.getBlock(CopyHead0->Successors[1]);
  const CFGBlock *CopyTail1 = Result.Output.getBlock(CopyHead1->Successors[0]);
  const CFGBlock *CopyTrap1 = Result.Output.getBlock(CopyHead1->Successors[1]);
  assert(CopyTail0 != nullptr && CopyTrap0 != nullptr);
  assert(CopyTail1 != nullptr && CopyTrap1 != nullptr);
  assert(CopyTail0->Successors.size() == 1);
  assert(CopyTail1->Successors.size() == 1);
  assert(hasSinglePayload(CopyTail0->Statements, 20));
  assert(hasSinglePayload(CopyTail1->Statements, 20));
  assert(CopyTrap0->Terminator == TerminatorKind::Unreachable);
  assert(CopyTrap1->Terminator == TerminatorKind::Unreachable);
  assert(hasSinglePayload(CopyTrap0->Statements, 22));
  assert(hasSinglePayload(CopyTrap1->Statements, 22));

  const CFGBlock *CopyRet0 = Result.Output.getBlock(CopyTail0->Successors.front());
  const CFGBlock *CopyRet1 = Result.Output.getBlock(CopyTail1->Successors.front());
  assert(CopyRet0 != nullptr && CopyRet1 != nullptr);
  assert(CopyRet0->Terminator == TerminatorKind::Return);
  assert(CopyRet1->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(CopyRet0->Statements, 21));
  assert(hasSinglePayload(CopyRet1->Statements, 21));
}

void testReturnDuplicatorLowCopiesBranchReturnRegionWithPayloadRewrite() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Branch = branchBlock(2, {3, 5});
  Branch.Condition = {30};
  Cfg.addBlock(std::move(Branch));

  CFGBlock ThenTail = block(3, {4});
  ThenTail.Statements.push_back({31});
  Cfg.addBlock(std::move(ThenTail));

  CFGBlock ThenRet = block(4, {});
  ThenRet.Terminator = TerminatorKind::Return;
  ThenRet.Statements.push_back({32});
  Cfg.addBlock(std::move(ThenRet));

  CFGBlock ElseTail = block(5, {6});
  ElseTail.Statements.push_back({41});
  Cfg.addBlock(std::move(ElseTail));

  CFGBlock ElseRet = block(6, {});
  ElseRet.Terminator = TerminatorKind::Return;
  ElseRet.Statements.push_back({42});
  Cfg.addBlock(std::move(ElseRet));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (!Payload.isValid()) {
          return Payload;
        }
        assert(Context.OriginalPredecessor != InvalidBlockId);
        assert(Context.NewPredecessor != InvalidBlockId);
        return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
      },
      /*SupportsPredecessorRewrite=*/true);

  StructuringOptimizationOptions Options =
      ReturnDuplicatorLow::defaultOptions();
  Options.MaxOptIters = 1;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;

  CFGEdgeGotoRegionStructurer Structurer;
  ReturnDuplicatorLow Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  assert(Result.Changed);
  for (BlockId Original : {2, 3, 4, 5, 6}) {
    assert(Result.Output.getBlock(Original) == nullptr);
  }

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  const CFGBlock *Block1 = Result.Output.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != Block1->Successors.front());

  const CFGBlock *CopyBranch0 =
      Result.Output.getBlock(Block0->Successors.front());
  const CFGBlock *CopyBranch1 =
      Result.Output.getBlock(Block1->Successors.front());
  assert(CopyBranch0 != nullptr && CopyBranch1 != nullptr);
  assert(CopyBranch0->Terminator == TerminatorKind::Branch);
  assert(CopyBranch1->Terminator == TerminatorKind::Branch);
  assert(CopyBranch0->Successors.size() == 2);
  assert(CopyBranch1->Successors.size() == 2);
  assert(CopyBranch0->Condition.Id == 30 + Block0->Id * 1000);
  assert(CopyBranch1->Condition.Id == 30 + Block1->Id * 1000);

  const CFGBlock *CopyThenTail0 =
      Result.Output.getBlock(CopyBranch0->Successors[0]);
  const CFGBlock *CopyElseTail0 =
      Result.Output.getBlock(CopyBranch0->Successors[1]);
  const CFGBlock *CopyThenTail1 =
      Result.Output.getBlock(CopyBranch1->Successors[0]);
  const CFGBlock *CopyElseTail1 =
      Result.Output.getBlock(CopyBranch1->Successors[1]);
  assert(CopyThenTail0 != nullptr && CopyElseTail0 != nullptr);
  assert(CopyThenTail1 != nullptr && CopyElseTail1 != nullptr);
  assert(hasSinglePayload(CopyThenTail0->Statements,
                          31 + CopyBranch0->Id * 1000));
  assert(hasSinglePayload(CopyThenTail1->Statements,
                          31 + CopyBranch1->Id * 1000));
  assert(hasSinglePayload(CopyElseTail0->Statements,
                          41 + CopyBranch0->Id * 1000));
  assert(hasSinglePayload(CopyElseTail1->Statements,
                          41 + CopyBranch1->Id * 1000));

  const CFGBlock *CopyThenRet0 =
      Result.Output.getBlock(CopyThenTail0->Successors.front());
  const CFGBlock *CopyElseRet0 =
      Result.Output.getBlock(CopyElseTail0->Successors.front());
  const CFGBlock *CopyThenRet1 =
      Result.Output.getBlock(CopyThenTail1->Successors.front());
  const CFGBlock *CopyElseRet1 =
      Result.Output.getBlock(CopyElseTail1->Successors.front());
  assert(CopyThenRet0 != nullptr && CopyElseRet0 != nullptr);
  assert(CopyThenRet1 != nullptr && CopyElseRet1 != nullptr);
  assert(CopyThenRet0->Terminator == TerminatorKind::Return);
  assert(CopyThenRet1->Terminator == TerminatorKind::Return);
  assert(CopyElseRet0->Terminator == TerminatorKind::Return);
  assert(CopyElseRet1->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(CopyThenRet0->Statements,
                          32 + CopyThenTail0->Id * 1000));
  assert(hasSinglePayload(CopyThenRet1->Statements,
                          32 + CopyThenTail1->Id * 1000));
  assert(hasSinglePayload(CopyElseRet0->Statements,
                          42 + CopyElseTail0->Id * 1000));
  assert(hasSinglePayload(CopyElseRet1->Statements,
                          42 + CopyElseTail1->Id * 1000));
}

void testReturnDuplicatorLowCopiesBranchReturnRegionWithoutPredecessorRewriteSupport() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Branch = branchBlock(2, {3, 5});
  Branch.Condition = {30};
  Cfg.addBlock(std::move(Branch));

  CFGBlock ThenTail = block(3, {4});
  ThenTail.Statements.push_back({31});
  Cfg.addBlock(std::move(ThenTail));

  CFGBlock ThenRet = block(4, {});
  ThenRet.Terminator = TerminatorKind::Return;
  ThenRet.Statements.push_back({32});
  Cfg.addBlock(std::move(ThenRet));

  CFGBlock ElseTail = block(5, {6});
  ElseTail.Statements.push_back({41});
  Cfg.addBlock(std::move(ElseTail));

  CFGBlock ElseRet = block(6, {});
  ElseRet.Terminator = TerminatorKind::Return;
  ElseRet.Statements.push_back({42});
  Cfg.addBlock(std::move(ElseRet));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 2}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != 2);
  assert(Block1->Successors.front() == 2);
  assert(Cfg.getBlock(2) != nullptr);
  assert(Cfg.getBlock(4) != nullptr);
  assert(Cfg.getBlock(6) != nullptr);

  const CFGBlock *CopyBranch = Cfg.getBlock(Block0->Successors.front());
  assert(CopyBranch != nullptr);
  assert(CopyBranch->Terminator == TerminatorKind::Branch);
  assert(CopyBranch->BodyBlock == CopyBranch->Id);
  assert(CopyBranch->Condition.Id == 30);
  assert(CopyBranch->Successors.size() == 2);

  const CFGBlock *CopyThenTail = Cfg.getBlock(CopyBranch->Successors[0]);
  const CFGBlock *CopyElseTail = Cfg.getBlock(CopyBranch->Successors[1]);
  assert(CopyThenTail != nullptr && CopyElseTail != nullptr);
  assert(hasSinglePayload(CopyThenTail->Statements, 31));
  assert(hasSinglePayload(CopyElseTail->Statements, 41));

  const CFGBlock *CopyThenRet = Cfg.getBlock(CopyThenTail->Successors.front());
  const CFGBlock *CopyElseRet = Cfg.getBlock(CopyElseTail->Successors.front());
  assert(CopyThenRet != nullptr && CopyElseRet != nullptr);
  assert(CopyThenRet->Terminator == TerminatorKind::Return);
  assert(CopyElseRet->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(CopyThenRet->Statements, 32));
  assert(hasSinglePayload(CopyElseRet->Statements, 42));
}

void testReturnDuplicatorLowCopiesSwitchReturnRegionWithoutPredecessorRewriteSupport() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Switch = switchBlock(2, {3, 5});
  Switch.Condition = {50};
  Switch.Cases.clear();
  Switch.Cases.push_back({{51}, 5});
  Cfg.addBlock(std::move(Switch));

  CFGBlock DefaultTail = block(3, {4});
  DefaultTail.Statements.push_back({52});
  Cfg.addBlock(std::move(DefaultTail));

  CFGBlock DefaultRet = block(4, {});
  DefaultRet.Terminator = TerminatorKind::Return;
  DefaultRet.Statements.push_back({53});
  Cfg.addBlock(std::move(DefaultRet));

  CFGBlock CaseTail = block(5, {6});
  CaseTail.Statements.push_back({54});
  Cfg.addBlock(std::move(CaseTail));

  CFGBlock CaseRet = block(6, {});
  CaseRet.Terminator = TerminatorKind::Return;
  CaseRet.Statements.push_back({55});
  Cfg.addBlock(std::move(CaseRet));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::fromGotos({StructuredGoto{0, 2}});

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != 2);
  assert(Block1->Successors.front() == 2);

  const CFGBlock *CopySwitch = Cfg.getBlock(Block0->Successors.front());
  const CFGBlock *OriginalSwitch = Cfg.getBlock(2);
  assert(CopySwitch != nullptr && OriginalSwitch != nullptr);
  assert(CopySwitch->Terminator == TerminatorKind::Switch);
  assert(OriginalSwitch->Terminator == TerminatorKind::Switch);
  assert(CopySwitch->Condition.Id == 50);
  assert(OriginalSwitch->Condition.Id == 50);
  assert(CopySwitch->Cases.size() == 1);
  assert(OriginalSwitch->Cases.size() == 1);
  assert(CopySwitch->Cases.front().Value.Id == 51);
  assert(OriginalSwitch->Cases.front().Value.Id == 51);
  assert(CopySwitch->Successors.size() == 2);
  assert(OriginalSwitch->Successors.size() == 2);
  assert(CopySwitch->Cases.front().Target != 5);
  assert(OriginalSwitch->Cases.front().Target == 5);
  assert(CopySwitch->Successors[1] == CopySwitch->Cases.front().Target);
  assert(OriginalSwitch->Successors[1] == 5);

  const CFGBlock *CopyDefaultTail = Cfg.getBlock(CopySwitch->Successors[0]);
  const CFGBlock *CopyCaseTail = Cfg.getBlock(CopySwitch->Cases.front().Target);
  const CFGBlock *OriginalDefaultTail = Cfg.getBlock(3);
  const CFGBlock *OriginalCaseTail = Cfg.getBlock(5);
  assert(CopyDefaultTail != nullptr && CopyCaseTail != nullptr);
  assert(OriginalDefaultTail != nullptr && OriginalCaseTail != nullptr);
  assert(hasSinglePayload(CopyDefaultTail->Statements, 52));
  assert(hasSinglePayload(CopyCaseTail->Statements, 54));
  assert(hasSinglePayload(OriginalDefaultTail->Statements, 52));
  assert(hasSinglePayload(OriginalCaseTail->Statements, 54));
}

void testReturnDuplicatorLowUsesGotoInReturnTail() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {2}));

  CFGBlock Head = block(2, {3});
  Head.Statements.push_back({23});
  Cfg.addBlock(std::move(Head));

  CFGBlock Tail = block(3, {4});
  Tail.Statements.push_back({24});
  Cfg.addBlock(std::move(Tail));

  CFGBlock Ret = block(4, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({25});
  Cfg.addBlock(std::move(Ret));

  StructuredTree Tree;
  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  StructuredNode HeadNode;
  HeadNode.Kind = StructuredNodeKind::BasicBlock;
  HeadNode.Block = 2;
  Root.Children.push_back(Tree.addNode(std::move(HeadNode)));

  StructuredNode TailGoto;
  TailGoto.Kind = StructuredNodeKind::Goto;
  TailGoto.Target = 3;
  Root.Children.push_back(Tree.addNode(std::move(TailGoto)));

  Tree.setRoot(Tree.addNode(std::move(Root)));

  StructuringEvaluation Current;
  Current.Gotos = GotoManager::collect(Tree);

  TestReturnDuplicatorLow Pass(ReturnDuplicatorLow::defaultOptions());
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(2) == nullptr);
  assert(Cfg.getBlock(3) == nullptr);
  assert(Cfg.getBlock(4) == nullptr);

  const CFGBlock *Block0 = Cfg.getBlock(0);
  const CFGBlock *Block1 = Cfg.getBlock(1);
  assert(Block0 != nullptr && Block1 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block1->Successors.size() == 1);
  assert(Block0->Successors.front() != Block1->Successors.front());

  const CFGBlock *CopyHead0 = Cfg.getBlock(Block0->Successors.front());
  const CFGBlock *CopyHead1 = Cfg.getBlock(Block1->Successors.front());
  assert(CopyHead0 != nullptr && CopyHead1 != nullptr);
  assert(CopyHead0->Successors.size() == 1);
  assert(CopyHead1->Successors.size() == 1);
  assert(hasSinglePayload(CopyHead0->Statements, 23));
  assert(hasSinglePayload(CopyHead1->Statements, 23));

  const CFGBlock *CopyTail0 = Cfg.getBlock(CopyHead0->Successors.front());
  const CFGBlock *CopyTail1 = Cfg.getBlock(CopyHead1->Successors.front());
  assert(CopyTail0 != nullptr && CopyTail1 != nullptr);
  assert(CopyTail0->Successors.size() == 1);
  assert(CopyTail1->Successors.size() == 1);
  assert(hasSinglePayload(CopyTail0->Statements, 24));
  assert(hasSinglePayload(CopyTail1->Statements, 24));

  const CFGBlock *CopyRet0 = Cfg.getBlock(CopyTail0->Successors.front());
  const CFGBlock *CopyRet1 = Cfg.getBlock(CopyTail1->Successors.front());
  assert(CopyRet0 != nullptr && CopyRet1 != nullptr);
  assert(CopyRet0->Terminator == TerminatorKind::Return);
  assert(CopyRet1->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(CopyRet0->Statements, 25));
  assert(hasSinglePayload(CopyRet1->Statements, 25));
}

void testSwitchDefaultCaseDuplicatorCopiesReusedDefaultBlock() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {4});
  Default.Statements.push_back({11});
  Cfg.addBlock(std::move(Default));

  CFGBlock Case = block(2, {4});
  Case.Statements.push_back({12});
  Cfg.addBlock(std::move(Case));

  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(4, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch = Cfg.getBlock(0);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *CopyPred = Cfg.getBlock(3);
  assert(Switch != nullptr && DefaultBlock != nullptr && CopyPred != nullptr);
  assert(Switch->Successors.front() == 1);
  assert(DefaultBlock->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(DefaultBlock->Statements, 11));

  BlockId CopyId = CopyPred->Successors.front();
  assert(CopyId != 1);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Successors == std::vector<BlockId>{4});
  assert(Copy->SourceBlock == 1);
  assert(Copy->BodyMaterialized);
  assert(Copy->BodyBlock == Copy->Id);
  assert(hasSinglePayload(Copy->Statements, 11));
}

void testSwitchDefaultCaseDuplicatorCommitsRewriteAtomically() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(switchBlock(3, {1, 4}));

  CFGBlock Default = block(1, {5});
  Default.Statements.push_back({41});
  Cfg.addBlock(std::move(Default));

  Cfg.addBlock(block(2, {5}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch3 = Cfg.getBlock(3);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch3 != nullptr && DefaultBlock != nullptr);
  assert(Switch0->Successors.front() != 1);
  assert(Switch3->Successors.front() != 1);
  assert(Switch0->Successors.front() != Switch3->Successors.front());
  assert(DefaultBlock->Successors == std::vector<BlockId>{5});
  assert(hasSinglePayload(DefaultBlock->Statements, 41));
  assert(Cfg.getBlock(Switch0->Successors.front()) != nullptr);
  assert(Cfg.getBlock(Switch3->Successors.front()) != nullptr);
}

void testSwitchDefaultCaseDuplicatorInsertsSharedDefaultGotosByDefault() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(switchBlock(3, {1, 4}));

  CFGBlock Default = block(1, {5});
  Default.Statements.push_back({21});
  Cfg.addBlock(std::move(Default));

  CFGBlock Case0 = block(2, {5});
  Case0.Statements.push_back({22});
  Cfg.addBlock(std::move(Case0));

  CFGBlock Case1 = block(4, {5});
  Case1.Statements.push_back({23});
  Cfg.addBlock(std::move(Case1));

  Cfg.addBlock(block(5, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(3);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr && DefaultBlock != nullptr);
  assert(Switch0->Successors.front() != 1);
  assert(Switch1->Successors.front() != 1);
  assert(Switch0->Successors.front() != Switch1->Successors.front());
  assert(DefaultBlock->Successors == std::vector<BlockId>{5});
  assert(hasSinglePayload(DefaultBlock->Statements, 21));

  const CFGBlock *Goto0 = Cfg.getBlock(Switch0->Successors.front());
  const CFGBlock *Goto1 = Cfg.getBlock(Switch1->Successors.front());
  assert(Goto0 != nullptr && Goto1 != nullptr);
  assert(Goto0->BodyBlock == Goto0->Id);
  assert(Goto1->BodyBlock == Goto1->Id);
  assert(Goto0->Statements.empty());
  assert(Goto1->Statements.empty());
  assert(Goto0->Terminator == TerminatorKind::Fallthrough);
  assert(Goto1->Terminator == TerminatorKind::Fallthrough);
  assert(Goto0->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto1->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto0->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto1->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto0->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Goto1->CreatedBy == CFGBlockCreator::SAILRDeoptimization);
  assert(Goto0->BodyMaterialized);
  assert(Goto1->BodyMaterialized);
  assert(Goto0->SyntheticSource == 0);
  assert(Goto1->SyntheticSource == 3);
  assert(Goto0->SyntheticTarget == 1);
  assert(Goto1->SyntheticTarget == 1);
  assert(Goto0->Successors == std::vector<BlockId>{1});
  assert(Goto1->Successors == std::vector<BlockId>{1});
}

void testSwitchDefaultCaseDuplicatorCanUseSharedDefaultForwarders() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(switchBlock(3, {1, 4}));

  CFGBlock Default = block(1, {5});
  Default.Statements.push_back({21});
  Cfg.addBlock(std::move(Default));

  Cfg.addBlock(block(2, {5}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions(),
      SwitchDefaultCaseDuplicator::SharedDefaultRewriteMode::
          SyntheticForwarder);
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(3);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  const CFGBlock *Forwarder0 = Cfg.getBlock(Switch0->Successors.front());
  const CFGBlock *Forwarder1 = Cfg.getBlock(Switch1->Successors.front());
  assert(Forwarder0 != nullptr && Forwarder1 != nullptr);
  assert(Forwarder0->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(Forwarder1->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(Forwarder0->SyntheticSource == 0);
  assert(Forwarder1->SyntheticSource == 3);
  assert(Forwarder0->SyntheticTarget == 1);
  assert(Forwarder1->SyntheticTarget == 1);
  assert(Forwarder0->Successors == std::vector<BlockId>{1});
  assert(Forwarder1->Successors == std::vector<BlockId>{1});
}

void testSwitchDefaultCaseDuplicatorGotosTerminalSharedDefault() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(switchBlock(3, {1, 4}));

  CFGBlock Default = block(1, {});
  Default.Terminator = TerminatorKind::Return;
  Default.Statements.push_back({24});
  Cfg.addBlock(std::move(Default));

  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(4, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch3 = Cfg.getBlock(3);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch3 != nullptr && DefaultBlock != nullptr);
  assert(Switch0->Successors.front() != 1);
  assert(Switch3->Successors.front() != 1);
  assert(Switch0->Successors.front() != Switch3->Successors.front());
  assert(DefaultBlock->Successors.empty());
  assert(DefaultBlock->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(DefaultBlock->Statements, 24));

  const CFGBlock *Goto0 = Cfg.getBlock(Switch0->Successors.front());
  const CFGBlock *Goto1 = Cfg.getBlock(Switch3->Successors.front());
  assert(Goto0 != nullptr && Goto1 != nullptr);
  assert(Goto0->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto1->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto0->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto1->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto0->Successors == std::vector<BlockId>{1});
  assert(Goto1->Successors == std::vector<BlockId>{1});
  assert(Goto0->SyntheticSource == 0);
  assert(Goto1->SyntheticSource == 3);
  assert(Goto0->SyntheticTarget == 1);
  assert(Goto1->SyntheticTarget == 1);
}

void testSwitchDefaultCaseDuplicatorKeepsCaseTargetsOnDefaultReuse() {
  StructuredCFG Cfg;

  CFGBlock Switch0 = switchBlock(0, {1});
  Switch0.Cases.push_back({{}, 1});
  Cfg.addBlock(std::move(Switch0));

  CFGBlock Switch3 = switchBlock(3, {1, 4});
  Cfg.addBlock(std::move(Switch3));

  CFGBlock Default = block(1, {5});
  Default.Statements.push_back({24});
  Cfg.addBlock(std::move(Default));

  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);

  const CFGBlock *Switch0Block = Cfg.getBlock(0);
  const CFGBlock *Switch3Block = Cfg.getBlock(3);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  assert(Switch0Block != nullptr && Switch3Block != nullptr &&
         DefaultBlock != nullptr);
  assert(Switch0Block->Successors.front() != 1);
  assert(Switch0Block->Cases.size() == 1);
  assert(Switch0Block->Cases.front().Target == 1);
  assert(Switch3Block->Successors.front() != 1);
  assert(Switch3Block->Successors.front() != Switch0Block->Successors.front());
  assert(DefaultBlock->Successors == std::vector<BlockId>{5});
  assert(hasSinglePayload(DefaultBlock->Statements, 24));
}

void testLoweredSwitchSimplifierKeepsSwitchReuseShape() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {9, 1, 2, 3}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {4}));
  Cfg.addBlock(block(3, {4}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(9, {}));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  const CFGBlock *Switch = Cfg.getBlock(0);
  assert(Switch != nullptr);
  assert(Switch->Cases.size() == 3);
  assert(Switch->Successors.front() == 9);
  assert(!Changed || Switch->Cases[0].Target != 1 ||
         Switch->Cases[1].Target != 2 || Switch->Cases[2].Target != 3);

  const CFGBlock *Case1 = Cfg.getBlock(1);
  const CFGBlock *Case2 = Cfg.getBlock(2);
  const CFGBlock *Case3 = Cfg.getBlock(3);
  assert(Case1 != nullptr && Case2 != nullptr && Case3 != nullptr);
  assert(Case1->Successors == std::vector<BlockId>{4});
  assert(Case2->Successors == std::vector<BlockId>{4});
  assert(Case3->Successors == std::vector<BlockId>{4});
}

void testSwitchDefaultCaseDuplicatorSkipsSwitchInternalDefaultPred() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {5});
  Default.Statements.push_back({27});
  Cfg.addBlock(std::move(Default));

  CFGBlock CaseHead = block(2, {3});
  CaseHead.Statements.push_back({28});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(3, {1});
  CaseTail.Statements.push_back({29});
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(5, {}));

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *Switch = Cfg.getBlock(0);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *CaseTailBlock = Cfg.getBlock(3);
  assert(Switch != nullptr && DefaultBlock != nullptr && CaseTailBlock != nullptr);
  assert(Switch->Successors.front() == 1);
  assert(CaseTailBlock->Successors == std::vector<BlockId>{1});
  assert(DefaultBlock->Successors == std::vector<BlockId>{5});
  assert(hasSinglePayload(DefaultBlock->Statements, 27));
}

void testSwitchDefaultCaseDuplicatorCopiesDefaultTailRegion() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {4});
  Default.Statements.push_back({31});
  Cfg.addBlock(std::move(Default));

  CFGBlock Tail = block(4, {5});
  Tail.Statements.push_back({32});
  Cfg.addBlock(std::move(Tail));

  CFGBlock Ret = block(5, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({33});
  Cfg.addBlock(std::move(Ret));

  CFGBlock Case = block(2, {5});
  Case.Statements.push_back({34});
  Cfg.addBlock(std::move(Case));

  Cfg.addBlock(block(3, {1}));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (!Payload.isValid()) {
          return Payload;
        }
        assert(Context.OriginalPredecessor != InvalidBlockId);
        assert(Context.NewPredecessor != InvalidBlockId);
        return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
      },
      /*SupportsPredecessorRewrite=*/true);

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(1) != nullptr);
  assert(Cfg.getBlock(4) != nullptr);
  assert(Cfg.getBlock(5) != nullptr);

  const CFGBlock *Switch = Cfg.getBlock(0);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *CopyPred = Cfg.getBlock(3);
  assert(Switch != nullptr && DefaultBlock != nullptr && CopyPred != nullptr);
  assert(Switch->Successors.front() == 1);
  assert(DefaultBlock->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(DefaultBlock->Statements, 31));

  BlockId CopyId = CopyPred->Successors.front();
  assert(CopyId != 1);
  const CFGBlock *Copy = Cfg.getBlock(CopyId);
  assert(Copy != nullptr);
  assert(Copy->Successors.size() == 1);
  assert(hasSinglePayload(Copy->Statements, 31 + CopyPred->Id * 1000));

  const CFGBlock *CopyTail = Cfg.getBlock(Copy->Successors.front());
  assert(CopyTail != nullptr);
  assert(CopyTail->Successors == std::vector<BlockId>{5});
  assert(CopyTail->SourceBlock == 4);
  assert(CopyTail->BodyMaterialized);
  assert(CopyTail->BodyBlock == CopyTail->Id);
  assert(hasSinglePayload(CopyTail->Statements, 32 + Copy->Id * 1000));

  assert(hasSinglePayload(Cfg.getBlock(5)->Statements, 33));
}

void testSwitchDefaultCaseDuplicatorKeepsPredSensitiveCopiesSeparate() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {4});
  Default.Statements.push_back({31});
  Cfg.addBlock(std::move(Default));

  CFGBlock Tail = block(4, {});
  Tail.Statements.push_back({32});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(6, {3, 1}));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        assert(Context.OriginalPredecessor != InvalidBlockId);
        assert(Context.NewPredecessor != InvalidBlockId);
        assert(Context.OriginalPredecessors.size() == 1);
        assert(Context.NewPredecessors.size() == 1);
        return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
      },
      /*SupportsPredecessorRewrite=*/true);

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *Pred3 = Cfg.getBlock(3);
  const CFGBlock *Pred6 = Cfg.getBlock(6);
  assert(DefaultBlock != nullptr && Pred3 != nullptr && Pred6 != nullptr);
  assert(DefaultBlock->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(DefaultBlock->Statements, 31));

  BlockId Copy3Id = Pred3->Successors.front();
  BlockId Copy6Id = Pred6->Successors[1];
  assert(Copy3Id != 1);
  assert(Copy6Id != 1);
  assert(Copy3Id != Copy6Id);

  const CFGBlock *Copy3 = Cfg.getBlock(Copy3Id);
  const CFGBlock *Copy6 = Cfg.getBlock(Copy6Id);
  assert(Copy3 != nullptr && Copy6 != nullptr);
  assert(Copy3->SourceBlock == 1);
  assert(Copy6->SourceBlock == 1);
  assert(hasSinglePayload(Copy3->Statements, 31 + 3 * 1000));
  assert(!Copy6->Statements.empty());

  const CFGBlock *Copy3Tail = Cfg.getBlock(Copy3->Successors.front());
  const CFGBlock *Copy6Tail = Cfg.getBlock(Copy6->Successors.front());
  assert(Copy3Tail != nullptr && Copy6Tail != nullptr);
  assert(Copy3Tail->SourceBlock == 4);
  assert(Copy6Tail->SourceBlock == 4);
  assert(hasSinglePayload(Copy3Tail->Statements, 32 + Copy3->Id * 1000));
  assert(hasSinglePayload(Copy6Tail->Statements, 32 + Copy6->Id * 1000));
}

void testSwitchDefaultCaseDuplicatorReportsGroupedPredecessors() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {4});
  Default.Statements.push_back({31});
  Cfg.addBlock(std::move(Default));

  CFGBlock Tail = block(4, {});
  Tail.Statements.push_back({32});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(6, {3, 1}));

  std::vector<BlockId> SawOriginalGroup;
  std::vector<BlockId> SawNewGroup;
  std::size_t CaseRewriteCount = 0;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
          PayloadRef Payload, std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::Statement) {
          if (Context.OriginalPredecessors == std::vector<BlockId>({3, 6})) {
            SawOriginalGroup = Context.OriginalPredecessors;
            SawNewGroup = Context.NewPredecessors;
            return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
          }
          return Payload;
        }
        if (Kind == PayloadMaterializeKind::SwitchCaseValue) {
          assert(Index == 0);
          assert(Context.OriginalTarget == 4);
          assert(Context.NewTarget == 4);
          ++CaseRewriteCount;
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(SawOriginalGroup == std::vector<BlockId>({3, 6}));
  assert(SawNewGroup == std::vector<BlockId>({3, 6}));
  assert(CaseRewriteCount == 0);

  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *Pred3 = Cfg.getBlock(3);
  const CFGBlock *Pred6 = Cfg.getBlock(6);
  assert(DefaultBlock != nullptr && Pred3 != nullptr && Pred6 != nullptr);
  assert(DefaultBlock->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(DefaultBlock->Statements, 31));

  BlockId Copy3Id = Pred3->Successors.front();
  BlockId Copy6Id = Pred6->Successors[1];
  const CFGBlock *Copy3 = Cfg.getBlock(Copy3Id);
  const CFGBlock *Copy6 = Cfg.getBlock(Copy6Id);
  assert(Copy3 != nullptr && Copy6 != nullptr);
  assert(hasSinglePayload(Copy3->Statements, 31 + 3 * 1000));
  assert(!Copy6->Statements.empty());
  assert(Copy6->SourceBlock == 1);
}

void testSwitchDefaultCaseDuplicatorRollsBackGroupedPredecessorFailure() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));

  CFGBlock Default = block(1, {4});
  Default.Statements.push_back({31});
  Cfg.addBlock(std::move(Default));

  CFGBlock Tail = block(4, {});
  Tail.Statements.push_back({32});
  Cfg.addBlock(std::move(Tail));

  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(3, {1}));
  Cfg.addBlock(block(6, {3, 1}));

  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
          PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        if (Context.OriginalPredecessors == std::vector<BlockId>({3, 6})) {
          return std::nullopt;
        }
        return Payload;
      },
      /*SupportsPredecessorRewrite=*/true,
      /*SupportsGroupedPredecessorRewrite=*/true);

  TestSwitchDefaultCaseDuplicator Pass(
      SwitchDefaultCaseDuplicator::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *Switch = Cfg.getBlock(0);
  const CFGBlock *DefaultBlock = Cfg.getBlock(1);
  const CFGBlock *Pred3 = Cfg.getBlock(3);
  const CFGBlock *Pred6 = Cfg.getBlock(6);
  assert(Switch != nullptr && DefaultBlock != nullptr && Pred3 != nullptr &&
         Pred6 != nullptr);
  assert(Switch->Successors.front() == 1);
  assert(DefaultBlock->Successors == std::vector<BlockId>{4});
  assert(hasSinglePayload(DefaultBlock->Statements, 31));
  assert(Pred3->Successors == std::vector<BlockId>{1});
  assert((Pred6->Successors == std::vector<BlockId>{3, 1}));
}

void testSwitchReusedEntryRewriterCreatesGotoWithoutCopyingEntryTail() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {3, 1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(switchBlock(2, {5, 1}));
  Cfg.addBlock(block(3, {6}));
  Cfg.addBlock(block(4, {6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  CFGBlock *Entry = Cfg.getBlock(1);
  CFGBlock *Tail = Cfg.getBlock(4);
  assert(Entry != nullptr && Tail != nullptr);
  Entry->Statements.push_back({22});
  Tail->Statements.push_back({24});

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch2 = Cfg.getBlock(2);
  const CFGBlock *EntryBlock = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch2 != nullptr && EntryBlock != nullptr);
  assert(Switch2->Cases.front().Target != 1);
  assert(hasSinglePayload(EntryBlock->Statements, 22));

  const CFGBlock *Goto = Cfg.getBlock(Switch2->Cases.front().Target);
  assert(Goto != nullptr);
  assert(Goto->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto->Successors.empty());
  assert(Goto->SyntheticSource == 2);
  assert(Goto->SyntheticTarget == 1);
  assert(!Cfg.hasEdge(Goto->Id, 1));
  const CFGBlock *TailAfterRewrite = Cfg.getBlock(4);
  assert(TailAfterRewrite != nullptr);
  assert(hasSinglePayload(TailAfterRewrite->Statements, 24));
}

void testSwitchReusedEntryRewriterPreservesReusedEntryTail() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {3, 1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(switchBlock(2, {5, 1}));
  Cfg.addBlock(block(3, {6}));
  Cfg.addBlock(block(4, {6}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  CFGBlock *Entry = Cfg.getBlock(1);
  CFGBlock *Tail = Cfg.getBlock(4);
  assert(Entry != nullptr && Tail != nullptr);
  Entry->Statements.push_back({22});
  Tail->Statements.push_back({24});

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *EntryBlock = Cfg.getBlock(1);
  const CFGBlock *TailBlock = Cfg.getBlock(4);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch2 = Cfg.getBlock(2);
  assert(EntryBlock != nullptr && TailBlock != nullptr && Switch0 != nullptr &&
         Switch2 != nullptr);
  assert(hasSinglePayload(EntryBlock->Statements, 22));
  assert(hasSinglePayload(TailBlock->Statements, 24));
  assert(Switch0->Cases.front().Target == 1);
  assert(Switch2->Cases.front().Target != 1);

  const CFGBlock *Goto = Cfg.getBlock(Switch2->Cases.front().Target);
  assert(Goto != nullptr);
  assert(Goto->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto->Successors.empty());
  assert(Goto->SyntheticSource == 2);
  assert(Goto->SyntheticTarget == 1);
}

void testSwitchReusedEntryRewriterKeepsLowestSwitchIdEntry() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(10, {12, 1}));
  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(switchBlock(0, {11, 1}));
  Cfg.addBlock(block(1, {20}));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(20, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch10 = Cfg.getBlock(10);
  assert(Switch0 != nullptr && Switch10 != nullptr);
  assert(Switch0->Cases.front().Target == 1);
  assert(Switch10->Cases.front().Target != 1);

  const CFGBlock *Goto = Cfg.getBlock(Switch10->Cases.front().Target);
  assert(Goto != nullptr);
  assert(Goto->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto->SyntheticSource == 10);
  assert(Goto->SyntheticTarget == 1);
  assert(Goto->Successors.empty());
}

void testSwitchReusedEntryRewriterKeepsDefaultSuccessorUntouched() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {2, 1}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(switchBlock(3, {5, 1}));
  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(5, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch3 = Cfg.getBlock(3);
  assert(Switch0 != nullptr && Switch3 != nullptr);
  assert(Switch0->Successors.front() == 2);
  assert(Switch3->Successors.front() == 5);
  assert(Switch3->Successors.size() == 2);
  assert(Switch0->Cases.front().Target == 1);
  assert(Switch3->Cases.front().Target != 1);
  assert(Switch3->Successors[1] == Switch3->Cases.front().Target);

  const CFGBlock *Goto = Cfg.getBlock(Switch3->Cases.front().Target);
  assert(Goto != nullptr);
  assert(Goto->Origin == CFGBlockOrigin::Synthetic);
  assert(Goto->CopyKind == CFGBlockCopyKind::SyntheticGoto);
  assert(Goto->SyntheticSource == 3);
  assert(Goto->SyntheticTarget == 1);
}

void testSwitchReusedEntryRewriterRequiresRealCaseEdge() {
  StructuredCFG Cfg;
  CFGBlock Switch0 = switchBlock(0, {2, 1});
  Cfg.addBlock(std::move(Switch0));

  CFGBlock Switch3 = switchBlock(3, {5, 6});
  Switch3.Cases.front().Target = 1;
  Cfg.addBlock(std::move(Switch3));

  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(block(2, {}));
  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(5, {}));
  Cfg.addBlock(block(6, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  const CFGBlock *Switch3Block = Cfg.getBlock(3);
  assert(Switch3Block != nullptr);
  assert(Switch3Block->Cases.front().Target == 1);
  assert(Switch3Block->Successors == std::vector<BlockId>({5, 6}));
}

void testSwitchReusedEntryRewriterSkipsDefaultOnlyTargets() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 3}));
  Cfg.addBlock(block(1, {4}));
  Cfg.addBlock(switchBlock(2, {1, 5}));
  Cfg.addBlock(block(3, {}));
  Cfg.addBlock(block(4, {}));
  Cfg.addBlock(block(5, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions());
  StructuringEvaluation Current;
  Pass.runOnGraph(Cfg, Current);

  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch2 = Cfg.getBlock(2);
  assert(Switch0 != nullptr && Switch2 != nullptr);
  assert(Switch0->Successors.front() == 1);
  assert(Switch2->Successors.front() == 1);
  assert(Switch0->Cases.front().Target == 3);
  assert(Switch2->Cases.front().Target == 5);
}

void testSwitchReusedEntryRewriterSkipsEntryOverReuseLimit() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {10, 1}));
  Cfg.addBlock(block(1, {20}));
  Cfg.addBlock(switchBlock(2, {11, 1}));
  Cfg.addBlock(switchBlock(3, {12, 1}));
  Cfg.addBlock(block(10, {}));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(20, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions(),
      /*MaxEntryReuseCount=*/2, /*MaxReusedEntries=*/20);
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(0)->Cases.front().Target == 1);
  assert(Cfg.getBlock(2)->Cases.front().Target == 1);
  assert(Cfg.getBlock(3)->Cases.front().Target == 1);
}

void testSwitchReusedEntryRewriterSkipsTooManyReusedEntries() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {10, 1}));
  Cfg.addBlock(block(1, {20}));
  Cfg.addBlock(switchBlock(2, {11, 1}));
  Cfg.addBlock(switchBlock(3, {12, 4}));
  Cfg.addBlock(block(4, {21}));
  Cfg.addBlock(switchBlock(5, {13, 4}));
  Cfg.addBlock(block(10, {}));
  Cfg.addBlock(block(11, {}));
  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));
  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(21, {}));

  TestSwitchReusedEntryRewriter Pass(
      SwitchReusedEntryRewriter::defaultOptions(),
      /*MaxEntryReuseCount=*/10, /*MaxReusedEntries=*/1);
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(0)->Cases.front().Target == 1);
  assert(Cfg.getBlock(2)->Cases.front().Target == 1);
  assert(Cfg.getBlock(3)->Cases.front().Target == 4);
  assert(Cfg.getBlock(5)->Cases.front().Target == 4);
}

void testLoweredSwitchSimplifierCopiesLinearSharedCaseRegion() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({31});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {12});
  CaseTail.Statements.push_back({32});
  Cfg.addBlock(std::move(CaseTail));

  CFGBlock CaseRet = block(12, {});
  CaseRet.Terminator = TerminatorKind::Return;
  CaseRet.Statements.push_back({33});
  Cfg.addBlock(std::move(CaseRet));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context,
          PayloadMaterializeKind Kind, PayloadRef Payload,
          std::size_t Index) -> std::optional<PayloadRef> {
        if (Kind == PayloadMaterializeKind::SwitchCaseValue) {
          assert(Context.OriginalTarget == 10);
          assert(Context.NewTarget != 10);
          return PayloadRef{Payload.Id + 200};
        }
        return PayloadRef{Payload.Id};
      });

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(10) == nullptr);
  assert(Cfg.getBlock(11) == nullptr);
  assert(Cfg.getBlock(12) == nullptr);

  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  assert(Switch0->Successors.front() == 20);
  assert(Switch1->Successors.front() == 30);
  assert(Switch0->Cases.size() == 1);
  assert(Switch1->Cases.size() == 1);
  const CFGBlock *Copy0Head = Cfg.getBlock(Switch0->Cases.front().Target);
  const CFGBlock *Copy1Head = Cfg.getBlock(Switch1->Cases.front().Target);
  assert(Copy0Head != nullptr && Copy1Head != nullptr);
  assert(Copy0Head->BodyBlock == Copy0Head->Id);
  assert(Copy1Head->BodyBlock == Copy1Head->Id);
  assert(hasSinglePayload(Copy0Head->Statements, 31));
  assert(hasSinglePayload(Copy1Head->Statements, 31));
  assert(Copy0Head->Successors.size() == 1);
  assert(Copy1Head->Successors.size() == 1);

  const CFGBlock *Copy0Tail = Cfg.getBlock(Copy0Head->Successors.front());
  const CFGBlock *Copy1Tail = Cfg.getBlock(Copy1Head->Successors.front());
  assert(Copy0Tail != nullptr && Copy1Tail != nullptr);
  assert(Copy0Tail->BodyBlock == Copy0Tail->Id);
  assert(Copy1Tail->BodyBlock == Copy1Tail->Id);
  assert(hasSinglePayload(Copy0Tail->Statements, 32));
  assert(hasSinglePayload(Copy1Tail->Statements, 32));
  assert(Copy0Tail->Successors.size() == 1);
  assert(Copy1Tail->Successors.size() == 1);

  const CFGBlock *Copy0Ret = Cfg.getBlock(Copy0Tail->Successors.front());
  const CFGBlock *Copy1Ret = Cfg.getBlock(Copy1Tail->Successors.front());
  assert(Copy0Ret != nullptr && Copy1Ret != nullptr);
  assert(Copy0Ret->BodyBlock == Copy0Ret->Id);
  assert(Copy1Ret->BodyBlock == Copy1Ret->Id);
  assert(Copy0Ret->Terminator == TerminatorKind::Return);
  assert(Copy1Ret->Terminator == TerminatorKind::Return);
  assert(hasSinglePayload(Copy0Ret->Statements, 33));
  assert(hasSinglePayload(Copy1Ret->Statements, 33));
}

void testLoweredSwitchSimplifierKeepsPredSensitiveCopiesSeparate() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {0, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({31});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {});
  CaseTail.Statements.push_back({32});
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(20, {}));

  Cfg.setPayloadMaterializeHook(
      [](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
         PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        assert(Context.OriginalPredecessor != InvalidBlockId);
        assert(Context.NewPredecessor != InvalidBlockId);
        assert(Context.OriginalPredecessors.size() == 1);
        assert(Context.NewPredecessors.size() == 1);
        return PayloadRef{Payload.Id + Context.NewPredecessor * 1000};
      },
      /*SupportsPredecessorRewrite=*/true);

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(10) == nullptr);
  assert(Cfg.getBlock(11) == nullptr);

  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  assert(Switch0->Cases.size() == 1);
  assert(Switch1->Cases.size() == 1);
  assert(Switch0->Cases.front().Target != Switch1->Cases.front().Target);

  const CFGBlock *Copy0 = Cfg.getBlock(Switch0->Cases.front().Target);
  const CFGBlock *Copy1 = Cfg.getBlock(Switch1->Cases.front().Target);
  assert(Copy0 != nullptr && Copy1 != nullptr);
  assert(Copy0->SourceBlock == 10);
  assert(Copy1->SourceBlock == 10);
  assert(hasSinglePayload(Copy0->Statements, 31 + 0 * 1000));
  assert(hasSinglePayload(Copy1->Statements, 31 + 1 * 1000));

  const CFGBlock *Copy0Tail = Cfg.getBlock(Copy0->Successors.front());
  const CFGBlock *Copy1Tail = Cfg.getBlock(Copy1->Successors.front());
  assert(Copy0Tail != nullptr && Copy1Tail != nullptr);
  assert(Copy0Tail->SourceBlock == 11);
  assert(Copy1Tail->SourceBlock == 11);
  assert(hasSinglePayload(Copy0Tail->Statements, 32 + Copy0->Id * 1000));
  assert(hasSinglePayload(Copy1Tail->Statements, 32 + Copy1->Id * 1000));
}

void testLoweredSwitchSimplifierCommitsCopyAtomically() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({51});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {});
  CaseTail.Statements.push_back({52});
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  assert(Switch0->Successors.front() == 20);
  assert(Switch1->Successors.front() == 30);
  assert(Cfg.getBlock(10) == nullptr);
  assert(Cfg.getBlock(11) == nullptr);
  const CFGBlock *Copy0 = Cfg.getBlock(Switch0->Cases.front().Target);
  const CFGBlock *Copy1 = Cfg.getBlock(Switch1->Cases.front().Target);
  assert(Copy0 != nullptr && Copy1 != nullptr);
  assert(Copy0->BodyBlock == Copy0->Id);
  assert(Copy1->BodyBlock == Copy1->Id);
  assert(hasSinglePayload(Copy0->Statements, 51));
  assert(hasSinglePayload(Copy1->Statements, 51));
}

void testLoweredSwitchSimplifierSkipsPartialCopyOnFailure() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({71});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {12});
  CaseTail.Statements.push_back({72});
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  bool SawGoodGroup = false;
  Cfg.setPayloadMaterializeHook(
      [&](const PayloadMaterializeContext &Context, PayloadMaterializeKind Kind,
          PayloadRef Payload, std::size_t) -> std::optional<PayloadRef> {
        if (Kind != PayloadMaterializeKind::Statement) {
          return Payload;
        }
        if (Context.OriginalPredecessor == 0) {
          SawGoodGroup = true;
          return PayloadRef{Payload.Id + 100};
        }
        return std::nullopt;
      },
      /*SupportsPredecessorRewrite=*/true);

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(SawGoodGroup);
  assert(Cfg.getBlock(10) != nullptr);
  assert(Cfg.getBlock(11) != nullptr);
  assert(Cfg.getBlock(0)->Cases.front().Target == 10);
  assert(Cfg.getBlock(1)->Cases.front().Target == 10);
}

void testLoweredSwitchSimplifierSkipsUnsafeOriginalDeletion() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({34});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = switchBlock(11, {12, 13});
  CaseTail.Cases.front().Value = {74};
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(12, {}));
  Cfg.addBlock(block(13, {}));
  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  CFGBlock BadCopy = switchBlock(40, {12});
  BadCopy.Origin = CFGBlockOrigin::Copied;
  BadCopy.SourceBlock = 11;
  BadCopy.CopyKind = CFGBlockCopyKind::RegionCopy;
  BadCopy.CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  BadCopy.BodyBlock = 11;
  BadCopy.BodyMaterialized = false;
  Cfg.addBlock(std::move(BadCopy));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(10) != nullptr);
  assert(Cfg.getBlock(11) != nullptr);
  assert(Cfg.getBlock(40) != nullptr);
  assert(Cfg.getBlock(0)->Cases.front().Target == 10);
  assert(Cfg.getBlock(1)->Cases.front().Target == 10);
  assert(!Cfg.getBlock(40)->BodyMaterialized);
  assert(Cfg.getBlock(40)->BodyBlock == 11);
}

void testLoweredSwitchSimplifierSkipsDefaultOnlyTargets() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {10, 20}));
  Cfg.addBlock(switchBlock(1, {10, 30}));

  CFGBlock DefaultHead = block(10, {11});
  DefaultHead.Statements.push_back({35});
  Cfg.addBlock(std::move(DefaultHead));

  CFGBlock DefaultTail = block(11, {});
  DefaultTail.Statements.push_back({36});
  Cfg.addBlock(std::move(DefaultTail));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(10) != nullptr);
  assert(Cfg.getBlock(11) != nullptr);
  assert(Cfg.getBlock(0)->Successors.front() == 10);
  assert(Cfg.getBlock(1)->Successors.front() == 10);
  assert(Cfg.getBlock(0)->Cases.front().Target == 20);
  assert(Cfg.getBlock(1)->Cases.front().Target == 30);
}

void testLoweredSwitchSimplifierCopiesTerminalForkCaseRegion() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({41});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock Fork = branchBlock(11, {12, 13});
  Fork.Statements.push_back({42});
  Cfg.addBlock(std::move(Fork));

  CFGBlock Ret = block(12, {});
  Ret.Terminator = TerminatorKind::Return;
  Ret.Statements.push_back({43});
  Cfg.addBlock(std::move(Ret));

  CFGBlock Trap = block(13, {});
  Trap.Terminator = TerminatorKind::Unreachable;
  Trap.Statements.push_back({44});
  Cfg.addBlock(std::move(Trap));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  assert(Cfg.getBlock(10) == nullptr);
  assert(Cfg.getBlock(11) == nullptr);
  assert(Cfg.getBlock(12) == nullptr);
  assert(Cfg.getBlock(13) == nullptr);

  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);

  assert(Switch0->Successors.front() == 20);
  assert(Switch1->Successors.front() == 30);
  assert(Switch0->Cases.size() == 1);
  assert(Switch1->Cases.size() == 1);

  const CFGBlock *Copy0Head = Cfg.getBlock(Switch0->Cases.front().Target);
  const CFGBlock *Copy1Head = Cfg.getBlock(Switch1->Cases.front().Target);
  assert(Copy0Head != nullptr && Copy1Head != nullptr);
  assert(Copy0Head->Successors.size() == 1);
  assert(Copy1Head->Successors.size() == 1);
  assert(hasSinglePayload(Copy0Head->Statements, 41));
  assert(hasSinglePayload(Copy1Head->Statements, 41));

  const CFGBlock *Copy0Fork = Cfg.getBlock(Copy0Head->Successors.front());
  const CFGBlock *Copy1Fork = Cfg.getBlock(Copy1Head->Successors.front());
  assert(Copy0Fork != nullptr && Copy1Fork != nullptr);
  assert(Copy0Fork->Terminator == TerminatorKind::Branch);
  assert(Copy1Fork->Terminator == TerminatorKind::Branch);
  assert(Copy0Fork->Successors.size() == 2);
  assert(Copy1Fork->Successors.size() == 2);
  assert(hasSinglePayload(Copy0Fork->Statements, 42));
  assert(hasSinglePayload(Copy1Fork->Statements, 42));

  const CFGBlock *Copy0Ret = Cfg.getBlock(Copy0Fork->Successors[0]);
  const CFGBlock *Copy0Trap = Cfg.getBlock(Copy0Fork->Successors[1]);
  const CFGBlock *Copy1Ret = Cfg.getBlock(Copy1Fork->Successors[0]);
  const CFGBlock *Copy1Trap = Cfg.getBlock(Copy1Fork->Successors[1]);
  assert(Copy0Ret != nullptr && Copy0Trap != nullptr);
  assert(Copy1Ret != nullptr && Copy1Trap != nullptr);
  assert(Copy0Ret->Terminator == TerminatorKind::Return);
  assert(Copy1Ret->Terminator == TerminatorKind::Return);
  assert(Copy0Trap->Terminator == TerminatorKind::Unreachable);
  assert(Copy1Trap->Terminator == TerminatorKind::Unreachable);
  assert(hasSinglePayload(Copy0Ret->Statements, 43));
  assert(hasSinglePayload(Copy1Ret->Statements, 43));
  assert(hasSinglePayload(Copy0Trap->Statements, 44));
  assert(hasSinglePayload(Copy1Trap->Statements, 44));
}

void testLoweredSwitchSimplifierKeepsDefaultWhenCaseTargetIsReused() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {10, 10}));
  Cfg.addBlock(switchBlock(1, {30, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({61});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {});
  CaseTail.Statements.push_back({62});
  Cfg.addBlock(std::move(CaseTail));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  assert(Switch0->Successors.front() == 10);
  assert(Switch1->Successors.front() == 30);
  assert(Switch0->Cases.front().Target != 10);
  assert(Switch1->Cases.front().Target != 10);
  assert(Switch0->Cases.front().Target != Switch1->Cases.front().Target);

  const CFGBlock *OriginalHead = Cfg.getBlock(10);
  const CFGBlock *OriginalTail = Cfg.getBlock(11);
  assert(OriginalHead != nullptr && OriginalTail != nullptr);
  assert(hasSinglePayload(OriginalHead->Statements, 61));
  assert(hasSinglePayload(OriginalTail->Statements, 62));

  const CFGBlock *Copy0 = Cfg.getBlock(Switch0->Cases.front().Target);
  const CFGBlock *Copy1 = Cfg.getBlock(Switch1->Cases.front().Target);
  assert(Copy0 != nullptr && Copy1 != nullptr);
  assert(Copy0->SourceBlock == 10);
  assert(Copy1->SourceBlock == 10);
  assert(Copy0->BodyBlock == Copy0->Id);
  assert(Copy1->BodyBlock == Copy1->Id);
  assert(hasSinglePayload(Copy0->Statements, 61));
  assert(hasSinglePayload(Copy1->Statements, 61));
}

void testLoweredSwitchSimplifierSplitsSingleCaseDefaultReuse() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {10, 10}));

  CFGBlock CaseHead = block(10, {11});
  CaseHead.Statements.push_back({81});
  Cfg.addBlock(std::move(CaseHead));

  CFGBlock CaseTail = block(11, {});
  CaseTail.Statements.push_back({82});
  Cfg.addBlock(std::move(CaseTail));

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(Changed);
  const CFGBlock *Switch = Cfg.getBlock(0);
  assert(Switch != nullptr);
  assert(Switch->Successors.front() == 10);
  assert(Switch->Cases.size() == 1);
  assert(Switch->Cases.front().Target != 10);

  const CFGBlock *OriginalHead = Cfg.getBlock(10);
  const CFGBlock *OriginalTail = Cfg.getBlock(11);
  assert(OriginalHead != nullptr && OriginalTail != nullptr);
  assert(hasSinglePayload(OriginalHead->Statements, 81));
  assert(hasSinglePayload(OriginalTail->Statements, 82));

  const CFGBlock *CopyHead = Cfg.getBlock(Switch->Cases.front().Target);
  assert(CopyHead != nullptr);
  assert(CopyHead->SourceBlock == 10);
  assert(CopyHead->BodyBlock == CopyHead->Id);
  assert(hasSinglePayload(CopyHead->Statements, 81));
  assert(CopyHead->Successors.size() == 1);

  const CFGBlock *CopyTail = Cfg.getBlock(CopyHead->Successors.front());
  assert(CopyTail != nullptr);
  assert(CopyTail->SourceBlock == 11);
  assert(CopyTail->BodyBlock == CopyTail->Id);
  assert(hasSinglePayload(CopyTail->Statements, 82));
}

void testLoweredSwitchSimplifierKeepsNestedSwitchSharedShape() {
  StructuredCFG Cfg;

  Cfg.addBlock(switchBlock(0, {20, 10}));
  Cfg.addBlock(switchBlock(1, {10, 30}));

  CFGBlock OuterDefault = block(10, {11});
  OuterDefault.Statements.push_back({41});
  Cfg.addBlock(std::move(OuterDefault));

  CFGBlock InnerSwitch = switchBlock(11, {12, 13});
  InnerSwitch.Cases.front().Value = {510};
  InnerSwitch.Cases.back().Value = {511};
  Cfg.addBlock(std::move(InnerSwitch));

  CFGBlock Inner1 = block(12, {14});
  Inner1.Statements.push_back({42});
  Cfg.addBlock(std::move(Inner1));

  CFGBlock Inner2 = block(13, {14});
  Inner2.Statements.push_back({43});
  Cfg.addBlock(std::move(Inner2));

  CFGBlock Exit = block(14, {});
  Exit.Terminator = TerminatorKind::Return;
  Exit.Statements.push_back({44});
  Cfg.addBlock(std::move(Exit));

  Cfg.addBlock(block(20, {}));
  Cfg.addBlock(block(30, {}));

  const CFGBlock *Switch0 = Cfg.getBlock(0);
  const CFGBlock *Switch1 = Cfg.getBlock(1);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  assert(Switch0->Successors.size() == 2);
  assert(Switch1->Successors.size() == 2);

  TestLoweredSwitchSimplifier Pass(
      LoweredSwitchSimplifier::defaultOptions());
  StructuringEvaluation Current;
  bool Changed = Pass.runOnGraph(Cfg, Current);

  assert(!Changed);
  assert(Cfg.getBlock(10) != nullptr);
  assert(Cfg.getBlock(11) != nullptr);
  assert(Cfg.getBlock(12) != nullptr);
  assert(Cfg.getBlock(13) != nullptr);
  assert(hasSinglePayload(Cfg.getBlock(10)->Statements, 41));
  assert(hasSinglePayload(Cfg.getBlock(12)->Statements, 42));
  assert(hasSinglePayload(Cfg.getBlock(13)->Statements, 43));
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

void testStructuringOptimizationPassEnforcesStrictlyLessGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.StrictlyLessGotos = true;
  Options.MustImproveRelativeQuality = false;
  CFGEdgeGotoRegionStructurer Structurer;
  AddFirstSuccessorPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(!Result.Succeeded);
}

void testStructuringOptimizationPassCanOverrideNewGotos() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.MustImproveRelativeQuality = false;
  CFGEdgeGotoRegionStructurer Structurer;
  AddFirstSuccessorIgnoringNewGotosPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.size() == 1);
  assert(Block0->Successors.front() == 1);
}

void testStructuringOptimizationPassUsesFilteredGotosForQuality() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {2}));
  Cfg.addBlock(block(1, {}));
  Cfg.addBlock(block(2, {}));

  StructuringOptimizationOptions Options;
  Options.RequireGotos = false;
  Options.PreventNewGotos = true;
  Options.MustImproveRelativeQuality = true;
  CFGEdgeGotoRegionStructurer Structurer;
  RetargetFirstSuccessorIgnoringNewGotosPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Result.Succeeded);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors == std::vector<BlockId>{1});
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

void testStructuringOptimizationPassRejectsOnlyRolledBackChanges() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 1;
  Options.MustImproveRelativeQuality = false;
  FailOnBlockRegionStructurer Structurer;
  RecoverThenRemoveSuccessorPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Pass.Attempts == 1);
  assert(!Result.Succeeded);
}

void testStructuringOptimizationPassRollsBackCopiedDephicationMetadata() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));

  Cfg.addBlock(block(1, {4}));

  CFGBlock Edge = block(4, {3});
  Edge.Origin = CFGBlockOrigin::Synthetic;
  Edge.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Edge.CreatedBy = CFGBlockCreator::SAILRDephication;
  Edge.SyntheticSource = 1;
  Edge.SyntheticTarget = 3;
  Edge.Statements.push_back({40});
  Cfg.addBlock(std::move(Edge));

  CFGBlock Merge = block(3, {});
  Merge.Statements.push_back({41});
  Cfg.addBlock(std::move(Merge));

  VVarId VVar = Cfg.addDephicationVVar("x", 3);
  Cfg.addDephicationIncoming(VVar, 1, 3, 4, {40}, "a");

  StructuringOptimizationOptions Options;
  Options.MaxOptIters = 2;
  Options.RequireGotos = false;
  Options.PreventNewGotos = false;
  Options.MustImproveRelativeQuality = false;
  FailOnBlockRegionStructurer Structurer;
  RecoverCopiedDephicationPass Pass(Options);
  StructuringOptimizationResult Result = Pass.analyze(Cfg, Structurer);

  assert(Pass.Attempts == 2);
  assert(Result.Succeeded);
  assert(Result.Changed);

  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.empty());
  assert(Result.Output.getBlock(99) == nullptr);
  assert(Result.Output.dephicationVVars().size() == 1);
  assert(Result.Output.dephicationIncomings().size() == 1);
}

void testStructuringOptimizationPipelineKeepsAcceptedPasses() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions RejectingOptions;
  RejectingOptions.RequireGotos = false;
  RejectingOptions.MustImproveRelativeQuality = false;

  StructuringOptimizationPipeline Pipeline;
  Pipeline.addPass(std::make_unique<RemoveFirstSuccessorPass>());
  Pipeline.addPass(std::make_unique<AddFirstSuccessorPass>(RejectingOptions));

  CFGEdgeGotoRegionStructurer Structurer;
  StructuringOptimizationPipelineResult Result = Pipeline.run(Cfg, Structurer);

  assert(Result.Changed);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.empty());
}

void testStructuringOptimizationPipelineSkipsRejectedPassAndContinues() {
  StructuredCFG Cfg;
  Cfg.addBlock(block(0, {1}));
  Cfg.addBlock(block(1, {}));

  StructuringOptimizationOptions RejectingOptions;
  RejectingOptions.RequireGotos = false;
  RejectingOptions.MustImproveRelativeQuality = false;

  StructuringOptimizationPipeline Pipeline;
  Pipeline.addPass(std::make_unique<AddFirstSuccessorPass>(RejectingOptions));
  Pipeline.addPass(std::make_unique<RemoveFirstSuccessorPass>());

  CFGEdgeGotoRegionStructurer Structurer;
  StructuringOptimizationPipelineResult Result = Pipeline.run(Cfg, Structurer);

  assert(Result.Changed);
  const CFGBlock *Block0 = Result.Output.getBlock(0);
  assert(Block0 != nullptr);
  assert(Block0->Successors.empty());
}

void testSAILRDeoptimizationPipelineMatchesAngrOrder() {
  StructuringOptimizationPipeline Pipeline = buildSAILRDeoptimizationPipeline();
  assert((Pipeline.passNames() == std::vector<std::string>{
                                      "SwitchDefaultCaseDuplicator",
                                      "DuplicationReverter",
                                      "SwitchReusedEntryRewriter",
                                      "LoweredSwitchSimplifier",
                                      "ReturnDuplicatorLow",
                                      "CrossJumpReverter",
                                  }));
}

void testSAILRDeoptimizationPipelineCanUseSharedDefaultForwarders() {
  StructuredCFG Cfg;
  Cfg.addBlock(switchBlock(0, {1, 2}));
  Cfg.addBlock(switchBlock(3, {1, 4}));
  Cfg.addBlock(block(1, {5}));
  Cfg.addBlock(block(2, {5}));
  Cfg.addBlock(block(4, {5}));
  Cfg.addBlock(block(5, {}));

  SAILRDeoptimizationPipelineOptions Options;
  Options.SharedDefaultMode =
      SwitchDefaultCaseDuplicator::SharedDefaultRewriteMode::SyntheticForwarder;

  PhoenixStructurer Structurer;
  StructuringOptimizationPipeline Pipeline =
      buildSAILRDeoptimizationPipeline(Options);
  StructuringOptimizationPipelineResult Result = Pipeline.run(Cfg, Structurer);

  assert(Result.Changed);
  const CFGBlock *Switch0 = Result.Output.getBlock(0);
  const CFGBlock *Switch1 = Result.Output.getBlock(3);
  assert(Switch0 != nullptr && Switch1 != nullptr);
  const CFGBlock *Forwarder0 =
      Result.Output.getBlock(Switch0->Successors.front());
  const CFGBlock *Forwarder1 =
      Result.Output.getBlock(Switch1->Successors.front());
  assert(Forwarder0 != nullptr && Forwarder1 != nullptr);
  assert(Forwarder0->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(Forwarder1->CopyKind == CFGBlockCopyKind::SyntheticForwarder);
  assert(Forwarder0->SyntheticTarget == 1);
  assert(Forwarder1->SyntheticTarget == 1);
}

void testSAILRDefaultSharedRewriteModeUsesSyntheticGoto() {
  SAILRDeoptimizationPipelineOptions Options =
      defaultSAILRDeoptimizationPipelineOptions();
  assert(Options.SharedDefaultMode ==
         SwitchDefaultCaseDuplicator::SharedDefaultRewriteMode::
             SyntheticGoto);
}

void testSAILRDeoptimizationDefaultOptionsMatchAngr() {
  StructuringOptimizationOptions SwitchDefaultOptions =
      SwitchDefaultCaseDuplicator::defaultOptions();
  assert(!SwitchDefaultOptions.RequireGotos);
  assert(!SwitchDefaultOptions.PreventNewGotos);
  assert(!SwitchDefaultOptions.MustImproveRelativeQuality);
  assert(SwitchDefaultOptions.MaxOptIters == 2);

  StructuringOptimizationOptions DuplicationOptions =
      DuplicationReverter::defaultOptions();
  assert(DuplicationOptions.RequireGotos);
  assert(DuplicationOptions.PreventNewGotos);
  assert(DuplicationOptions.MustImproveRelativeQuality);
  assert(DuplicationOptions.MaxOptIters == 5);

  StructuringOptimizationOptions LoweredSwitchOptions =
      LoweredSwitchSimplifier::defaultOptions();
  assert(!LoweredSwitchOptions.RequireGotos);
  assert(!LoweredSwitchOptions.PreventNewGotos);
  assert(!LoweredSwitchOptions.MustImproveRelativeQuality);
  assert(LoweredSwitchOptions.MaxOptIters == 2);

  StructuringOptimizationOptions ReturnOptions =
      ReturnDuplicatorLow::defaultOptions();
  assert(ReturnOptions.RequireGotos);
  assert(ReturnOptions.PreventNewGotos);
  assert(ReturnOptions.MaxOptIters == 4);

  StructuringOptimizationOptions CrossJumpOptions =
      CrossJumpReverter::defaultOptions();
  assert(CrossJumpOptions.RequireGotos);
  assert(CrossJumpOptions.PreventNewGotos);
  assert(CrossJumpOptions.StrictlyLessGotos);
  assert(CrossJumpOptions.MaxOptIters == 3);

  StructuringOptimizationOptions ReusedEntryOptions =
      SwitchReusedEntryRewriter::defaultOptions();
  assert(!ReusedEntryOptions.RequireGotos);
  assert(!ReusedEntryOptions.PreventNewGotos);
  assert(!ReusedEntryOptions.MustImproveRelativeQuality);
  assert(ReusedEntryOptions.MaxOptIters == 2);
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
  testGotoManagerCollectsSwitchGotoEdgeKinds();
  testStructuringEvaluatorCollectsGotoSummary();
  testStructuringEvaluatorRemovesEdgesForTrialOnly();
  testStructuredCFGDuplicatesBlockBodySource();
  testStructuredCFGDuplicateCopyKeepsOriginalBodySource();
  testStructuredCFGMaterializeRewritesCopiedPayloads();
  testStructuredCFGMaterializeFastPathReportsCommit();
  testStructuredCFGMaterializeFastPathKeepsCopiedSwitchIdentity();
  testStructuredCFGMaterializeSelfReportsFullContext();
  testStructuredCFGMaterializeCopyOfCopyKeepsSwitchIdentity();
  testStructuredCFGMaterializeReportsGroupedPredecessors();
  testStructuredCFGMaterializeRewriteFailureIsAtomic();
  testGotoStructurerRendersVirtualBlockBodySource();
  testGotoStructurerRendersSyntheticForwarder();
  testGotoStructurerRendersSyntheticGoto();
  testPhoenixStructurerRendersElseIfScope();
  testSolidityBodyBuilderRendersVirtualBlockBodySource();
  testSolidityBodyBuilderRendersSyntheticForwarder();
  testSolidityBodyBuilderConsumesStructuredSyntheticGoto();
  testLLVMFunctionCFGBuilderMaterializesPhiEdgePayloads();
  testSolidityBodyBuilderReadsSharedPhiAssignments();
  testSolidityBodyBuilderReadsCopiedSharedPhiAssignments();
  testSolidityBodyBuilderRewritesCopiedDephicationVVars();
  testStructuredCFGDuplicateDephicationEdgeCopiesMetadata();
  testStructuredCFGQueriesDephicationEdgeContext();
  testStructuredCFGQueriesCopiedDephicationEdgeWithoutCopiedMerge();
  testStructuredCFGMaterializeCopiedMergeReportsDephicationVVarCopy();
  testStructuredCFGDuplicateCopiedDephicationEdgeKeepsIdentityChain();
  testStructuredCFGRedirectDephicationEdgeRematerializesAssignment();
  testStructuredCFGRedirectDephicationEdgeFailureRollsBack();
  testStructuredCFGRemoveCopiedDephicationMergeRetiresVVar();
  testStructuredCFGRemoveBlockMaintainsDephicationMetadata();
  testStructuredCFGRemoveBlockMaterializesCopiedBody();
  testStructuredCFGRemoveBlockRejectsUnmaterializedCopy();
  testStructuredCFGRemoveBlockIsAtomicOnMaterializeFailure();
  testStructuredCFGRemoveBlocksIsAtomicOnLaterFailure();
  testStructuredCFGMaterializesCopiedSwitchWithoutRewritingTargets();
  testStructuredCFGDuplicateCopyKeepsSwitchBodySource();
  testStructuredCFGMaterializeRejectsMismatchedSwitchCases();
  testStructuredCFGRejectsInconsistentCopiedSwitchSuccessors();
  testStructuredCFGMaterializeFailsWhenBodySourceIsMissing();
  testStructuredCFGRedirectPredecessorsIsAtomic();
  testStructuredCFGRedirectPredecessorsUpdatesSwitchCases();
  testStructuredCFGReplaceEdgeUpdatesSwitchCases();
  testStructuredCFGFindsCaseOnlyPredecessors();
  testStructuredCFGSuccessorsOfIncludesCaseTargets();
  testStructuredCFGSuccessorsOfDeduplicatesCaseTargets();
  testStructuredCFGDuplicateRegionRewritesInternalEdges();
  testStructuredCFGDuplicateRegionKeepsSyntheticForwarderIdentity();
  testStructuredCFGDuplicateSyntheticForwarderReportsTargets();
  testStructuredCFGDuplicateSyntheticGotoReportsTargets();
  testStructuredCFGDuplicateSyntheticGotoKeepsCopyIdentityChain();
  testStructuredCFGMaterializeCopiedSyntheticGotoKeepsIdentity();
  testStructuredCFGDuplicateRegionRollsBackOnMissingBlock();
  testStructuredCFGCreateSyntheticBlock();
  testDuplicationReverterMergesExactDuplicateBlocks();
  testDuplicationReverterMatchesTrueAGraphDeduplication();
  testDuplicationReverterKeepsProgrammerWrittenDuplication();
  testDuplicationReverterMergesDuplicatedTailProxyShape();
  testDuplicationReverterKeepsPayloadDivergentDuplicateBranches();
  testDuplicationReverterCommitsMergeAtomically();
  testDuplicationReverterRedirectsSwitchPredecessorCases();
  testDuplicationReverterSkipsWhenDropCannotBeRemoved();
  testDuplicationReverterKeepsSyntheticIdentitySeparate();
  testDuplicationReverterKeepsCopiedSourcesSeparate();
  testDuplicationReverterFiltersFutureIrreducibleGotos();
  testDuplicationReverterKeepsGotosWithinEndpointCutoff();
  testDuplicationReverterKeepsValidEndGotos();
  testDuplicationReverterSeparatesWrittenAndMergeableDuplication();
  testDuplicationReverterExtractsGotoRelatedCommonStatementTail();
  testDuplicationReverterKeepsCommonStatementTailWithoutGotoHint();
  testDuplicationReverterSkipsCommonTailWhenRegionIsNotLinear();
  testDuplicationReverterMergesGotoRelatedLinearRegionTail();
  testDuplicationReverterSkipsGotoRelatedLinearTailWithoutHint();
  testCrossJumpReverterDuplicatesLinearGotoTarget();
  testCrossJumpReverterCommitsCopyAtomically();
  testCrossJumpReverterCopiesConnectedPredsOnce();
  testCrossJumpReverterKeepsPredSensitiveCopiesSeparate();
  testCrossJumpReverterSkipsAmbiguousSwitchCaseDefaultTarget();
  testCrossJumpReverterRedirectsSwitchCasesOnly();
  testCrossJumpReverterUsesSwitchCaseGotoKind();
  testCrossJumpReverterUsesSwitchDefaultGotoKind();
  testReturnDuplicatorLowDuplicatesGotoReturnTarget();
  testReturnDuplicatorLowSkipsLargeFunction();
  testReturnDuplicatorLowCopiesConnectedPredsOnce();
  testReturnDuplicatorLowExpandsGotoPredToConnectedComponent();
  testReturnDuplicatorLowUsesSwitchCaseGotoSource();
  testReturnDuplicatorLowNormalizesCopiedGotoTargets();
  testReturnDuplicatorLowAcceptsCopiedSwitchCaseReturnRegion();
  testReturnDuplicatorLowReportsGroupedPredecessorRewrite();
  testReturnDuplicatorLowCopiesGroupedReturnPredsWithPayloadRewrite();
  testReturnDuplicatorLowCommitsCopyAtomically();
  testReturnDuplicatorLowUsesParentGotoSource();
  testReturnDuplicatorLowSkipsBranchParentGotoSource();
  testReturnDuplicatorLowCopiesReturnRegionWithDephicationVVars();
  testReturnDuplicatorLowCopiesTerminalForkRegion();
  testReturnDuplicatorLowCopiesReturnTailForkRegion();
  testReturnDuplicatorLowCopiesBranchReturnRegionWithPayloadRewrite();
  testReturnDuplicatorLowCopiesBranchReturnRegionWithoutPredecessorRewriteSupport();
  testReturnDuplicatorLowCopiesSwitchReturnRegionWithoutPredecessorRewriteSupport();
  testReturnDuplicatorLowUsesGotoInReturnTail();
  testReturnDuplicatorLowRollsBackGroupedPredecessorFailure();
  testReturnDuplicatorLowSkipsPartialGroupedCopyOnFailure();
  testSwitchReusedEntryRewriterCreatesGotoWithoutCopyingEntryTail();
  testSwitchReusedEntryRewriterPreservesReusedEntryTail();
  testSwitchReusedEntryRewriterKeepsLowestSwitchIdEntry();
  testSwitchReusedEntryRewriterKeepsDefaultSuccessorUntouched();
  testSwitchReusedEntryRewriterRequiresRealCaseEdge();
  testSwitchReusedEntryRewriterSkipsDefaultOnlyTargets();
  testSwitchReusedEntryRewriterSkipsEntryOverReuseLimit();
  testSwitchReusedEntryRewriterSkipsTooManyReusedEntries();
  testLoweredSwitchSimplifierCopiesLinearSharedCaseRegion();
  testLoweredSwitchSimplifierKeepsPredSensitiveCopiesSeparate();
  testLoweredSwitchSimplifierCommitsCopyAtomically();
  testLoweredSwitchSimplifierSkipsPartialCopyOnFailure();
  testLoweredSwitchSimplifierSkipsUnsafeOriginalDeletion();
  testLoweredSwitchSimplifierSkipsDefaultOnlyTargets();
  testLoweredSwitchSimplifierCopiesTerminalForkCaseRegion();
  testLoweredSwitchSimplifierKeepsDefaultWhenCaseTargetIsReused();
  testLoweredSwitchSimplifierSplitsSingleCaseDefaultReuse();
  testLoweredSwitchSimplifierKeepsNestedSwitchSharedShape();
  testSwitchDefaultCaseDuplicatorCopiesReusedDefaultBlock();
  testSwitchDefaultCaseDuplicatorInsertsSharedDefaultGotosByDefault();
  testSwitchDefaultCaseDuplicatorCanUseSharedDefaultForwarders();
  testSwitchDefaultCaseDuplicatorGotosTerminalSharedDefault();
  testSwitchDefaultCaseDuplicatorKeepsCaseTargetsOnDefaultReuse();
  testLoweredSwitchSimplifierKeepsSwitchReuseShape();
  testSwitchDefaultCaseDuplicatorSkipsSwitchInternalDefaultPred();
  testSwitchDefaultCaseDuplicatorCopiesDefaultTailRegion();
  testSwitchDefaultCaseDuplicatorKeepsPredSensitiveCopiesSeparate();
  testSwitchDefaultCaseDuplicatorReportsGroupedPredecessors();
  testSwitchDefaultCaseDuplicatorRollsBackGroupedPredecessorFailure();
  testSwitchDefaultCaseDuplicatorCommitsRewriteAtomically();
  testControlFlowStructureCounterCollectsSharedQuality();
  testRelativeQualityRejectsBackwardGotoTrade();
  testRelativeQualityRejectsMoreGotoTargets();
  testStructuringOptimizationPassAcceptsImprovedGraph();
  testStructuringOptimizationPassRejectsNewGotos();
  testStructuringOptimizationPassEnforcesStrictlyLessGotos();
  testStructuringOptimizationPassCanOverrideNewGotos();
  testStructuringOptimizationPassUsesFilteredGotosForQuality();
  testStructuringOptimizationPassUsesRemovedEdgesForInitialGotos();
  testStructuringOptimizationPassRecoversAndContinuesFixedPoint();
  testStructuringOptimizationPassRejectsOnlyRolledBackChanges();
  testStructuringOptimizationPassRollsBackCopiedDephicationMetadata();
  testStructuringOptimizationPipelineKeepsAcceptedPasses();
  testStructuringOptimizationPipelineSkipsRejectedPassAndContinues();
  testSAILRDeoptimizationPipelineMatchesAngrOrder();
  testSAILRDeoptimizationPipelineCanUseSharedDefaultForwarders();
  testSAILRDefaultSharedRewriteModeUsesSyntheticGoto();
  testSAILRDeoptimizationDefaultOptionsMatchAngr();
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
