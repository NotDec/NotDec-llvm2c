#include "notdec-backends/Structuring/GotoStructurer.h"

#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>
#include <set>

namespace notdec::backend::structuring {

static StructuredNode makeGoto(BlockId Target) {
  StructuredNode Node;
  Node.Kind = StructuredNodeKind::Goto;
  Node.Target = Target;
  return Node;
}

StructuredTree GotoStructurer::structure(const StructuredCFG &Cfg) {
  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  return RecursiveStructurer().structure(Cfg, Manager, *this);
}

NodeId GotoStructurer::structureRegion(const StructuredCFG &Cfg,
                                       const Region &R, StructuredTree &Tree) {

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  for (BlockId Id : R.Blocks) {
    const CFGBlock *BlockPtr = Cfg.getBlock(Id);
    if (BlockPtr == nullptr) {
      continue;
    }
    const CFGBlock &Block = *BlockPtr;

    StructuredNode Label;
    Label.Kind = StructuredNodeKind::Label;
    Label.Block = Block.Id;
    Root.Children.push_back(Tree.addNode(std::move(Label)));

    StructuredNode Body;
    Body.Kind = StructuredNodeKind::BasicBlock;
    Body.Block = Block.Id;
    Body.Statements = Block.Statements;
    Root.Children.push_back(Tree.addNode(std::move(Body)));

    switch (Block.Terminator) {
    case TerminatorKind::Branch: {
      StructuredNode IfNode;
      IfNode.Kind = StructuredNodeKind::If;
      IfNode.Block = Block.Id;
      IfNode.Condition = Block.Condition;
      if (!Block.Successors.empty()) {
        IfNode.Then = Tree.addNode(makeGoto(Block.Successors[0]));
        IfNode.Children.push_back(IfNode.Then);
      }
      if (Block.Successors.size() > 1) {
        IfNode.Else = Tree.addNode(makeGoto(Block.Successors[1]));
        IfNode.Children.push_back(IfNode.Else);
      }
      Root.Children.push_back(Tree.addNode(std::move(IfNode)));
      break;
    }
    case TerminatorKind::Switch: {
      StructuredNode SwitchNode;
      SwitchNode.Kind = StructuredNodeKind::Switch;
      SwitchNode.Block = Block.Id;
      SwitchNode.Condition = Block.Condition;
      SwitchNode.Cases = Block.Cases;
      if (!Block.Successors.empty()) {
        SwitchNode.Default = Tree.addNode(makeGoto(Block.Successors[0]));
      }
      for (const auto &Case : Block.Cases) {
        NodeId CaseBody = Tree.addNode(makeGoto(Case.Target));
        SwitchNode.StructuredCases.push_back(
            {Case.Value, Case.Target, CaseBody});
        SwitchNode.Children.push_back(CaseBody);
      }
      for (BlockId Succ : Block.Successors) {
        SwitchNode.Children.push_back(Tree.addNode(makeGoto(Succ)));
      }
      Root.Children.push_back(Tree.addNode(std::move(SwitchNode)));
      break;
    }
    case TerminatorKind::Fallthrough:
      for (BlockId Succ : Block.Successors) {
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

  return Tree.addNode(std::move(Root));
}

NodeId GotoStructurer::structureRegion(const StructuredCFG &Cfg,
                                       RegionOverlay &Overlay,
                                       StructuredTree &Tree) {
  const Region *R = Overlay.region();
  if (R == nullptr) {
    return InvalidNodeId;
  }

  StructuredNode Root;
  Root.Kind = StructuredNodeKind::Sequence;

  std::set<BlockId> ChildBlocks;
  for (const FinalizedChildRegion &Child :
       Overlay.manager()->finalizedChildren(Overlay.id())) {
    const Region *ChildRegion = Child.RegionData;
    if (ChildRegion != nullptr) {
      ChildBlocks.insert(ChildRegion->Blocks.begin(), ChildRegion->Blocks.end());
    }
    Root.Children.push_back(Child.StructuredRoot);
  }

  for (BlockId Id : R->Blocks) {
    if (ChildBlocks.count(Id) != 0) {
      continue;
    }
    const CFGBlock *BlockPtr = Cfg.getBlock(Id);
    if (BlockPtr == nullptr) {
      continue;
    }
    const CFGBlock &Block = *BlockPtr;

    StructuredNode Label;
    Label.Kind = StructuredNodeKind::Label;
    Label.Block = Block.Id;
    Root.Children.push_back(Tree.addNode(std::move(Label)));

    StructuredNode Body;
    Body.Kind = StructuredNodeKind::BasicBlock;
    Body.Block = Block.Id;
    Body.Statements = Block.Statements;
    Root.Children.push_back(Tree.addNode(std::move(Body)));
  }

  return Tree.addNode(std::move(Root));
}

} // namespace notdec::backend::structuring
