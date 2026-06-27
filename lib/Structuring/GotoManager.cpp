#include "notdec-backends/Structuring/GotoManager.h"

namespace notdec::backend::structuring {
namespace {

BlockId sourceBlockForNode(const StructuredNode &Node, BlockId CurrentSource) {
  switch (Node.Kind) {
  case StructuredNodeKind::BasicBlock:
  case StructuredNodeKind::If:
  case StructuredNodeKind::Switch:
  case StructuredNodeKind::While:
  case StructuredNodeKind::DoWhile:
  case StructuredNodeKind::InfiniteLoop:
  case StructuredNodeKind::Return:
  case StructuredNodeKind::Unreachable:
    return Node.Block == InvalidBlockId ? CurrentSource : Node.Block;
  case StructuredNodeKind::Label:
    return CurrentSource;
  case StructuredNodeKind::Sequence:
  case StructuredNodeKind::Goto:
  case StructuredNodeKind::Break:
  case StructuredNodeKind::Continue:
    return CurrentSource;
  }
  return CurrentSource;
}

bool preservesSwitchEdgeContext(StructuredNodeKind Kind) {
  switch (Kind) {
  case StructuredNodeKind::Sequence:
  case StructuredNodeKind::BasicBlock:
  case StructuredNodeKind::Label:
  case StructuredNodeKind::Goto:
    return true;
  case StructuredNodeKind::If:
  case StructuredNodeKind::Switch:
  case StructuredNodeKind::Break:
  case StructuredNodeKind::Continue:
  case StructuredNodeKind::While:
  case StructuredNodeKind::DoWhile:
  case StructuredNodeKind::InfiniteLoop:
  case StructuredNodeKind::Return:
  case StructuredNodeKind::Unreachable:
    return false;
  }
  return false;
}

StructuredGotoEdgeKind childEdgeKind(const StructuredNode &Parent,
                                     const StructuredNode *Child,
                                     StructuredGotoEdgeKind EdgeKind) {
  // Switch case/default bodies may be wrapped in a Sequence with labels and
  // block bodies before the real synthetic goto. Keep the switch edge identity
  // through that linear shell, but stop at nested control structures.
  if (Parent.Kind == StructuredNodeKind::Sequence && Child != nullptr &&
      preservesSwitchEdgeContext(Child->Kind)) {
    return EdgeKind;
  }
  return StructuredGotoEdgeKind::Unknown;
}

bool collectsChildList(StructuredNodeKind Kind) {
  return Kind == StructuredNodeKind::Sequence;
}

void collectGotos(const StructuredTree &Tree, NodeId Id, BlockId CurrentSource,
                  StructuredGotoEdgeKind EdgeKind,
                  std::set<StructuredGoto> &Gotos) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return;
  }

  BlockId Source = sourceBlockForNode(*Node, CurrentSource);
  if (Node->Kind == StructuredNodeKind::Goto &&
      Node->Target != InvalidBlockId) {
    Gotos.insert({Source, Node->Target, Id, EdgeKind});
  }

  if (collectsChildList(Node->Kind)) {
    for (NodeId Child : Node->Children) {
      const StructuredNode *ChildNode = Tree.getNode(Child);
      collectGotos(Tree, Child, Source,
                   childEdgeKind(*Node, ChildNode, EdgeKind), Gotos);
      if (ChildNode != nullptr && ChildNode->Block != InvalidBlockId) {
        Source = ChildNode->Block;
      }
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    // A case body is entered through its case target, not through the switch
    // header. Dephication edge blocks may sit between the case block and the
    // merge, so later SAILR passes need the goto source to stay on that case
    // side of the edge.
    BlockId CaseSource = Case.Target == InvalidBlockId ? Source : Case.Target;
    collectGotos(Tree, Case.Body, CaseSource,
                 StructuredGotoEdgeKind::SwitchCase, Gotos);
  }
  collectGotos(Tree, Node->Then, Source, StructuredGotoEdgeKind::Unknown,
               Gotos);
  collectGotos(Tree, Node->Else, Source, StructuredGotoEdgeKind::Unknown,
               Gotos);
  collectGotos(Tree, Node->Body, Source, StructuredGotoEdgeKind::Unknown,
               Gotos);
  collectGotos(Tree, Node->Default, Source,
               StructuredGotoEdgeKind::SwitchDefault, Gotos);
}

} // namespace

GotoManager GotoManager::collect(const StructuredTree &Tree) {
  GotoManager Manager;
  collectGotos(Tree, Tree.root(), InvalidBlockId,
               StructuredGotoEdgeKind::Unknown, Manager.Gotos);
  return Manager;
}

GotoManager GotoManager::fromGotos(const std::vector<StructuredGoto> &Gotos) {
  GotoManager Manager;
  Manager.Gotos.insert(Gotos.begin(), Gotos.end());
  return Manager;
}

std::vector<StructuredGoto> GotoManager::gotosInBlock(BlockId Source) const {
  std::vector<StructuredGoto> Result;
  for (const StructuredGoto &Goto : Gotos) {
    if (Goto.Source == Source) {
      Result.push_back(Goto);
    }
  }
  return Result;
}

bool GotoManager::isGotoEdge(BlockId Source, BlockId Target) const {
  for (const StructuredGoto &Goto : Gotos) {
    if (Goto.Source == Source && Goto.Target == Target) {
      return true;
    }
  }
  return false;
}

} // namespace notdec::backend::structuring
