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

  for (NodeId Child : Node->Children) {
    collectGotos(Tree, Child, Source, StructuredGotoEdgeKind::Unknown, Gotos);
    const StructuredNode *ChildNode = Tree.getNode(Child);
    if (ChildNode != nullptr && ChildNode->Block != InvalidBlockId) {
      Source = ChildNode->Block;
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
