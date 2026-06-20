#include "notdec-backends/Structuring/RegionStructurer.h"

namespace notdec::backend::structuring {
namespace {

bool containsStructuredControl(const StructuredTree &Tree, NodeId Id) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return false;
  }
  if (Node->Kind == StructuredNodeKind::Switch ||
      Node->Kind == StructuredNodeKind::While ||
      Node->Kind == StructuredNodeKind::DoWhile ||
      Node->Kind == StructuredNodeKind::InfiniteLoop) {
    return true;
  }

  for (NodeId Child : Node->Children) {
    if (containsStructuredControl(Tree, Child)) {
      return true;
    }
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    if (containsStructuredControl(Tree, Case.Body)) {
      return true;
    }
  }
  return containsStructuredControl(Tree, Node->Then) ||
         containsStructuredControl(Tree, Node->Else) ||
         containsStructuredControl(Tree, Node->Body) ||
         containsStructuredControl(Tree, Node->Default);
}

} // namespace

bool RegionStructurer::shouldUseStructuredChildRegion(
    const StructuredTree &Tree, NodeId ChildRoot) const {
  return containsStructuredControl(Tree, ChildRoot);
}

} // namespace notdec::backend::structuring
