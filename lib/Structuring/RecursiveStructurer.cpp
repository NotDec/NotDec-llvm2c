#include "notdec-backends/Structuring/RecursiveStructurer.h"

#include <map>

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

NodeId structureRegionRecursive(
    const StructuredCFG &Cfg, const RegionTree &Regions, const Region &R,
    RegionStructurer &Structurer,
    std::map<RegionId, NodeId> &StructuredRegions, StructuredTree &Tree) {
  auto Existing = StructuredRegions.find(R.Id);
  if (Existing != StructuredRegions.end()) {
    return Existing->second;
  }

  std::map<RegionId, NodeId> StructuredChildren;
  if (Structurer.supportsChildRegions()) {
    for (RegionId ChildId : R.Children) {
      const Region *Child = Regions.getRegion(ChildId);
      if (Child == nullptr) {
        continue;
      }
      NodeId ChildRoot = structureRegionRecursive(
          Cfg, Regions, *Child, Structurer, StructuredRegions, Tree);
      if (containsStructuredControl(Tree, ChildRoot)) {
        StructuredChildren[ChildId] = ChildRoot;
      }
    }
  }

  NodeId Root = Structurer.structureRegion(Cfg, Regions, R, StructuredChildren,
                                           Tree);
  StructuredRegions[R.Id] = Root;
  return Root;
}

} // namespace

StructuredTree RecursiveStructurer::structure(const StructuredCFG &Cfg,
                                              const RegionTree &Regions,
                                              RegionStructurer &Structurer) {
  StructuredTree Tree;
  const Region *Root = Regions.getRegion(Regions.root());
  if (Root == nullptr) {
    return Tree;
  }

  std::map<RegionId, NodeId> StructuredRegions;
  NodeId RootNode = structureRegionRecursive(Cfg, Regions, *Root, Structurer,
                                             StructuredRegions, Tree);
  Tree.setRoot(RootNode);
  return Tree;
}

} // namespace notdec::backend::structuring
