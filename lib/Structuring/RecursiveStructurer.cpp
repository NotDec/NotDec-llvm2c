#include "notdec-backends/Structuring/RecursiveStructurer.h"

#include <map>

namespace notdec::backend::structuring {
namespace {

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
      if (Structurer.shouldPassChildRegionToParent(R, *Child) &&
          Structurer.shouldUseStructuredChildRegion(Tree, ChildRoot)) {
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
