#include "notdec-backends/Structuring/RecursiveStructurer.h"

namespace notdec::backend::structuring {

StructuredTree RecursiveStructurer::structure(const StructuredCFG &Cfg,
                                              const RegionTree &Regions,
                                              RegionStructurer &Structurer) {
  StructuredTree Tree;
  const Region *Root = Regions.getRegion(Regions.root());
  if (Root == nullptr) {
    return Tree;
  }

  NodeId RootNode = Structurer.structureRegion(Cfg, *Root, Tree);
  Tree.setRoot(RootNode);
  return Tree;
}

} // namespace notdec::backend::structuring
