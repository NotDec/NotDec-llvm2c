#include "notdec-backends/Structuring/RecursiveStructurer.h"

namespace notdec::backend::structuring {
namespace {

bool shouldFinalizeStructuredChild(const StructuredTree &Tree,
                                   const RegionOverlay &Overlay,
                                   NodeId ChildRoot) {
  if (ChildRoot == InvalidNodeId) {
    return false;
  }
  const Region *ChildRegion = Overlay.region();
  if (ChildRegion == nullptr || ChildRegion->Kind != RegionKind::NaturalLoop) {
    return true;
  }

  const StructuredNode *Node = Tree.getNode(ChildRoot);
  if (Node == nullptr) {
    return false;
  }

  // Keep fallback "wrap the whole region in while(1)" loops visible to the
  // parent CFG until overlay graph mutation is implemented. Real while/do-while
  // nodes can already propagate as stable child results.
  return Node->Kind != StructuredNodeKind::InfiniteLoop;
}

NodeId structureRegionRecursive(const StructuredCFG &Cfg, OverlayManager &Manager,
                                RegionOverlay &Overlay,
                                RegionStructurer &Structurer,
                                StructuredTree &Tree) {
  if (Structurer.supportsChildRegions()) {
    for (RegionId ChildId : Overlay.children()) {
      RegionOverlay *Child = Manager.getRegion(ChildId);
      if (Child == nullptr) {
        continue;
      }
      NodeId ChildRoot =
          structureRegionRecursive(Cfg, Manager, *Child, Structurer, Tree);
      if (shouldFinalizeStructuredChild(Tree, *Child, ChildRoot)) {
        Child->finalize(ChildRoot);
      } else {
        Child->dissolve();
      }
    }
  }

  const Region *R = Overlay.region();
  if (R == nullptr) {
    return InvalidNodeId;
  }
  NodeId Root = Structurer.structureRegion(Cfg, Overlay, Tree);
  if (Root != InvalidNodeId) {
    Overlay.finalize(Root);
  } else {
    Overlay.dissolve();
  }
  return Root;
}

} // namespace

StructuredTree RecursiveStructurer::structure(const StructuredCFG &Cfg,
                                              const RegionTree &Regions,
                                              RegionStructurer &Structurer) {
  OverlayManager Manager(Regions);
  return structure(Cfg, Manager, Structurer);
}

StructuredTree RecursiveStructurer::structure(const StructuredCFG &Cfg,
                                              OverlayManager &Manager,
                                              RegionStructurer &Structurer) {
  StructuredTree Tree;
  RegionOverlay *Root = Manager.root();
  if (Root == nullptr) {
    return Tree;
  }

  NodeId RootNode =
      structureRegionRecursive(Cfg, Manager, *Root, Structurer, Tree);
  Tree.setRoot(RootNode);
  return Tree;
}

} // namespace notdec::backend::structuring
