#include "notdec-backends/Structuring/RecursiveStructurer.h"

#include <map>
#include <set>
#include <vector>

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

void finishChildRegion(const StructuredTree &Tree, RegionOverlay &Overlay,
                       NodeId Root) {
  if (shouldFinalizeStructuredChild(Tree, Overlay, Root)) {
    Overlay.finalize(Root);
  } else {
    Overlay.dissolve();
  }
}

RegionOverlay *nextUnprocessedChild(OverlayManager &Manager,
                                    const RegionOverlay &Overlay,
                                    const std::set<RegionId> &Processed) {
  for (RegionId ChildId : Overlay.children()) {
    if (Processed.count(ChildId) != 0) {
      continue;
    }
    RegionOverlay *Child = Manager.getRegion(ChildId);
    if (Child != nullptr) {
      return Child;
    }
  }
  return nullptr;
}

NodeId structureOverlayTree(const StructuredCFG &Cfg, OverlayManager &Manager,
                            RegionOverlay &Root,
                            RegionStructurer &Structurer,
                            StructuredTree &Tree) {
  std::vector<RegionId> Stack = {Root.id()};
  std::set<RegionId> Processed;
  std::map<RegionId, NodeId> Results;

  while (!Stack.empty()) {
    RegionOverlay *Current = Manager.getRegion(Stack.back());
    if (Current == nullptr) {
      Processed.insert(Stack.back());
      Stack.pop_back();
      continue;
    }

    if (Structurer.supportsChildRegions()) {
      RegionOverlay *Child =
          nextUnprocessedChild(Manager, *Current, Processed);
      if (Child != nullptr) {
        Stack.push_back(Child->id());
        continue;
      }
    }

    Stack.pop_back();
    const Region *R = Current->region();
    NodeId RootNode =
        R == nullptr ? InvalidNodeId
                     : Structurer.structureRegion(Cfg, *Current, Tree);
    Results[Current->id()] = RootNode;
    Processed.insert(Current->id());

    if (Current->id() != Root.id()) {
      finishChildRegion(Tree, *Current, RootNode);
    }
  }

  auto It = Results.find(Root.id());
  return It == Results.end() ? InvalidNodeId : It->second;
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

  std::size_t Checkpoint = Manager.checkpoint();
  NodeId RootNode = structureOverlayTree(Cfg, Manager, *Root, Structurer, Tree);
  Manager.rollback(Checkpoint);
  Manager.commit(Checkpoint);
  Tree.setRoot(RootNode);
  return Tree;
}

} // namespace notdec::backend::structuring
