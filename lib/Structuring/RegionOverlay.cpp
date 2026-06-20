#include "notdec-backends/Structuring/RegionOverlay.h"

namespace notdec::backend::structuring {

OverlayManager::OverlayManager(RegionTree Regions) : Regions(std::move(Regions)) {
  for (const Region &R : this->Regions.regions()) {
    Overlays.emplace(R.Id, RegionOverlay(this, R.Id));
  }
}

RegionOverlay *OverlayManager::root() {
  return getRegion(Regions.root());
}

const RegionOverlay *OverlayManager::root() const {
  return getRegion(Regions.root());
}

RegionOverlay *OverlayManager::getRegion(RegionId Id) {
  auto It = Overlays.find(Id);
  return It == Overlays.end() ? nullptr : &It->second;
}

const RegionOverlay *OverlayManager::getRegion(RegionId Id) const {
  auto It = Overlays.find(Id);
  return It == Overlays.end() ? nullptr : &It->second;
}

Region *OverlayManager::getRegionData(RegionId Id) {
  return Regions.getRegion(Id);
}

const Region *OverlayManager::getRegionData(RegionId Id) const {
  return Regions.getRegion(Id);
}

RegionTree OverlayManager::visibleRegionTree() const {
  RegionTree Visible = Regions;
  for (Region &R : Visible.regions()) {
    std::vector<RegionId> VisibleChildren;
    VisibleChildren.reserve(R.Children.size());
    for (const FinalizedChildRegion &Child : finalizedChildren(R.Id)) {
      VisibleChildren.push_back(Child.RegionData->Id);
    }
    R.Children = std::move(VisibleChildren);
  }
  return Visible;
}

NodeId OverlayManager::getStructuredRoot(RegionId Id) const {
  auto It = StructuredRoots.find(Id);
  return It == StructuredRoots.end() ? InvalidNodeId : It->second;
}

bool OverlayManager::isRegionFinalized(RegionId Id) const {
  return getStructuredRoot(Id) != InvalidNodeId;
}

std::vector<FinalizedChildRegion>
OverlayManager::finalizedChildren(RegionId Id) const {
  std::vector<FinalizedChildRegion> Result;
  const Region *Parent = getRegionData(Id);
  if (Parent == nullptr) {
    return Result;
  }

  Result.reserve(Parent->Children.size());
  for (RegionId ChildId : Parent->Children) {
    const RegionOverlay *ChildOverlay = getRegion(ChildId);
    const Region *ChildRegion = getRegionData(ChildId);
    NodeId StructuredRoot = getStructuredRoot(ChildId);
    if (ChildOverlay == nullptr || ChildRegion == nullptr ||
        StructuredRoot == InvalidNodeId) {
      continue;
    }
    Result.push_back({ChildOverlay, ChildRegion, StructuredRoot});
  }
  return Result;
}

std::size_t OverlayManager::checkpoint() {
  StructuredRootCheckpoints.push_back(StructuredRoots);
  return StructuredRootCheckpoints.size() - 1;
}

void OverlayManager::rollback(std::size_t Checkpoint) {
  if (Checkpoint >= StructuredRootCheckpoints.size()) {
    return;
  }
  StructuredRoots = StructuredRootCheckpoints[Checkpoint];
}

void OverlayManager::commit(std::size_t Checkpoint) {
  if (Checkpoint >= StructuredRootCheckpoints.size()) {
    return;
  }
  StructuredRootCheckpoints.erase(StructuredRootCheckpoints.begin() +
                                  Checkpoint,
                                  StructuredRootCheckpoints.end());
}

void OverlayManager::setStructuredRoot(RegionId Id, NodeId RootId) {
  if (RootId == InvalidNodeId) {
    StructuredRoots.erase(Id);
    return;
  }
  StructuredRoots[Id] = RootId;
}

void OverlayManager::clearStructuredRoot(RegionId Id) {
  StructuredRoots.erase(Id);
}

RegionOverlay::RegionOverlay(OverlayManager *Manager, RegionId Id)
    : Manager(Manager), Id(Id) {}

const Region *RegionOverlay::region() const {
  return Manager == nullptr ? nullptr : Manager->getRegionData(Id);
}

Region *RegionOverlay::region() {
  return Manager == nullptr ? nullptr : Manager->getRegionData(Id);
}

RegionKind RegionOverlay::kind() const {
  const Region *R = region();
  return R == nullptr ? RegionKind::Root : R->Kind;
}

BlockId RegionOverlay::head() const {
  const Region *R = region();
  return R == nullptr ? InvalidBlockId : R->Head;
}

BlockId RegionOverlay::latch() const {
  const Region *R = region();
  return R == nullptr ? InvalidBlockId : R->Latch;
}

BlockId RegionOverlay::follow() const {
  const Region *R = region();
  return R == nullptr ? InvalidBlockId : R->Follow;
}

const std::vector<BlockId> &RegionOverlay::blocks() const {
  static const std::vector<BlockId> Empty;
  const Region *R = region();
  return R == nullptr ? Empty : R->Blocks;
}

const std::vector<BlockId> &RegionOverlay::successors() const {
  static const std::vector<BlockId> Empty;
  const Region *R = region();
  return R == nullptr ? Empty : R->Successors;
}

const std::vector<RegionId> &RegionOverlay::children() const {
  static const std::vector<RegionId> Empty;
  const Region *R = region();
  return R == nullptr ? Empty : R->Children;
}

bool RegionOverlay::isCollapsed() const {
  return structuredRoot() != InvalidNodeId;
}

NodeId RegionOverlay::structuredRoot() const {
  return Manager == nullptr ? InvalidNodeId : Manager->getStructuredRoot(Id);
}

void RegionOverlay::finalize(NodeId RootId) {
  if (Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  Manager->setStructuredRoot(Id, RootId);
}

void RegionOverlay::dissolve() {
  if (Manager == nullptr) {
    return;
  }
  Manager->clearStructuredRoot(Id);
}

} // namespace notdec::backend::structuring
