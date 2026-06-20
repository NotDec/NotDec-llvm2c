#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H

#include "notdec-backends/Structuring/Region.h"

#include <map>
#include <optional>
#include <vector>

namespace notdec::backend::structuring {

class RegionOverlay;

struct FinalizedChildRegion {
  const RegionOverlay *Overlay = nullptr;
  const Region *RegionData = nullptr;
  NodeId StructuredRoot = InvalidNodeId;
};

// This is the first C++ step toward angr's OverlayManager/RegionOverlay model.
// The full graph mutation API will be filled in as Phoenix/SAILR move over.
// For now it gives RecursiveStructurer a shared region tree object instead of
// materializing child results out-of-band.
class OverlayManager {
public:
  explicit OverlayManager(RegionTree Regions);

  RegionOverlay *root();
  const RegionOverlay *root() const;

  RegionOverlay *getRegion(RegionId Id);
  const RegionOverlay *getRegion(RegionId Id) const;
  Region *getRegionData(RegionId Id);
  const Region *getRegionData(RegionId Id) const;
  const RegionTree &regionTree() const { return Regions; }
  RegionTree visibleRegionTree() const;
  NodeId getStructuredRoot(RegionId Id) const;
  bool isRegionFinalized(RegionId Id) const;
  std::vector<FinalizedChildRegion> finalizedChildren(RegionId Id) const;
  void setStructuredRoot(RegionId Id, NodeId RootId);
  void clearStructuredRoot(RegionId Id);

private:
  RegionTree Regions;
  std::map<RegionId, RegionOverlay> Overlays;
  std::map<RegionId, NodeId> StructuredRoots;
};

class RegionOverlay {
public:
  RegionOverlay() = default;
  RegionOverlay(OverlayManager *Manager, RegionId Id);

  RegionId id() const { return Id; }
  const Region *region() const;
  Region *region();
  OverlayManager *manager() const { return Manager; }

  RegionKind kind() const;
  BlockId head() const;
  BlockId latch() const;
  BlockId follow() const;
  const std::vector<BlockId> &blocks() const;
  const std::vector<BlockId> &successors() const;
  const std::vector<RegionId> &children() const;
  bool isCollapsed() const;
  NodeId structuredRoot() const;
  void finalize(NodeId RootId);
  void dissolve();

private:
  OverlayManager *Manager = nullptr;
  RegionId Id = InvalidRegionId;
};

} // namespace notdec::backend::structuring

#endif
