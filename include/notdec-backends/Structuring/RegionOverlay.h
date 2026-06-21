#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H

#include "notdec-backends/Structuring/Region.h"

#include <cstddef>
#include <map>
#include <optional>
#include <vector>

namespace notdec::backend::structuring {

class RegionOverlay;

struct SuccessorSnapshot {
  std::vector<BlockId> Successors;
};

struct FinalizedChildRegion {
  const RegionOverlay *Overlay = nullptr;
  const Region *RegionData = nullptr;
  NodeId StructuredRoot = InvalidNodeId;
  SuccessorSnapshot Snapshot;
};

// Angr's OverlayManager owns one mutable graph and makes every RegionOverlay a
// view over that graph. NotDec is still one step short of that: the manager
// owns the region tree plus finalized child results, and MutableRegionGraph
// rebuilds the current region view from this state. Keep graph-facing state
// here so the later shared-graph migration has the same boundary as Angr.
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
  std::size_t checkpoint();
  void rollback(std::size_t Checkpoint);
  void commit(std::size_t Checkpoint);
  void setStructuredRoot(RegionId Id, NodeId RootId,
                         const SuccessorSnapshot &Snapshot = {});
  void clearStructuredRoot(RegionId Id);

private:
  struct CheckpointState {
    std::map<RegionId, NodeId> StructuredRoots;
    std::map<RegionId, SuccessorSnapshot> SuccessorSnapshots;
  };

  RegionTree Regions;
  std::map<RegionId, RegionOverlay> Overlays;
  std::map<RegionId, NodeId> StructuredRoots;
  std::map<RegionId, SuccessorSnapshot> SuccessorSnapshots;
  std::vector<CheckpointState> StructuredRootCheckpoints;
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
  SuccessorSnapshot snapshotSuccessors() const;
  void finalize(NodeId RootId, const SuccessorSnapshot &Snapshot = {});
  void dissolve();

private:
  OverlayManager *Manager = nullptr;
  RegionId Id = InvalidRegionId;
};

} // namespace notdec::backend::structuring

#endif
