#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONOVERLAY_H

#include "notdec-backends/Structuring/Region.h"

#include <cstddef>
#include <map>
#include <optional>
#include <set>
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

enum class OverlayMemberKind {
  Block,
  Region,
  Structured,
};

enum class OverlayNodeKind {
  Block,
  Region,
  Structured,
};

// Stable identity for a node visible in an overlay graph view. Angr view nodes
// can be real graph nodes or child RegionOverlay objects. NotDec keeps the same
// split explicit so the shared graph can later move beyond raw BlockId without
// losing the current block-based renderer path.
struct OverlayNodeKey {
  OverlayNodeKind Kind = OverlayNodeKind::Block;
  BlockId Block = InvalidBlockId;
  RegionId Region = InvalidRegionId;
  NodeId StructuredRoot = InvalidNodeId;

  static OverlayNodeKey block(BlockId Id);
  static OverlayNodeKey region(RegionId Id);
  static OverlayNodeKey structured(NodeId Id,
                                   RegionId SourceRegion = InvalidRegionId);

  bool isBlock() const { return Kind == OverlayNodeKind::Block; }
  bool isRegion() const { return Kind == OverlayNodeKind::Region; }
  bool isStructured() const { return Kind == OverlayNodeKind::Structured; }
  bool operator<(const OverlayNodeKey &Other) const;
  bool operator==(const OverlayNodeKey &Other) const;
};

// One member visible in a RegionOverlay. Angr stores either graph nodes or child
// RegionOverlay objects in the same member set. NotDec keeps stable ids here so
// the existing BlockId/NodeId based reducers can migrate in small steps.
struct OverlayMember {
  OverlayMemberKind Kind = OverlayMemberKind::Block;
  BlockId Block = InvalidBlockId;
  RegionId Region = InvalidRegionId;
  NodeId StructuredRoot = InvalidNodeId;

  static OverlayMember block(BlockId Id);
  static OverlayMember region(RegionId Id);
  static OverlayMember structured(NodeId Id,
                                  RegionId SourceRegion = InvalidRegionId);
};

// One edge in an overlay-derived region view. Member views only contain
// member-to-member edges. Full views may also contain member-to-successor and
// successor-to-successor edges, matching Angr's quotient edge shape.
struct OverlayViewEdge {
  OverlayMember From;
  BlockId ExternalSource = InvalidBlockId;
  OverlayMember To;
  BlockId ExternalSuccessor = InvalidBlockId;

  bool sourcesMember() const { return ExternalSource == InvalidBlockId; }
  bool targetsMember() const { return ExternalSuccessor == InvalidBlockId; }
};

struct OverlayEdgeEndpoint {
  OverlayMember Member;
  BlockId ExternalBlock = InvalidBlockId;

  static OverlayEdgeEndpoint member(OverlayMember Member);
  static OverlayEdgeEndpoint external(BlockId Block);
  bool isMember() const { return ExternalBlock == InvalidBlockId; }
};

struct OverlayHiddenEdge {
  // Hidden edges are keyed by overlay node identity. Angr hides edges in the
  // current view, not just raw basic-block edges; structured child results need
  // the same treatment once they replace child overlays in the parent view.
  OverlayNodeKey From;
  OverlayNodeKey To;
};

// Angr's OverlayManager owns one mutable graph and makes every RegionOverlay a
// view over that graph. NotDec is still one step short of that: the manager
// owns the region tree plus finalized child results, and MutableRegionGraph
// rebuilds the current region view from this state. Keep graph-facing state
// here so the later shared-graph migration has the same boundary as Angr.
class OverlayManager {
public:
  explicit OverlayManager(RegionTree Regions);
  OverlayManager(RegionTree Regions, const StructuredCFG &Cfg);

  RegionOverlay *root();
  const RegionOverlay *root() const;

  RegionOverlay *getRegion(RegionId Id);
  const RegionOverlay *getRegion(RegionId Id) const;
  Region *getRegionData(RegionId Id);
  const Region *getRegionData(RegionId Id) const;
  const RegionTree &regionTree() const { return Regions; }
  RegionTree visibleRegionTree() const;
  RegionId parentOf(RegionId Id) const;
  RegionId ownerOf(BlockId Id) const;
  const std::vector<OverlayMember> &members(RegionId Id) const;
  OverlayNodeKey nodeKey(const OverlayMember &Member) const;
  BlockId representativeBlock(const OverlayMember &Member) const;
  const std::vector<OverlayNodeKey> &
  sharedNodeSuccessors(const OverlayNodeKey &Id) const;
  const std::vector<BlockId> &sharedSuccessors(BlockId Id) const;
  std::vector<BlockId> visibleSuccessors(RegionId Id) const;
  std::vector<OverlayViewEdge> quotientEdges(RegionId Id,
                                             bool IncludeSuccessors) const;
  void addBlockMember(RegionId Id, BlockId Block);
  void removeBlockMember(BlockId Block);
  void addNodeEdge(const OverlayNodeKey &From, const OverlayNodeKey &To);
  void detachNodeEdge(const OverlayNodeKey &From, const OverlayNodeKey &To);
  void addEdge(BlockId From, BlockId To);
  void detachEdge(BlockId From, BlockId To);
  void hideNodeEdge(RegionId Id, const OverlayNodeKey &From,
                    const OverlayNodeKey &To);
  void hideEdge(RegionId Id, BlockId From, BlockId To);
  void hideEdgeToSuccessor(RegionId Id, BlockId Successor);
  void removeEdgeWithSuccessorsOnly(RegionId Id,
                                    const OverlayEdgeEndpoint &From,
                                    const OverlayEdgeEndpoint &To);
  void addExtraFullEdge(RegionId Id, const OverlayEdgeEndpoint &From,
                        const OverlayEdgeEndpoint &To);
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
  friend class RegionOverlay;

  struct CheckpointState {
    std::map<RegionId, NodeId> StructuredRoots;
    std::map<RegionId, SuccessorSnapshot> SuccessorSnapshots;
    std::map<RegionId, std::vector<OverlayMember>> Members;
    std::map<BlockId, RegionId> BlockOwners;
    std::map<RegionId, RegionId> ParentRegions;
    std::map<OverlayNodeKey, std::vector<OverlayNodeKey>> SharedNodeSuccessors;
    std::map<BlockId, std::vector<BlockId>> SharedSuccessors;
    std::map<RegionId, std::vector<OverlayHiddenEdge>> HiddenEdges;
    std::map<RegionId, std::vector<OverlayViewEdge>> HiddenFullEdges;
    std::map<RegionId, std::vector<OverlayViewEdge>> ExtraFullEdges;
  };

  void initializeOverlayState();
  void initializeSharedGraph(const StructuredCFG &Cfg);
  const OverlayMember *memberForBlock(RegionId ViewId, BlockId Block) const;
  std::vector<BlockId> underlyingBlocks(const OverlayMember &Member) const;
  std::optional<OverlayMember> memberForEndpoint(
      RegionId ViewId, const OverlayEdgeEndpoint &Endpoint) const;
  std::optional<OverlayViewEdge> viewEdgeForEndpoints(
      RegionId ViewId, const OverlayEdgeEndpoint &From,
      const OverlayEdgeEndpoint &To) const;
  bool isHiddenEdge(RegionId Id, const OverlayNodeKey &From,
                    const OverlayNodeKey &To) const;
  bool isHiddenFullEdge(RegionId Id, const OverlayViewEdge &Edge) const;
  void rebuildBlockSuccessorCompatibility();
  void clearHiddenEdge(const OverlayNodeKey &From, const OverlayNodeKey &To);
  void clearEdgeStateForBlock(BlockId Block);
  void finalizeRegionMembers(RegionId Id, NodeId RootId);
  void dissolveRegionMembers(RegionId Id);

  RegionTree Regions;
  std::map<RegionId, RegionOverlay> Overlays;
  std::map<RegionId, RegionId> ParentRegions;
  std::map<RegionId, std::vector<OverlayMember>> Members;
  std::map<BlockId, RegionId> BlockOwners;
  std::map<OverlayNodeKey, std::vector<OverlayNodeKey>> SharedNodeSuccessors;
  std::map<BlockId, std::vector<BlockId>> SharedSuccessors;
  std::map<RegionId, std::vector<OverlayHiddenEdge>> HiddenEdges;
  std::map<RegionId, std::vector<OverlayViewEdge>> HiddenFullEdges;
  std::map<RegionId, std::vector<OverlayViewEdge>> ExtraFullEdges;
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
  void addBlockMember(BlockId Block);
  void removeBlockMember(BlockId Block);
  void addEdge(BlockId From, BlockId To);
  void detachEdge(BlockId From, BlockId To);
  void hideEdge(BlockId From, BlockId To);
  void hideEdgeToSuccessor(BlockId Successor);
  void removeEdgeWithSuccessorsOnly(const OverlayEdgeEndpoint &From,
                                    const OverlayEdgeEndpoint &To);
  void addExtraFullEdge(const OverlayEdgeEndpoint &From,
                        const OverlayEdgeEndpoint &To);
  void finalize(NodeId RootId, const SuccessorSnapshot &Snapshot = {});
  void dissolve();

private:
  OverlayManager *Manager = nullptr;
  RegionId Id = InvalidRegionId;
};

} // namespace notdec::backend::structuring

#endif
