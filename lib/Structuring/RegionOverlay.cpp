#include "notdec-backends/Structuring/RegionOverlay.h"

#include <algorithm>
#include <set>
#include <utility>

namespace notdec::backend::structuring {

namespace {

void appendUniqueBlock(std::vector<BlockId> &Values, BlockId Id) {
  if (std::find(Values.begin(), Values.end(), Id) == Values.end()) {
    Values.push_back(Id);
  }
}

bool sameMember(const OverlayMember &Lhs, const OverlayMember &Rhs) {
  if (Lhs.Kind != Rhs.Kind) {
    return false;
  }
  switch (Lhs.Kind) {
  case OverlayMemberKind::Block:
    return Lhs.Block == Rhs.Block;
  case OverlayMemberKind::Region:
    return Lhs.Region == Rhs.Region;
  case OverlayMemberKind::Structured:
    return Lhs.Region == Rhs.Region &&
           Lhs.StructuredRoot == Rhs.StructuredRoot;
  }
  return false;
}

bool sameEdge(const OverlayViewEdge &Lhs, const OverlayViewEdge &Rhs) {
  return sameMember(Lhs.From, Rhs.From) && sameMember(Lhs.To, Rhs.To) &&
         Lhs.ExternalSuccessor == Rhs.ExternalSuccessor;
}

void appendUniqueEdge(std::vector<OverlayViewEdge> &Edges,
                      const OverlayViewEdge &Edge) {
  auto It = std::find_if(Edges.begin(), Edges.end(),
                         [&](const OverlayViewEdge &Existing) {
                           return sameEdge(Existing, Edge);
                         });
  if (It == Edges.end()) {
    Edges.push_back(Edge);
  }
}

} // namespace

OverlayMember OverlayMember::block(BlockId Id) {
  OverlayMember Member;
  Member.Kind = OverlayMemberKind::Block;
  Member.Block = Id;
  return Member;
}

OverlayMember OverlayMember::region(RegionId Id) {
  OverlayMember Member;
  Member.Kind = OverlayMemberKind::Region;
  Member.Region = Id;
  return Member;
}

OverlayMember OverlayMember::structured(NodeId Id, RegionId SourceRegion) {
  OverlayMember Member;
  Member.Kind = OverlayMemberKind::Structured;
  Member.Region = SourceRegion;
  Member.StructuredRoot = Id;
  return Member;
}

OverlayManager::OverlayManager(RegionTree Regions) : Regions(std::move(Regions)) {
  for (const Region &R : this->Regions.regions()) {
    Overlays.emplace(R.Id, RegionOverlay(this, R.Id));
  }
  initializeOverlayState();
}

OverlayManager::OverlayManager(RegionTree Regions, const StructuredCFG &Cfg)
    : OverlayManager(std::move(Regions)) {
  initializeSharedGraph(Cfg);
}

void OverlayManager::initializeOverlayState() {
  ParentRegions.clear();
  Members.clear();
  BlockOwners.clear();

  for (const Region &R : Regions.regions()) {
    Members[R.Id];
    for (RegionId ChildId : R.Children) {
      ParentRegions[ChildId] = R.Id;
    }
  }

  for (const Region &R : Regions.regions()) {
    std::set<BlockId> ChildBlocks;
    for (RegionId ChildId : R.Children) {
      const Region *Child = getRegionData(ChildId);
      if (Child != nullptr) {
        ChildBlocks.insert(Child->Blocks.begin(), Child->Blocks.end());
      }
      Members[R.Id].push_back(OverlayMember::region(ChildId));
    }

    for (BlockId Block : R.Blocks) {
      if (ChildBlocks.count(Block) != 0) {
        continue;
      }
      Members[R.Id].push_back(OverlayMember::block(Block));
      BlockOwners[Block] = R.Id;
    }
  }
}

void OverlayManager::initializeSharedGraph(const StructuredCFG &Cfg) {
  SharedSuccessors.clear();
  for (const CFGBlock &Block : Cfg.blocks()) {
    SharedSuccessors[Block.Id] = Block.Successors;
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

RegionId OverlayManager::parentOf(RegionId Id) const {
  auto It = ParentRegions.find(Id);
  return It == ParentRegions.end() ? InvalidRegionId : It->second;
}

RegionId OverlayManager::ownerOf(BlockId Id) const {
  auto It = BlockOwners.find(Id);
  return It == BlockOwners.end() ? InvalidRegionId : It->second;
}

const std::vector<OverlayMember> &OverlayManager::members(RegionId Id) const {
  static const std::vector<OverlayMember> Empty;
  auto It = Members.find(Id);
  return It == Members.end() ? Empty : It->second;
}

const std::vector<BlockId> &OverlayManager::sharedSuccessors(BlockId Id) const {
  static const std::vector<BlockId> Empty;
  auto It = SharedSuccessors.find(Id);
  return It == SharedSuccessors.end() ? Empty : It->second;
}

const OverlayMember *OverlayManager::memberForBlock(RegionId ViewId,
                                                    BlockId Block) const {
  const std::vector<OverlayMember> &ViewMembers = members(ViewId);
  for (const OverlayMember &Member : ViewMembers) {
    if (Member.Kind == OverlayMemberKind::Block && Member.Block == Block) {
      return &Member;
    }
    if (Member.Kind == OverlayMemberKind::Region ||
        Member.Kind == OverlayMemberKind::Structured) {
      const Region *MemberRegion = getRegionData(Member.Region);
      if (MemberRegion != nullptr &&
          std::find(MemberRegion->Blocks.begin(), MemberRegion->Blocks.end(),
                    Block) != MemberRegion->Blocks.end()) {
        return &Member;
      }
    }
  }
  return nullptr;
}

std::vector<BlockId> OverlayManager::visibleSuccessors(RegionId Id) const {
  std::vector<BlockId> Result;
  const std::vector<OverlayMember> &ViewMembers = members(Id);
  for (const OverlayMember &Member : ViewMembers) {
    std::vector<BlockId> Blocks;
    if (Member.Kind == OverlayMemberKind::Block) {
      Blocks.push_back(Member.Block);
    } else {
      const Region *MemberRegion = getRegionData(Member.Region);
      if (MemberRegion != nullptr) {
        Blocks = MemberRegion->Blocks;
      }
    }

    for (BlockId Block : Blocks) {
      for (BlockId Succ : sharedSuccessors(Block)) {
        if (memberForBlock(Id, Succ) == &Member) {
          continue;
        }
        if (memberForBlock(Id, Succ) != nullptr) {
          continue;
        }
        appendUniqueBlock(Result, Succ);
      }
    }
  }
  return Result;
}

std::vector<OverlayViewEdge>
OverlayManager::quotientEdges(RegionId Id, bool IncludeSuccessors) const {
  std::vector<OverlayViewEdge> Result;
  const std::vector<OverlayMember> &ViewMembers = members(Id);
  for (const OverlayMember &Member : ViewMembers) {
    std::vector<BlockId> Blocks;
    if (Member.Kind == OverlayMemberKind::Block) {
      Blocks.push_back(Member.Block);
    } else {
      const Region *MemberRegion = getRegionData(Member.Region);
      if (MemberRegion != nullptr) {
        Blocks = MemberRegion->Blocks;
      }
    }

    for (BlockId Block : Blocks) {
      for (BlockId Succ : sharedSuccessors(Block)) {
        const OverlayMember *SuccMember = memberForBlock(Id, Succ);
        if (SuccMember != nullptr) {
          if (sameMember(Member, *SuccMember) &&
              Member.Kind != OverlayMemberKind::Block) {
            continue;
          }
          appendUniqueEdge(Result, {Member, *SuccMember, InvalidBlockId});
          continue;
        }
        if (IncludeSuccessors) {
          appendUniqueEdge(Result, {Member, {}, Succ});
        }
      }
    }
  }
  return Result;
}

void OverlayManager::finalizeRegionMembers(RegionId Id, NodeId RootId) {
  RegionId ParentId = parentOf(Id);
  if (ParentId == InvalidRegionId) {
    return;
  }

  std::vector<OverlayMember> &ParentMembers = Members[ParentId];
  for (OverlayMember &Member : ParentMembers) {
    if (Member.Kind == OverlayMemberKind::Region && Member.Region == Id) {
      Member = OverlayMember::structured(RootId, Id);
      break;
    }
  }
}

void OverlayManager::dissolveRegionMembers(RegionId Id) {
  RegionId ParentId = parentOf(Id);
  if (ParentId == InvalidRegionId) {
    return;
  }

  std::vector<OverlayMember> ChildMembers = Members[Id];
  std::vector<OverlayMember> &ParentMembers = Members[ParentId];
  std::vector<OverlayMember> Rebuilt;
  Rebuilt.reserve(ParentMembers.size() + ChildMembers.size());
  for (const OverlayMember &Member : ParentMembers) {
    if (Member.Kind == OverlayMemberKind::Region && Member.Region == Id) {
      Rebuilt.insert(Rebuilt.end(), ChildMembers.begin(), ChildMembers.end());
      continue;
    }
    Rebuilt.push_back(Member);
  }
  ParentMembers = std::move(Rebuilt);
  Members[Id].clear();

  for (const OverlayMember &Member : ChildMembers) {
    if (Member.Kind == OverlayMemberKind::Block) {
      BlockOwners[Member.Block] = ParentId;
    } else if (Member.Kind == OverlayMemberKind::Region) {
      ParentRegions[Member.Region] = ParentId;
    }
  }
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
    auto SnapshotIt = SuccessorSnapshots.find(ChildId);
    SuccessorSnapshot Snapshot =
        SnapshotIt == SuccessorSnapshots.end() ? SuccessorSnapshot{}
                                               : SnapshotIt->second;
    Result.push_back({ChildOverlay, ChildRegion, StructuredRoot, Snapshot});
  }
  return Result;
}

std::size_t OverlayManager::checkpoint() {
  StructuredRootCheckpoints.push_back(
      {StructuredRoots, SuccessorSnapshots, Members, BlockOwners,
       ParentRegions, SharedSuccessors});
  return StructuredRootCheckpoints.size() - 1;
}

void OverlayManager::rollback(std::size_t Checkpoint) {
  if (Checkpoint >= StructuredRootCheckpoints.size()) {
    return;
  }
  StructuredRoots = StructuredRootCheckpoints[Checkpoint].StructuredRoots;
  SuccessorSnapshots =
      StructuredRootCheckpoints[Checkpoint].SuccessorSnapshots;
  Members = StructuredRootCheckpoints[Checkpoint].Members;
  BlockOwners = StructuredRootCheckpoints[Checkpoint].BlockOwners;
  ParentRegions = StructuredRootCheckpoints[Checkpoint].ParentRegions;
  SharedSuccessors = StructuredRootCheckpoints[Checkpoint].SharedSuccessors;
}

void OverlayManager::commit(std::size_t Checkpoint) {
  if (Checkpoint >= StructuredRootCheckpoints.size()) {
    return;
  }
  StructuredRootCheckpoints.erase(StructuredRootCheckpoints.begin() +
                                  Checkpoint,
                                  StructuredRootCheckpoints.end());
}

void OverlayManager::setStructuredRoot(RegionId Id, NodeId RootId,
                                       const SuccessorSnapshot &Snapshot) {
  if (RootId == InvalidNodeId) {
    clearStructuredRoot(Id);
    return;
  }
  StructuredRoots[Id] = RootId;
  SuccessorSnapshots[Id] = Snapshot;
  finalizeRegionMembers(Id, RootId);
}

void OverlayManager::clearStructuredRoot(RegionId Id) {
  StructuredRoots.erase(Id);
  SuccessorSnapshots.erase(Id);
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

SuccessorSnapshot RegionOverlay::snapshotSuccessors() const {
  SuccessorSnapshot Snapshot;
  if (Manager != nullptr) {
    Snapshot.Successors = Manager->visibleSuccessors(Id);
  }
  if (Snapshot.Successors.empty()) {
    Snapshot.Successors = successors();
  }
  return Snapshot;
}

void RegionOverlay::finalize(NodeId RootId, const SuccessorSnapshot &Snapshot) {
  (void)Snapshot;
  if (Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  Manager->setStructuredRoot(Id, RootId, Snapshot);
}

void RegionOverlay::dissolve() {
  if (Manager == nullptr) {
    return;
  }
  Manager->dissolveRegionMembers(Id);
  Manager->clearStructuredRoot(Id);
}

} // namespace notdec::backend::structuring
