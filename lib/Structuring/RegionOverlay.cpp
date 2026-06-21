#include "notdec-backends/Structuring/RegionOverlay.h"

#include <algorithm>
#include <set>
#include <tuple>
#include <utility>

namespace notdec::backend::structuring {

namespace {

void appendUniqueBlock(std::vector<BlockId> &Values, BlockId Id) {
  if (std::find(Values.begin(), Values.end(), Id) == Values.end()) {
    Values.push_back(Id);
  }
}

void appendUniqueNodeKey(std::vector<OverlayNodeKey> &Values,
                         const OverlayNodeKey &Id) {
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
  return sameMember(Lhs.From, Rhs.From) &&
         Lhs.ExternalSource == Rhs.ExternalSource &&
         Lhs.HasExternalSourceNode == Rhs.HasExternalSourceNode &&
         Lhs.ExternalSourceNode == Rhs.ExternalSourceNode &&
         sameMember(Lhs.To, Rhs.To) &&
         Lhs.ExternalSuccessor == Rhs.ExternalSuccessor &&
         Lhs.HasExternalSuccessorNode == Rhs.HasExternalSuccessorNode &&
         Lhs.ExternalSuccessorNode == Rhs.ExternalSuccessorNode;
}

bool memberReferencesBlock(const OverlayMember &Member, BlockId Block) {
  return Member.Kind == OverlayMemberKind::Block && Member.Block == Block;
}

bool edgeReferencesBlock(const OverlayViewEdge &Edge, BlockId Block) {
  return (Edge.sourcesMember() && memberReferencesBlock(Edge.From, Block)) ||
         (!Edge.sourcesMember() && Edge.ExternalSource == Block) ||
         (Edge.HasExternalSourceNode && Edge.ExternalSourceNode.isBlock() &&
          Edge.ExternalSourceNode.Block == Block) ||
         (Edge.targetsMember() && memberReferencesBlock(Edge.To, Block)) ||
         (!Edge.targetsMember() && Edge.ExternalSuccessor == Block) ||
         (Edge.HasExternalSuccessorNode &&
          Edge.ExternalSuccessorNode.isBlock() &&
          Edge.ExternalSuccessorNode.Block == Block);
}

bool isBackEdgeByOrder(const OverlayNodeKey &From, const OverlayNodeKey &To,
                       const std::map<OverlayNodeKey, unsigned> &NodeOrder) {
  auto FromIt = NodeOrder.find(From);
  auto ToIt = NodeOrder.find(To);
  if (FromIt == NodeOrder.end() || ToIt == NodeOrder.end()) {
    return false;
  }
  return FromIt->second >= ToIt->second;
}

bool isBlacklistedEdge(const OverlayNodeKey &From, const OverlayNodeKey &To,
                       const std::vector<OverlayHiddenEdge> &BlacklistedEdges) {
  return std::find_if(BlacklistedEdges.begin(), BlacklistedEdges.end(),
                      [&](const OverlayHiddenEdge &Edge) {
                        return Edge.From == From && Edge.To == To;
                      }) != BlacklistedEdges.end();
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

void appendUniqueHiddenEdge(std::vector<OverlayHiddenEdge> &Edges,
                            OverlayHiddenEdge Edge) {
  auto It = std::find_if(Edges.begin(), Edges.end(),
                         [&](const OverlayHiddenEdge &Existing) {
                           return Existing.From == Edge.From &&
                                  Existing.To == Edge.To;
                         });
  if (It == Edges.end()) {
    Edges.push_back(Edge);
  }
}

} // namespace

OverlayNodeKey OverlayNodeKey::block(BlockId Id) {
  OverlayNodeKey Key;
  Key.Kind = OverlayNodeKind::Block;
  Key.Block = Id;
  return Key;
}

OverlayNodeKey OverlayNodeKey::region(RegionId Id) {
  OverlayNodeKey Key;
  Key.Kind = OverlayNodeKind::Region;
  Key.Region = Id;
  return Key;
}

OverlayNodeKey OverlayNodeKey::structured(NodeId Id, RegionId SourceRegion) {
  OverlayNodeKey Key;
  Key.Kind = OverlayNodeKind::Structured;
  Key.Region = SourceRegion;
  Key.StructuredRoot = Id;
  return Key;
}

bool OverlayNodeKey::operator<(const OverlayNodeKey &Other) const {
  return std::tie(Kind, Block, Region, StructuredRoot) <
         std::tie(Other.Kind, Other.Block, Other.Region,
                  Other.StructuredRoot);
}

bool OverlayNodeKey::operator==(const OverlayNodeKey &Other) const {
  return Kind == Other.Kind && Block == Other.Block &&
         Region == Other.Region && StructuredRoot == Other.StructuredRoot;
}

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

OverlayMember OverlayMember::structured(NodeId Id, RegionId SourceRegion,
                                        BlockId RepresentativeBlock) {
  OverlayMember Member;
  Member.Kind = OverlayMemberKind::Structured;
  Member.Region = SourceRegion;
  Member.StructuredRoot = Id;
  Member.Block = RepresentativeBlock;
  return Member;
}

OverlayNodeKey OverlayViewEdge::sourceNode() const {
  if (sourcesMember()) {
    return OverlayNodeKey{};
  }
  if (HasExternalSourceNode) {
    return ExternalSourceNode;
  }
  return OverlayNodeKey::block(ExternalSource);
}

OverlayNodeKey OverlayViewEdge::targetNode() const {
  if (targetsMember()) {
    return OverlayNodeKey{};
  }
  if (HasExternalSuccessorNode) {
    return ExternalSuccessorNode;
  }
  return OverlayNodeKey::block(ExternalSuccessor);
}

OverlayEdgeEndpoint OverlayEdgeEndpoint::member(OverlayMember Member) {
  OverlayEdgeEndpoint Endpoint;
  Endpoint.Member = Member;
  return Endpoint;
}

OverlayEdgeEndpoint OverlayEdgeEndpoint::external(BlockId Block) {
  OverlayEdgeEndpoint Endpoint;
  Endpoint.ExternalBlock = Block;
  return Endpoint;
}

OverlayEdgeEndpoint OverlayEdgeEndpoint::external(const OverlayNodeKey &Node) {
  OverlayEdgeEndpoint Endpoint;
  if (Node.isBlock()) {
    Endpoint.ExternalBlock = Node.Block;
  } else {
    Endpoint.HasExternalNode = true;
    Endpoint.ExternalNode = Node;
  }
  return Endpoint;
}

OverlayNodeKey OverlayEdgeEndpoint::node() const {
  if (HasExternalNode) {
    return ExternalNode;
  }
  return OverlayNodeKey::block(ExternalBlock);
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
  SharedNodeSuccessors.clear();
  SharedSuccessors.clear();
  for (const CFGBlock &Block : Cfg.blocks()) {
    OverlayNodeKey From = OverlayNodeKey::block(Block.Id);
    std::vector<OverlayNodeKey> &NodeSuccs = SharedNodeSuccessors[From];
    for (BlockId Succ : Block.Successors) {
      NodeSuccs.push_back(OverlayNodeKey::block(Succ));
    }
  }
  rebuildBlockSuccessorCompatibility();
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

OverlayNodeKey OverlayManager::nodeKey(const OverlayMember &Member) const {
  switch (Member.Kind) {
  case OverlayMemberKind::Block:
    return OverlayNodeKey::block(Member.Block);
  case OverlayMemberKind::Region:
    return OverlayNodeKey::region(Member.Region);
  case OverlayMemberKind::Structured:
    return OverlayNodeKey::structured(Member.StructuredRoot, Member.Region);
  }
  return OverlayNodeKey::block(InvalidBlockId);
}

BlockId OverlayManager::representativeBlock(const OverlayMember &Member) const {
  if (Member.Kind == OverlayMemberKind::Block) {
    return Member.Block;
  }
  if (Member.Kind == OverlayMemberKind::Structured &&
      Member.Block != InvalidBlockId) {
    return Member.Block;
  }
  const Region *MemberRegion = getRegionData(Member.Region);
  return MemberRegion == nullptr ? InvalidBlockId : MemberRegion->Head;
}

const std::vector<BlockId> &OverlayManager::sharedSuccessors(BlockId Id) const {
  static const std::vector<BlockId> Empty;
  auto It = SharedSuccessors.find(Id);
  return It == SharedSuccessors.end() ? Empty : It->second;
}

const std::vector<OverlayNodeKey> &
OverlayManager::sharedNodeSuccessors(const OverlayNodeKey &Id) const {
  static const std::vector<OverlayNodeKey> Empty;
  auto It = SharedNodeSuccessors.find(Id);
  return It == SharedNodeSuccessors.end() ? Empty : It->second;
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

const OverlayMember *
OverlayManager::memberForNodeKey(RegionId ViewId,
                                 const OverlayNodeKey &Key) const {
  if (Key.isBlock()) {
    return memberForBlock(ViewId, Key.Block);
  }

  const std::vector<OverlayMember> &ViewMembers = members(ViewId);
  for (const OverlayMember &Member : ViewMembers) {
    if (nodeKey(Member) == Key) {
      return &Member;
    }
  }
  return nullptr;
}

std::vector<BlockId>
OverlayManager::underlyingBlocks(const OverlayMember &Member) const {
  if (Member.Kind == OverlayMemberKind::Block) {
    return {Member.Block};
  }
  const Region *MemberRegion = getRegionData(Member.Region);
  return MemberRegion == nullptr ? std::vector<BlockId>{}
                                 : MemberRegion->Blocks;
}

std::optional<OverlayMember> OverlayManager::memberForEndpoint(
    RegionId ViewId, const OverlayEdgeEndpoint &Endpoint) const {
  if (Endpoint.isMember()) {
    return Endpoint.Member;
  }
  if (!Endpoint.node().isBlock()) {
    return std::nullopt;
  }
  const OverlayMember *Member = memberForBlock(ViewId, Endpoint.node().Block);
  if (Member == nullptr) {
    return std::nullopt;
  }
  return *Member;
}

std::optional<OverlayViewEdge> OverlayManager::viewEdgeForEndpoints(
    RegionId ViewId, const OverlayEdgeEndpoint &From,
    const OverlayEdgeEndpoint &To) const {
  OverlayViewEdge Edge;
  if (From.isMember()) {
    Edge.From = From.Member;
  } else {
    OverlayNodeKey FromNode = From.node();
    const OverlayMember *Member =
        FromNode.isBlock() ? memberForBlock(ViewId, FromNode.Block) : nullptr;
    if (Member != nullptr) {
      Edge.From = *Member;
    } else {
      if (FromNode.isBlock()) {
        Edge.ExternalSource = FromNode.Block;
      } else {
        Edge.HasExternalSourceNode = true;
        Edge.ExternalSourceNode = FromNode;
      }
    }
  }

  if (To.isMember()) {
    Edge.To = To.Member;
  } else {
    OverlayNodeKey ToNode = To.node();
    const OverlayMember *Member =
        ToNode.isBlock() ? memberForBlock(ViewId, ToNode.Block) : nullptr;
    if (Member != nullptr) {
      Edge.To = *Member;
    } else {
      if (ToNode.isBlock()) {
        Edge.ExternalSuccessor = ToNode.Block;
      } else {
        Edge.HasExternalSuccessorNode = true;
        Edge.ExternalSuccessorNode = ToNode;
      }
    }
  }
  return Edge;
}

bool OverlayManager::isHiddenEdge(RegionId Id, const OverlayNodeKey &From,
                                  const OverlayNodeKey &To) const {
  auto It = HiddenEdges.find(Id);
  if (It == HiddenEdges.end()) {
    return false;
  }
  return std::find_if(It->second.begin(), It->second.end(),
                      [&](const OverlayHiddenEdge &Edge) {
                        return Edge.From == From && Edge.To == To;
                      }) != It->second.end();
}

bool OverlayManager::isHiddenFullEdge(RegionId Id,
                                      const OverlayViewEdge &Edge) const {
  auto It = HiddenFullEdges.find(Id);
  if (It == HiddenFullEdges.end()) {
    return false;
  }
  return std::find_if(It->second.begin(), It->second.end(),
                      [&](const OverlayViewEdge &Existing) {
                        return sameEdge(Existing, Edge);
                      }) != It->second.end();
}

bool OverlayManager::isMarkedEdge(RegionId Id, const OverlayNodeKey &From,
                                  const OverlayNodeKey &To) const {
  auto It = EdgeMarks.find(Id);
  if (It == EdgeMarks.end()) {
    return false;
  }
  for (const auto &Entry : It->second) {
    const std::vector<OverlayHiddenEdge> &Edges = Entry.second;
    if (std::find_if(Edges.begin(), Edges.end(),
                     [&](const OverlayHiddenEdge &Edge) {
                       return Edge.From == From && Edge.To == To;
                     }) != Edges.end()) {
      return true;
    }
  }
  return false;
}

bool OverlayManager::isMarkedViewEdge(RegionId Id,
                                      const OverlayViewEdge &Edge) const {
  OverlayNodeKey From =
      Edge.sourcesMember() ? nodeKey(Edge.From) : Edge.sourceNode();
  OverlayNodeKey To =
      Edge.targetsMember() ? nodeKey(Edge.To) : Edge.targetNode();
  return isMarkedEdge(Id, From, To);
}

void OverlayManager::rebuildBlockSuccessorCompatibility() {
  SharedSuccessors.clear();
  for (const auto &Entry : SharedNodeSuccessors) {
    if (!Entry.first.isBlock()) {
      continue;
    }
    std::vector<BlockId> &Succs = SharedSuccessors[Entry.first.Block];
    for (const OverlayNodeKey &Succ : Entry.second) {
      if (Succ.isBlock()) {
        appendUniqueBlock(Succs, Succ.Block);
      }
    }
  }
}

void OverlayManager::clearHiddenEdge(const OverlayNodeKey &From,
                                     const OverlayNodeKey &To) {
  for (auto &Entry : HiddenEdges) {
    std::vector<OverlayHiddenEdge> &Edges = Entry.second;
    Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                               [&](const OverlayHiddenEdge &Edge) {
                                 return Edge.From == From && Edge.To == To;
                               }),
                Edges.end());
  }
}

void OverlayManager::clearEdgeStateForBlock(BlockId Block) {
  SharedNodeSuccessors.erase(OverlayNodeKey::block(Block));
  for (auto &Entry : SharedNodeSuccessors) {
    std::vector<OverlayNodeKey> &Succs = Entry.second;
    Succs.erase(std::remove(Succs.begin(), Succs.end(),
                            OverlayNodeKey::block(Block)),
                Succs.end());
  }
  SharedSuccessors.erase(Block);
  for (auto &Entry : SharedSuccessors) {
    std::vector<BlockId> &Succs = Entry.second;
    Succs.erase(std::remove(Succs.begin(), Succs.end(), Block), Succs.end());
  }

  for (auto &Entry : HiddenEdges) {
    std::vector<OverlayHiddenEdge> &Edges = Entry.second;
    Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                               [&](const OverlayHiddenEdge &Edge) {
                                 return (Edge.From.isBlock() &&
                                         Edge.From.Block == Block) ||
                                        (Edge.To.isBlock() &&
                                         Edge.To.Block == Block);
                               }),
                Edges.end());
  }
  for (auto &Entry : HiddenFullEdges) {
    std::vector<OverlayViewEdge> &Edges = Entry.second;
    Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                               [&](const OverlayViewEdge &Edge) {
                                 return edgeReferencesBlock(Edge, Block);
                               }),
                Edges.end());
  }
  for (auto &Entry : ExtraFullEdges) {
    std::vector<OverlayViewEdge> &Edges = Entry.second;
    Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                               [&](const OverlayViewEdge &Edge) {
                                 return edgeReferencesBlock(Edge, Block);
                               }),
                Edges.end());
  }
  for (auto &Entry : EdgeMarks) {
    for (auto &MarkEntry : Entry.second) {
      std::vector<OverlayHiddenEdge> &Edges = MarkEntry.second;
      Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                                 [&](const OverlayHiddenEdge &Edge) {
                                   return (Edge.From.isBlock() &&
                                           Edge.From.Block == Block) ||
                                          (Edge.To.isBlock() &&
                                           Edge.To.Block == Block);
                                 }),
                  Edges.end());
    }
  }
}

std::vector<OverlayNodeKey>
OverlayManager::memberNodeKeys(const OverlayMember &Member) const {
  if (Member.Kind == OverlayMemberKind::Structured) {
    return {nodeKey(Member)};
  }
  if (Member.Kind == OverlayMemberKind::Block) {
    return {OverlayNodeKey::block(Member.Block)};
  }

  std::vector<OverlayNodeKey> Result;
  const Region *MemberRegion = getRegionData(Member.Region);
  if (MemberRegion == nullptr) {
    return Result;
  }
  for (BlockId Block : MemberRegion->Blocks) {
    appendUniqueNodeKey(Result, OverlayNodeKey::block(Block));
  }
  return Result;
}

void OverlayManager::remapViewEdgeEndpoint(
    RegionId ViewId, OverlayViewEdge &Edge, bool Source,
    const std::vector<OverlayNodeKey> &OldNodes,
    const OverlayNodeKey &NewNode) const {
  auto IsOldNode = [&](const OverlayNodeKey &Key) {
    return std::find(OldNodes.begin(), OldNodes.end(), Key) != OldNodes.end();
  };

  bool ShouldRemap = false;
  if (Source) {
    if (Edge.sourcesMember()) {
      ShouldRemap = IsOldNode(nodeKey(Edge.From));
    } else {
      ShouldRemap = IsOldNode(Edge.sourceNode());
    }
  } else {
    if (Edge.targetsMember()) {
      ShouldRemap = IsOldNode(nodeKey(Edge.To));
    } else {
      ShouldRemap = IsOldNode(Edge.targetNode());
    }
  }
  if (!ShouldRemap) {
    return;
  }

  const OverlayMember *Member = memberForNodeKey(ViewId, NewNode);
  if (Source) {
    Edge.From = Member == nullptr ? OverlayMember{} : *Member;
    Edge.ExternalSource = InvalidBlockId;
    Edge.HasExternalSourceNode = false;
    Edge.ExternalSourceNode = {};
    if (Member == nullptr) {
      if (NewNode.isBlock()) {
        Edge.ExternalSource = NewNode.Block;
      } else {
        Edge.HasExternalSourceNode = true;
        Edge.ExternalSourceNode = NewNode;
      }
    }
  } else {
    Edge.To = Member == nullptr ? OverlayMember{} : *Member;
    Edge.ExternalSuccessor = InvalidBlockId;
    Edge.HasExternalSuccessorNode = false;
    Edge.ExternalSuccessorNode = {};
    if (Member == nullptr) {
      if (NewNode.isBlock()) {
        Edge.ExternalSuccessor = NewNode.Block;
      } else {
        Edge.HasExternalSuccessorNode = true;
        Edge.ExternalSuccessorNode = NewNode;
      }
    }
  }
}

void OverlayManager::remapBookkeeping(
    RegionId Id, const std::vector<OverlayNodeKey> &OldNodes,
    const OverlayNodeKey &NewNode) {
  auto IsOldNode = [&](const OverlayNodeKey &Key) {
    return std::find(OldNodes.begin(), OldNodes.end(), Key) != OldNodes.end();
  };

  for (auto &Entry : HiddenEdges) {
    for (OverlayHiddenEdge &Edge : Entry.second) {
      if (IsOldNode(Edge.From)) {
        Edge.From = NewNode;
      }
      if (IsOldNode(Edge.To)) {
        Edge.To = NewNode;
      }
    }
  }
  for (auto &Entry : HiddenFullEdges) {
    for (OverlayViewEdge &Edge : Entry.second) {
      remapViewEdgeEndpoint(Entry.first, Edge, /*Source=*/true, OldNodes,
                            NewNode);
      remapViewEdgeEndpoint(Entry.first, Edge, /*Source=*/false, OldNodes,
                            NewNode);
    }
  }
  for (auto &Entry : ExtraFullEdges) {
    for (OverlayViewEdge &Edge : Entry.second) {
      remapViewEdgeEndpoint(Entry.first, Edge, /*Source=*/true, OldNodes,
                            NewNode);
      remapViewEdgeEndpoint(Entry.first, Edge, /*Source=*/false, OldNodes,
                            NewNode);
    }
  }
  for (auto &Entry : EdgeMarks) {
    for (auto &MarkEntry : Entry.second) {
      for (OverlayHiddenEdge &Edge : MarkEntry.second) {
        if (IsOldNode(Edge.From)) {
          Edge.From = NewNode;
        }
        if (IsOldNode(Edge.To)) {
          Edge.To = NewNode;
        }
      }
    }
  }
  (void)Id;
}

std::vector<OverlayNodeKey>
OverlayManager::visibleNodeSuccessors(RegionId Id,
                                      bool IncludeMarkedEdges) const {
  std::vector<OverlayNodeKey> Result;
  const std::vector<OverlayMember> &ViewMembers = members(Id);
  for (const OverlayMember &Member : ViewMembers) {
    if (Member.Kind != OverlayMemberKind::Region) {
      OverlayNodeKey From = nodeKey(Member);
      for (const OverlayNodeKey &Succ : sharedNodeSuccessors(From)) {
        if (isHiddenEdge(Id, From, Succ)) {
          continue;
        }
        if (!IncludeMarkedEdges && isMarkedEdge(Id, From, Succ)) {
          continue;
        }
        if (memberForNodeKey(Id, Succ) != nullptr) {
          continue;
        }
        appendUniqueNodeKey(Result, Succ);
      }
      continue;
    }

    std::vector<BlockId> Blocks;
    const Region *MemberRegion = getRegionData(Member.Region);
    if (MemberRegion != nullptr) {
      Blocks = MemberRegion->Blocks;
    }

    for (BlockId Block : Blocks) {
      OverlayNodeKey From = OverlayNodeKey::block(Block);
      for (const OverlayNodeKey &Succ : sharedNodeSuccessors(From)) {
        if (isHiddenEdge(Id, OverlayNodeKey::block(Block),
                         Succ)) {
          continue;
        }
        if (!IncludeMarkedEdges &&
            isMarkedEdge(Id, OverlayNodeKey::block(Block), Succ)) {
          continue;
        }
        if (memberForNodeKey(Id, Succ) != nullptr) {
          continue;
        }
        appendUniqueNodeKey(Result, Succ);
      }
    }
  }
  return Result;
}

std::vector<BlockId>
OverlayManager::visibleSuccessors(RegionId Id, bool IncludeMarkedEdges) const {
  std::vector<BlockId> Result;
  for (const OverlayNodeKey &Succ :
       visibleNodeSuccessors(Id, IncludeMarkedEdges)) {
    if (Succ.isBlock()) {
      appendUniqueBlock(Result, Succ.Block);
    }
  }
  return Result;
}

std::vector<OverlayViewEdge>
OverlayManager::quotientEdges(RegionId Id, bool IncludeSuccessors,
                              bool IncludeMarkedEdges) const {
  std::vector<OverlayViewEdge> Result;
  const std::vector<OverlayMember> &ViewMembers = members(Id);
  auto appendSuccessorEdge = [&](const OverlayMember &Member,
                                 const OverlayNodeKey &Succ) {
    if (Succ.isBlock()) {
      const OverlayMember *SuccMember = memberForBlock(Id, Succ.Block);
      if (SuccMember != nullptr) {
        if (sameMember(Member, *SuccMember) &&
            Member.Kind != OverlayMemberKind::Block) {
          return;
        }
        OverlayViewEdge Edge = {Member, InvalidBlockId, *SuccMember,
                                InvalidBlockId};
        if ((!IncludeSuccessors || !isHiddenFullEdge(Id, Edge)) &&
            (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
          appendUniqueEdge(Result, Edge);
        }
        return;
      }
      if (IncludeSuccessors) {
        OverlayViewEdge Edge = {Member, InvalidBlockId, {}, Succ.Block};
        if (!isHiddenFullEdge(Id, Edge) &&
            (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
          appendUniqueEdge(Result, Edge);
        }
      }
      return;
    }

    auto It = std::find_if(ViewMembers.begin(), ViewMembers.end(),
                           [&](const OverlayMember &Candidate) {
                             return nodeKey(Candidate) == Succ;
                           });
    if (It != ViewMembers.end()) {
      OverlayViewEdge Edge = {Member, InvalidBlockId, *It, InvalidBlockId};
      if ((!IncludeSuccessors || !isHiddenFullEdge(Id, Edge)) &&
          (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
        appendUniqueEdge(Result, Edge);
      }
      return;
    }

    if (IncludeSuccessors) {
      OverlayViewEdge Edge = {Member, InvalidBlockId, {}, InvalidBlockId};
      Edge.HasExternalSuccessorNode = true;
      Edge.ExternalSuccessorNode = Succ;
      if (!isHiddenFullEdge(Id, Edge) &&
          (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
        appendUniqueEdge(Result, Edge);
      }
    }
  };

  for (const OverlayMember &Member : ViewMembers) {
    if (Member.Kind != OverlayMemberKind::Region) {
      for (const OverlayNodeKey &Succ : sharedNodeSuccessors(nodeKey(Member))) {
        if (isHiddenEdge(Id, nodeKey(Member), Succ)) {
          continue;
        }
        appendSuccessorEdge(Member, Succ);
      }
      continue;
    }

    std::vector<BlockId> Blocks;
    const Region *MemberRegion = getRegionData(Member.Region);
    if (MemberRegion != nullptr) {
      Blocks = MemberRegion->Blocks;
    }

    for (BlockId Block : Blocks) {
      for (BlockId Succ : sharedSuccessors(Block)) {
        if (isHiddenEdge(Id, OverlayNodeKey::block(Block),
                         OverlayNodeKey::block(Succ))) {
          continue;
        }
        const OverlayMember *SuccMember = memberForBlock(Id, Succ);
        if (SuccMember != nullptr) {
          if (sameMember(Member, *SuccMember) &&
              Member.Kind != OverlayMemberKind::Block) {
            continue;
          }
          OverlayViewEdge Edge = {Member, InvalidBlockId, *SuccMember,
                                  InvalidBlockId};
          if ((!IncludeSuccessors || !isHiddenFullEdge(Id, Edge)) &&
              (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
            appendUniqueEdge(Result, Edge);
          }
          continue;
        }
        if (IncludeSuccessors) {
          OverlayViewEdge Edge = {Member, InvalidBlockId, {}, Succ};
          if (!isHiddenFullEdge(Id, Edge) &&
              (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
            appendUniqueEdge(Result, Edge);
          }
        }
      }
    }
  }

  const Region *R = getRegionData(Id);
  bool InLoop = R != nullptr && R->Kind == RegionKind::NaturalLoop;
  for (RegionId ParentId = parentOf(Id); ParentId != InvalidRegionId;
       ParentId = parentOf(ParentId)) {
    const Region *Parent = getRegionData(ParentId);
    if (Parent != nullptr && Parent->Kind == RegionKind::NaturalLoop) {
      InLoop = true;
      break;
    }
  }
  if (!IncludeSuccessors) {
    return Result;
  }

  if (InLoop) {
    std::vector<BlockId> Succs = visibleSuccessors(Id, IncludeMarkedEdges);
    for (BlockId Succ : Succs) {
      for (BlockId SuccSucc : sharedSuccessors(Succ)) {
        if (std::find(Succs.begin(), Succs.end(), SuccSucc) == Succs.end() ||
            Succ == SuccSucc) {
          continue;
        }
        OverlayViewEdge Edge = {{}, Succ, {}, SuccSucc};
        if (!isHiddenFullEdge(Id, Edge) &&
            (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
          appendUniqueEdge(Result, Edge);
        }
      }
    }
  }
  auto ExtraIt = ExtraFullEdges.find(Id);
  if (ExtraIt != ExtraFullEdges.end()) {
    for (const OverlayViewEdge &Edge : ExtraIt->second) {
      if (!isHiddenFullEdge(Id, Edge) &&
          (IncludeMarkedEdges || !isMarkedViewEdge(Id, Edge))) {
        appendUniqueEdge(Result, Edge);
      }
    }
  }
  return Result;
}

std::vector<OverlayNodeKey> OverlayManager::visibleNodeSuccessorsBlacklisted(
    RegionId Id, const std::vector<OverlayHiddenEdge> &BlacklistedEdges,
    bool IncludeMarkedEdges) const {
  std::vector<OverlayNodeKey> Result;
  for (const OverlayViewEdge &Edge :
       quotientEdgesBlacklisted(Id, /*IncludeSuccessors=*/true,
                                BlacklistedEdges, IncludeMarkedEdges)) {
    if (Edge.sourcesMember() && !Edge.targetsMember()) {
      appendUniqueNodeKey(Result, Edge.targetNode());
    }
  }
  return Result;
}

std::vector<BlockId> OverlayManager::visibleSuccessorsBlacklisted(
    RegionId Id, const std::vector<OverlayHiddenEdge> &BlacklistedEdges,
    bool IncludeMarkedEdges) const {
  std::vector<BlockId> Result;
  for (const OverlayNodeKey &Succ : visibleNodeSuccessorsBlacklisted(
           Id, BlacklistedEdges, IncludeMarkedEdges)) {
    if (Succ.isBlock()) {
      appendUniqueBlock(Result, Succ.Block);
    }
  }
  return Result;
}

std::vector<OverlayViewEdge> OverlayManager::quotientEdgesBlacklisted(
    RegionId Id, bool IncludeSuccessors,
    const std::vector<OverlayHiddenEdge> &BlacklistedEdges,
    bool IncludeMarkedEdges) const {
  std::vector<OverlayViewEdge> Result =
      quotientEdges(Id, IncludeSuccessors, IncludeMarkedEdges);
  Result.erase(std::remove_if(Result.begin(), Result.end(),
                              [&](const OverlayViewEdge &Edge) {
                                OverlayNodeKey From = Edge.sourcesMember()
                                                          ? nodeKey(Edge.From)
                                                          : Edge.sourceNode();
                                OverlayNodeKey To = Edge.targetsMember()
                                                        ? nodeKey(Edge.To)
                                                        : Edge.targetNode();
                                if (From == OverlayNodeKey{} ||
                                    To == OverlayNodeKey{}) {
                                  return false;
                                }
                                return isBlacklistedEdge(From, To,
                                                         BlacklistedEdges);
                              }),
               Result.end());
  return Result;
}

std::vector<OverlayNodeKey> OverlayManager::visibleNodeSuccessorsAcyclic(
    RegionId Id, const std::map<OverlayNodeKey, unsigned> &NodeOrder,
    bool IncludeMarkedEdges) const {
  std::vector<OverlayNodeKey> Result;
  for (const OverlayViewEdge &Edge :
       quotientEdgesAcyclic(Id, /*IncludeSuccessors=*/true, NodeOrder,
                            IncludeMarkedEdges)) {
    if (Edge.sourcesMember() && !Edge.targetsMember()) {
      appendUniqueNodeKey(Result, Edge.targetNode());
    }
  }
  return Result;
}

std::vector<BlockId> OverlayManager::visibleSuccessorsAcyclic(
    RegionId Id, const std::map<OverlayNodeKey, unsigned> &NodeOrder,
    bool IncludeMarkedEdges) const {
  std::vector<BlockId> Result;
  for (const OverlayNodeKey &Succ :
       visibleNodeSuccessorsAcyclic(Id, NodeOrder, IncludeMarkedEdges)) {
    if (Succ.isBlock()) {
      appendUniqueBlock(Result, Succ.Block);
    }
  }
  return Result;
}

std::vector<OverlayViewEdge> OverlayManager::quotientEdgesAcyclic(
    RegionId Id, bool IncludeSuccessors,
    const std::map<OverlayNodeKey, unsigned> &NodeOrder,
    bool IncludeMarkedEdges) const {
  std::vector<OverlayHiddenEdge> BlacklistedEdges;
  for (const OverlayViewEdge &Edge :
       quotientEdges(Id, IncludeSuccessors, IncludeMarkedEdges)) {
    OverlayNodeKey From =
        Edge.sourcesMember() ? nodeKey(Edge.From) : Edge.sourceNode();
    OverlayNodeKey To =
        Edge.targetsMember() ? nodeKey(Edge.To) : Edge.targetNode();
    if (From == OverlayNodeKey{} || To == OverlayNodeKey{}) {
      continue;
    }
    if (isBackEdgeByOrder(From, To, NodeOrder)) {
      appendUniqueHiddenEdge(BlacklistedEdges, {From, To});
    }
  }
  return quotientEdgesBlacklisted(Id, IncludeSuccessors, BlacklistedEdges,
                                  IncludeMarkedEdges);
}

void OverlayManager::addBlockMember(RegionId Id, BlockId Block) {
  std::vector<OverlayMember> &ViewMembers = Members[Id];
  auto It = std::find_if(ViewMembers.begin(), ViewMembers.end(),
                         [&](const OverlayMember &Member) {
                           return memberReferencesBlock(Member, Block);
                         });
  if (It == ViewMembers.end()) {
    ViewMembers.push_back(OverlayMember::block(Block));
  }
  BlockOwners[Block] = Id;
  SharedNodeSuccessors[OverlayNodeKey::block(Block)];
  SharedSuccessors[Block];
}

void OverlayManager::removeBlockMember(BlockId Block) {
  RegionId Owner = ownerOf(Block);
  if (Owner != InvalidRegionId) {
    std::vector<OverlayMember> &ViewMembers = Members[Owner];
    ViewMembers.erase(std::remove_if(ViewMembers.begin(), ViewMembers.end(),
                                     [&](const OverlayMember &Member) {
                                       return memberReferencesBlock(Member,
                                                                    Block);
                                     }),
                      ViewMembers.end());
  }
  BlockOwners.erase(Block);
  clearEdgeStateForBlock(Block);
}

void OverlayManager::addNodeEdge(const OverlayNodeKey &From,
                                 const OverlayNodeKey &To) {
  std::vector<OverlayNodeKey> &Succs = SharedNodeSuccessors[From];
  if (std::find(Succs.begin(), Succs.end(), To) == Succs.end()) {
    Succs.push_back(To);
  }
  rebuildBlockSuccessorCompatibility();
  clearHiddenEdge(From, To);
}

void OverlayManager::detachNodeEdge(const OverlayNodeKey &From,
                                    const OverlayNodeKey &To) {
  auto It = SharedNodeSuccessors.find(From);
  if (It == SharedNodeSuccessors.end()) {
    return;
  }
  std::vector<OverlayNodeKey> &Succs = It->second;
  Succs.erase(std::remove(Succs.begin(), Succs.end(), To), Succs.end());
  rebuildBlockSuccessorCompatibility();
  clearHiddenEdge(From, To);
}

void OverlayManager::addEdge(BlockId From, BlockId To) {
  addNodeEdge(OverlayNodeKey::block(From), OverlayNodeKey::block(To));
}

void OverlayManager::detachEdge(BlockId From, BlockId To) {
  detachNodeEdge(OverlayNodeKey::block(From), OverlayNodeKey::block(To));
}

void OverlayManager::hideNodeEdge(RegionId Id, const OverlayNodeKey &From,
                                  const OverlayNodeKey &To) {
  appendUniqueHiddenEdge(HiddenEdges[Id], {From, To});
}

void OverlayManager::hideEdge(RegionId Id, BlockId From, BlockId To) {
  hideNodeEdge(Id, OverlayNodeKey::block(From), OverlayNodeKey::block(To));
}

void OverlayManager::hideEdgeToSuccessor(RegionId Id, BlockId Successor) {
  hideEdgeToNodeSuccessor(Id, OverlayNodeKey::block(Successor));
}

void OverlayManager::hideEdgeToNodeSuccessor(
    RegionId Id, const OverlayNodeKey &Successor) {
  const std::vector<OverlayMember> &ViewMembers = members(Id);
  for (const OverlayMember &Member : ViewMembers) {
    for (const OverlayNodeKey &From : memberNodeKeys(Member)) {
      for (const OverlayNodeKey &Succ : sharedNodeSuccessors(From)) {
        if (Succ == Successor) {
          hideNodeEdge(Id, From, Succ);
        }
      }
    }
  }
}

void OverlayManager::removeEdgeWithSuccessorsOnly(
    RegionId Id, const OverlayEdgeEndpoint &From,
    const OverlayEdgeEndpoint &To) {
  std::optional<OverlayViewEdge> Edge = viewEdgeForEndpoints(Id, From, To);
  if (!Edge) {
    return;
  }
  appendUniqueEdge(HiddenFullEdges[Id], *Edge);
}

void OverlayManager::addExtraFullEdge(RegionId Id,
                                      const OverlayEdgeEndpoint &From,
                                      const OverlayEdgeEndpoint &To) {
  std::optional<OverlayViewEdge> Edge = viewEdgeForEndpoints(Id, From, To);
  if (!Edge) {
    return;
  }
  appendUniqueEdge(ExtraFullEdges[Id], *Edge);
}

void OverlayManager::markNodeEdge(RegionId Id, const OverlayNodeKey &From,
                                  const OverlayNodeKey &To,
                                  const std::string &Key) {
  appendUniqueHiddenEdge(EdgeMarks[Id][Key], {From, To});
}

void OverlayManager::markEdge(RegionId Id, const OverlayEdgeEndpoint &From,
                              const OverlayEdgeEndpoint &To,
                              const std::string &Key) {
  std::optional<OverlayViewEdge> Edge = viewEdgeForEndpoints(Id, From, To);
  if (!Edge) {
    return;
  }
  markNodeEdge(Id, Edge->sourcesMember() ? nodeKey(Edge->From)
                                         : Edge->sourceNode(),
               Edge->targetsMember() ? nodeKey(Edge->To) : Edge->targetNode(),
               Key);
}

void OverlayManager::dropEdgeMarksFrom(RegionId Id, const OverlayNodeKey &From,
                                       const std::string &Key) {
  auto It = EdgeMarks.find(Id);
  if (It == EdgeMarks.end()) {
    return;
  }
  auto MarkIt = It->second.find(Key);
  if (MarkIt == It->second.end()) {
    return;
  }
  std::vector<OverlayHiddenEdge> &Edges = MarkIt->second;
  Edges.erase(std::remove_if(Edges.begin(), Edges.end(),
                             [&](const OverlayHiddenEdge &Edge) {
                               return Edge.From == From;
                             }),
              Edges.end());
}

void OverlayManager::absorbSuccessorInto(
    RegionId Id, const OverlayEdgeEndpoint &Successor,
    const OverlayEdgeEndpoint &NewNode) {
  auto EndpointNode = [&](const OverlayEdgeEndpoint &Endpoint) {
    return Endpoint.isMember() ? nodeKey(Endpoint.Member) : Endpoint.node();
  };
  auto EdgeSourceNode = [&](const OverlayViewEdge &Edge) {
    return Edge.sourcesMember() ? nodeKey(Edge.From) : Edge.sourceNode();
  };
  auto EdgeTargetNode = [&](const OverlayViewEdge &Edge) {
    return Edge.targetsMember() ? nodeKey(Edge.To) : Edge.targetNode();
  };
  auto EdgeTargetEndpoint = [&](const OverlayViewEdge &Edge) {
    return Edge.targetsMember() ? OverlayEdgeEndpoint::member(Edge.To)
                                : OverlayEdgeEndpoint::external(
                                      EdgeTargetNode(Edge));
  };

  OverlayNodeKey SuccessorNode = EndpointNode(Successor);
  OverlayNodeKey NewNodeKey = EndpointNode(NewNode);
  for (const OverlayViewEdge &Edge :
       quotientEdges(Id, /*IncludeSuccessors=*/true)) {
    if (!(EdgeSourceNode(Edge) == SuccessorNode)) {
      continue;
    }
    if (EdgeTargetNode(Edge) == NewNodeKey) {
      continue;
    }
    addExtraFullEdge(Id, NewNode, EdgeTargetEndpoint(Edge));
  }
  hideEdgeToNodeSuccessor(Id, SuccessorNode);
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
       ParentRegions, SharedNodeSuccessors, SharedSuccessors, HiddenEdges,
       HiddenFullEdges, ExtraFullEdges, EdgeMarks});
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
  SharedNodeSuccessors =
      StructuredRootCheckpoints[Checkpoint].SharedNodeSuccessors;
  SharedSuccessors = StructuredRootCheckpoints[Checkpoint].SharedSuccessors;
  HiddenEdges = StructuredRootCheckpoints[Checkpoint].HiddenEdges;
  HiddenFullEdges = StructuredRootCheckpoints[Checkpoint].HiddenFullEdges;
  ExtraFullEdges = StructuredRootCheckpoints[Checkpoint].ExtraFullEdges;
  EdgeMarks = StructuredRootCheckpoints[Checkpoint].EdgeMarks;
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
  SuccessorSnapshot StoredSnapshot = Snapshot;
  if (StoredSnapshot.NodeSuccessors.empty()) {
    for (BlockId Succ : StoredSnapshot.Successors) {
      appendUniqueNodeKey(StoredSnapshot.NodeSuccessors,
                          OverlayNodeKey::block(Succ));
    }
  }
  if (StoredSnapshot.Successors.empty()) {
    for (const OverlayNodeKey &Succ : StoredSnapshot.NodeSuccessors) {
      if (Succ.isBlock()) {
        appendUniqueBlock(StoredSnapshot.Successors, Succ.Block);
      }
    }
  }
  SuccessorSnapshots[Id] = std::move(StoredSnapshot);
  finalizeRegionMembers(Id, RootId);

  OverlayNodeKey ResultNode = OverlayNodeKey::structured(RootId, Id);
  detachNodeEdge(ResultNode, ResultNode);

  RegionId ParentId = parentOf(Id);
  const Region *Parent = getRegionData(ParentId);
  BlockId ParentLoopHead =
      Parent != nullptr && Parent->Kind == RegionKind::NaturalLoop
          ? Parent->Head
          : InvalidBlockId;
  const SuccessorSnapshot &FinalSnapshot = SuccessorSnapshots[Id];
  for (const OverlayNodeKey &Succ : FinalSnapshot.NodeSuccessors) {
    if (Succ == ResultNode) {
      continue;
    }
    if (Succ.isBlock() && Succ.Block == ParentLoopHead) {
      continue;
    }
    addNodeEdge(ResultNode, Succ);
  }
}

void OverlayManager::clearStructuredRoot(RegionId Id) {
  StructuredRoots.erase(Id);
  SuccessorSnapshots.erase(Id);
}

void OverlayManager::collapseRegionTo(RegionId Id, NodeId RootId) {
  if (RootId == InvalidNodeId) {
    return;
  }
  RegionId ParentId = parentOf(Id);
  if (ParentId == InvalidRegionId) {
    return;
  }

  std::vector<OverlayNodeKey> OldNodes;
  for (const OverlayMember &Member : members(Id)) {
    for (const OverlayNodeKey &Key : memberNodeKeys(Member)) {
      appendUniqueNodeKey(OldNodes, Key);
    }
  }

  OverlayNodeKey ResultNode = OverlayNodeKey::structured(RootId, Id);
  std::vector<OverlayNodeKey> InSources;
  std::vector<OverlayNodeKey> OutTargets;
  auto IsOldNode = [&](const OverlayNodeKey &Key) {
    return std::find(OldNodes.begin(), OldNodes.end(), Key) != OldNodes.end();
  };

  for (const auto &Entry : SharedNodeSuccessors) {
    const OverlayNodeKey &From = Entry.first;
    bool FromInside = IsOldNode(From);
    for (const OverlayNodeKey &To : Entry.second) {
      bool ToInside = IsOldNode(To);
      if (FromInside && !ToInside) {
        appendUniqueNodeKey(OutTargets, To);
      } else if (!FromInside && ToInside) {
        appendUniqueNodeKey(InSources, From);
      }
    }
  }

  for (const OverlayNodeKey &Old : OldNodes) {
    SharedNodeSuccessors.erase(Old);
  }
  for (auto &Entry : SharedNodeSuccessors) {
    std::vector<OverlayNodeKey> &Succs = Entry.second;
    Succs.erase(std::remove_if(Succs.begin(), Succs.end(),
                               [&](const OverlayNodeKey &Succ) {
                                 return IsOldNode(Succ);
                               }),
                Succs.end());
  }
  SharedNodeSuccessors[ResultNode];
  rebuildBlockSuccessorCompatibility();

  for (const OverlayNodeKey &From : InSources) {
    if (!(From == ResultNode)) {
      addNodeEdge(From, ResultNode);
    }
  }
  for (const OverlayNodeKey &To : OutTargets) {
    if (!(To == ResultNode)) {
      addNodeEdge(ResultNode, To);
    }
  }

  StructuredRoots[Id] = RootId;
  SuccessorSnapshots.erase(Id);
  Members[Id].clear();
  finalizeRegionMembers(Id, RootId);
  for (const OverlayNodeKey &Old : OldNodes) {
    if (Old.isBlock()) {
      BlockOwners.erase(Old.Block);
    }
  }
}

void OverlayManager::replaceNodes(RegionId Id,
                                  const std::vector<OverlayNodeKey> &OldNodes,
                                  NodeId RootId, bool SelfLoop) {
  if (OldNodes.empty() || RootId == InvalidNodeId) {
    return;
  }

  BlockId Representative = InvalidBlockId;
  std::vector<OverlayMember> &ViewMembers = Members[Id];
  for (const OverlayMember &Member : ViewMembers) {
    if (std::find(OldNodes.begin(), OldNodes.end(), nodeKey(Member)) !=
        OldNodes.end()) {
      Representative = representativeBlock(Member);
      break;
    }
  }
  if (Representative == InvalidBlockId && OldNodes.front().isBlock()) {
    Representative = OldNodes.front().Block;
  }

  auto IsOldNode = [&](const OverlayNodeKey &Key) {
    return std::find(OldNodes.begin(), OldNodes.end(), Key) != OldNodes.end();
  };
  OverlayNodeKey NewNode = OverlayNodeKey::structured(RootId, Id);
  std::vector<OverlayNodeKey> InSources;
  std::vector<OverlayNodeKey> OutTargets;
  bool AddSelfLoop = false;

  for (const auto &Entry : SharedNodeSuccessors) {
    const OverlayNodeKey &From = Entry.first;
    bool FromInside = IsOldNode(From);
    for (const OverlayNodeKey &To : Entry.second) {
      bool ToInside = IsOldNode(To);
      if (!FromInside && ToInside) {
        appendUniqueNodeKey(InSources, From);
      } else if (FromInside && !ToInside) {
        appendUniqueNodeKey(OutTargets, To);
      } else if (FromInside && ToInside && SelfLoop) {
        if ((From == To) ||
            (OldNodes.size() == 2 && From == OldNodes[1] &&
             To == OldNodes[0])) {
          AddSelfLoop = true;
        }
      }
    }
  }

  for (const OverlayNodeKey &Old : OldNodes) {
    SharedNodeSuccessors.erase(Old);
  }
  for (auto &Entry : SharedNodeSuccessors) {
    std::vector<OverlayNodeKey> &Succs = Entry.second;
    Succs.erase(std::remove_if(Succs.begin(), Succs.end(),
                               [&](const OverlayNodeKey &Succ) {
                                 return IsOldNode(Succ);
                               }),
                Succs.end());
  }
  SharedNodeSuccessors[NewNode];
  rebuildBlockSuccessorCompatibility();

  for (const OverlayNodeKey &From : InSources) {
    addNodeEdge(From, NewNode);
  }
  for (const OverlayNodeKey &To : OutTargets) {
    addNodeEdge(NewNode, To);
  }
  if (AddSelfLoop) {
    addNodeEdge(NewNode, NewNode);
  }

  ViewMembers.erase(std::remove_if(ViewMembers.begin(), ViewMembers.end(),
                                   [&](const OverlayMember &Member) {
                                     return IsOldNode(nodeKey(Member));
                                   }),
                    ViewMembers.end());
  ViewMembers.push_back(OverlayMember::structured(RootId, Id, Representative));
  for (const OverlayNodeKey &Old : OldNodes) {
    if (Old.isBlock()) {
      BlockOwners.erase(Old.Block);
    }
  }
  remapBookkeeping(Id, OldNodes, NewNode);
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
    Snapshot.NodeSuccessors = Manager->visibleNodeSuccessors(Id);
    Snapshot.Successors = Manager->visibleSuccessors(Id);
  }
  if (Snapshot.NodeSuccessors.empty()) {
    for (BlockId Succ : successors()) {
      appendUniqueNodeKey(Snapshot.NodeSuccessors, OverlayNodeKey::block(Succ));
    }
  }
  if (Snapshot.Successors.empty()) {
    Snapshot.Successors = successors();
  }
  return Snapshot;
}

void RegionOverlay::addBlockMember(BlockId Block) {
  if (Manager != nullptr) {
    Manager->addBlockMember(Id, Block);
  }
}

void RegionOverlay::removeBlockMember(BlockId Block) {
  if (Manager != nullptr) {
    Manager->removeBlockMember(Block);
  }
}

void RegionOverlay::addEdge(BlockId From, BlockId To) {
  if (Manager != nullptr) {
    Manager->addEdge(From, To);
  }
}

void RegionOverlay::detachEdge(BlockId From, BlockId To) {
  if (Manager != nullptr) {
    Manager->detachEdge(From, To);
  }
}

void RegionOverlay::hideEdge(BlockId From, BlockId To) {
  if (Manager != nullptr) {
    Manager->hideEdge(Id, From, To);
  }
}

void RegionOverlay::hideEdgeToSuccessor(BlockId Successor) {
  if (Manager != nullptr) {
    Manager->hideEdgeToSuccessor(Id, Successor);
  }
}

void RegionOverlay::markEdge(const OverlayEdgeEndpoint &From,
                             const OverlayEdgeEndpoint &To,
                             const std::string &Key) {
  if (Manager != nullptr) {
    Manager->markEdge(Id, From, To, Key);
  }
}

void RegionOverlay::dropEdgeMarksFrom(const OverlayNodeKey &From,
                                      const std::string &Key) {
  if (Manager != nullptr) {
    Manager->dropEdgeMarksFrom(Id, From, Key);
  }
}

void RegionOverlay::removeEdgeWithSuccessorsOnly(
    const OverlayEdgeEndpoint &From, const OverlayEdgeEndpoint &To) {
  if (Manager != nullptr) {
    Manager->removeEdgeWithSuccessorsOnly(Id, From, To);
  }
}

void RegionOverlay::addExtraFullEdge(const OverlayEdgeEndpoint &From,
                                     const OverlayEdgeEndpoint &To) {
  if (Manager != nullptr) {
    Manager->addExtraFullEdge(Id, From, To);
  }
}

void RegionOverlay::absorbSuccessorInto(
    const OverlayEdgeEndpoint &Successor, const OverlayEdgeEndpoint &NewNode) {
  if (Manager != nullptr) {
    Manager->absorbSuccessorInto(Id, Successor, NewNode);
  }
}

void RegionOverlay::finalize(NodeId RootId, const SuccessorSnapshot &Snapshot) {
  if (Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  Manager->setStructuredRoot(Id, RootId, Snapshot);
}

void RegionOverlay::collapseTo(NodeId RootId) {
  if (Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  Manager->collapseRegionTo(Id, RootId);
}

void RegionOverlay::replaceNodes(const std::vector<OverlayNodeKey> &OldNodes,
                                 NodeId RootId, bool SelfLoop) {
  if (Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  Manager->replaceNodes(Id, OldNodes, RootId, SelfLoop);
}

void RegionOverlay::replaceNodes(
    const std::vector<OverlayNodeKey> &OldNodes, NodeId RootId,
    const std::optional<OverlayNodeKey> &AbsorbedSuccessor, bool SelfLoop) {
  replaceNodes(OldNodes, RootId, SelfLoop);
  if (!AbsorbedSuccessor || Manager == nullptr || RootId == InvalidNodeId) {
    return;
  }
  const std::vector<OverlayMember> &ViewMembers = Manager->members(Id);
  auto NewMemberIt =
      std::find_if(ViewMembers.begin(), ViewMembers.end(),
                   [&](const OverlayMember &Member) {
                     return Member.Kind == OverlayMemberKind::Structured &&
                            Member.StructuredRoot == RootId &&
                            Member.Region == Id;
                   });
  if (NewMemberIt == ViewMembers.end()) {
    return;
  }
  absorbSuccessorInto(
      OverlayEdgeEndpoint::external(*AbsorbedSuccessor),
      OverlayEdgeEndpoint::member(*NewMemberIt));
}

void RegionOverlay::dissolve() {
  if (Manager == nullptr) {
    return;
  }
  Manager->dissolveRegionMembers(Id);
  Manager->clearStructuredRoot(Id);
}

} // namespace notdec::backend::structuring
