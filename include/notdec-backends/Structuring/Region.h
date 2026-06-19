#ifndef NOTDEC_BACKENDS_STRUCTURING_REGION_H
#define NOTDEC_BACKENDS_STRUCTURING_REGION_H

#include "notdec-backends/Structuring/StructuredCFG.h"

#include <limits>
#include <vector>

namespace notdec::backend::structuring {

using RegionId = std::uint32_t;

constexpr RegionId InvalidRegionId = std::numeric_limits<RegionId>::max();

// A region is the unit that recursive structuring works on. The first version
// only needs a root region, but keeping children/successors here makes the
// Phoenix/SAILR port use the same shape later instead of adding another CFG
// wrapper.
struct Region {
  RegionId Id = InvalidRegionId;
  BlockId Head = InvalidBlockId;
  std::vector<BlockId> Blocks;
  std::vector<BlockId> Successors;
  std::vector<RegionId> Children;
};

class RegionTree {
public:
  RegionId addRegion(Region R);

  RegionId root() const { return Root; }
  void setRoot(RegionId Id) { Root = Id; }

  const Region *getRegion(RegionId Id) const;
  Region *getRegion(RegionId Id);
  const std::vector<Region> &regions() const { return Regions; }

private:
  RegionId Root = InvalidRegionId;
  std::vector<Region> Regions;
};

} // namespace notdec::backend::structuring

#endif
