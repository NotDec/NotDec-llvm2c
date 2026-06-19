#include "notdec-backends/Structuring/Region.h"

#include <utility>

namespace notdec::backend::structuring {

RegionId RegionTree::addRegion(Region R) {
  if (R.Id == InvalidRegionId) {
    R.Id = static_cast<RegionId>(Regions.size());
  }
  RegionId Id = R.Id;
  Regions.push_back(std::move(R));
  return Id;
}

const Region *RegionTree::getRegion(RegionId Id) const {
  if (Id >= Regions.size()) {
    return nullptr;
  }
  return &Regions[Id];
}

} // namespace notdec::backend::structuring
