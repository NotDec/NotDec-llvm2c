#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONSTRUCTURER_H

#include "notdec-backends/Structuring/Region.h"

#include <map>

namespace notdec::backend::structuring {

// A replaceable algorithm for one region. Implementations append nodes to Tree
// and return the root node for this region.
class RegionStructurer {
public:
  virtual ~RegionStructurer() = default;
  virtual bool supportsChildRegions() const { return false; }
  virtual bool shouldPassChildRegionToParent(const Region &Parent,
                                             const Region &Child) const {
    if (Parent.Kind == RegionKind::Root &&
        Child.Kind == RegionKind::NaturalLoop) {
      return false;
    }
    return true;
  }
  virtual NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                                 StructuredTree &Tree) = 0;
  virtual NodeId
  structureRegion(const StructuredCFG &Cfg, const RegionTree &Regions,
                  const Region &R,
                  const std::map<RegionId, NodeId> &StructuredChildren,
                  StructuredTree &Tree) {
    (void)Regions;
    (void)StructuredChildren;
    return structureRegion(Cfg, R, Tree);
  }
};

} // namespace notdec::backend::structuring

#endif
