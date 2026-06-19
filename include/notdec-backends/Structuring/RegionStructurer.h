#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONSTRUCTURER_H

#include "notdec-backends/Structuring/Region.h"

namespace notdec::backend::structuring {

// A replaceable algorithm for one region. Implementations append nodes to Tree
// and return the root node for this region.
class RegionStructurer {
public:
  virtual ~RegionStructurer() = default;
  virtual NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                                 StructuredTree &Tree) = 0;
};

} // namespace notdec::backend::structuring

#endif
