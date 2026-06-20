#ifndef NOTDEC_BACKENDS_STRUCTURING_GOTOSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_GOTOSTRUCTURER_H

#include "notdec-backends/Structuring/RegionStructurer.h"
#include "notdec-backends/Structuring/Structurer.h"

#include <map>

namespace notdec::backend::structuring {

// Minimal total structurer. It preserves every block as a labeled leaf and
// emits explicit control transfers, giving new language backends a complete
// fallback before Phoenix/Ghidra-style reducers are ported.
class GotoStructurer : public Structurer, public RegionStructurer {
public:
  StructuredTree structure(const StructuredCFG &Cfg) override;
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override;
  NodeId structureRegion(const StructuredCFG &Cfg, const RegionTree &Regions,
                         const Region &R,
                         const std::map<RegionId, NodeId> &StructuredChildren,
                         StructuredTree &Tree) override;
  bool supportsChildRegions() const override { return true; }
};

} // namespace notdec::backend::structuring

#endif
