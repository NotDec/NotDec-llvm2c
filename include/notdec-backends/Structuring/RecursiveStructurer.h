#ifndef NOTDEC_BACKENDS_STRUCTURING_RECURSIVESTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_RECURSIVESTRUCTURER_H

#include "notdec-backends/Structuring/RegionOverlay.h"
#include "notdec-backends/Structuring/RegionStructurer.h"

namespace notdec::backend::structuring {

// Drives region-by-region structuring. The first implementation only has a
// root region, but this is the public entry point that Phoenix/SAILR should
// target instead of taking over backend-specific CFGs.
class RecursiveStructurer {
public:
  StructuredTree structure(const StructuredCFG &Cfg, const RegionTree &Regions,
                           RegionStructurer &Structurer);
  StructuredTree structure(const StructuredCFG &Cfg, OverlayManager &Manager,
                           RegionStructurer &Structurer);
};

} // namespace notdec::backend::structuring

#endif
