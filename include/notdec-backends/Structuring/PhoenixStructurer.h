#ifndef NOTDEC_BACKENDS_STRUCTURING_PHOENIXSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_PHOENIXSTRUCTURER_H

#include "notdec-backends/Structuring/RegionStructurer.h"
#include "notdec-backends/Structuring/Structurer.h"

namespace notdec::backend::structuring {

// Phoenix-style reducer on the shared structuring model. This class is the
// backend-independent migration target for the old C Phoenix implementation:
// it mutates MutableRegionGraph, emits StructuredTree nodes, and never touches
// Clang or Solidity-specific ASTs.
class PhoenixStructurer : public Structurer, public RegionStructurer {
public:
  StructuredTree structure(const StructuredCFG &Cfg) override;
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override;
};

} // namespace notdec::backend::structuring

#endif
