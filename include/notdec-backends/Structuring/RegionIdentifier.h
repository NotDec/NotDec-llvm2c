#ifndef NOTDEC_BACKENDS_STRUCTURING_REGIONIDENTIFIER_H
#define NOTDEC_BACKENDS_STRUCTURING_REGIONIDENTIFIER_H

#include "notdec-backends/Structuring/Region.h"

namespace notdec::backend::structuring {

// Conservative first step toward Angr-style structuring. It identifies the
// whole function as one root region. Later versions should add natural loops
// and switch/if regions without changing backend renderers.
class RegionIdentifier {
public:
  static RegionTree identifyRoot(const StructuredCFG &Cfg);
};

} // namespace notdec::backend::structuring

#endif
