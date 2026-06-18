#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURER_H

#include "notdec-backends/Structuring/StructuredCFG.h"

namespace notdec::backend::structuring {

// Common interface for control-flow structuring algorithms. Implementations
// should return a StructuredTree even for irreducible CFGs; conservative
// algorithms can fall back to explicit labels and gotos.
class Structurer {
public:
  virtual ~Structurer() = default;
  virtual StructuredTree structure(const StructuredCFG &Cfg) = 0;
};

} // namespace notdec::backend::structuring

#endif
