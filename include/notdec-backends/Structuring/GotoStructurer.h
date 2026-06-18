#ifndef NOTDEC_BACKENDS_STRUCTURING_GOTOSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_GOTOSTRUCTURER_H

#include "notdec-backends/Structuring/Structurer.h"

namespace notdec::backend::structuring {

// Minimal total structurer. It preserves every block as a labeled leaf and
// emits explicit control transfers, giving new language backends a complete
// fallback before Phoenix/Ghidra-style reducers are ported.
class GotoStructurer : public Structurer {
public:
  StructuredTree structure(const StructuredCFG &Cfg) override;
};

} // namespace notdec::backend::structuring

#endif
