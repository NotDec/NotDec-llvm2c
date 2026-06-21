#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGEVALUATOR_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGEVALUATOR_H

#include "notdec-backends/Structuring/GotoManager.h"
#include "notdec-backends/Structuring/RegionStructurer.h"
#include "notdec-backends/Structuring/StructuringQuality.h"

namespace notdec::backend::structuring {

// One read-only structuring trial used by future SAILR deoptimization passes.
// It mirrors the first half of Angr's StructuringOptimizationPass check:
// identify regions, recursively structure, then collect goto facts from the
// structured result. Graph rewrites and fixed-point checks stay outside.
struct StructuringEvaluation {
  StructuredTree Tree;
  GotoManager Gotos;
  ControlFlowStructureCounter Quality;
  bool Succeeded = false;
};

class StructuringEvaluator {
public:
  StructuringEvaluation evaluate(const StructuredCFG &Cfg,
                                 RegionStructurer &Structurer) const;
};

} // namespace notdec::backend::structuring

#endif
