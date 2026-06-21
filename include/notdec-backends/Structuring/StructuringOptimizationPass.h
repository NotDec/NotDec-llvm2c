#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGOPTIMIZATIONPASS_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGOPTIMIZATIONPASS_H

#include "notdec-backends/Structuring/StructuringEvaluator.h"

#include <cstddef>
#include <vector>

namespace notdec::backend::structuring {

// Angr-style wrapper for SAILR deoptimization passes. Concrete passes only
// rewrite a StructuredCFG candidate; this wrapper owns the expensive checks:
// initial structuring, goto guards, fixed-point retries, rollback, and relative
// quality comparison. It is shared by C and Solidity because it only depends on
// the common structuring graph/tree types.
struct StructuringOptimizationOptions {
  bool RequireStructurableGraph = true;
  bool PreventNewGotos = true;
  bool StrictlyLessGotos = false;
  bool RecoverStructureFails = true;
  bool MustImproveRelativeQuality = true;
  bool RequireGotos = true;
  std::size_t MaxOptIters = 1;
  std::vector<StructuringEdge> EdgesToRemove;
};

struct StructuringOptimizationResult {
  bool Succeeded = false;
  bool Changed = false;
  StructuredCFG Output;
  StructuringEvaluation Evaluation;
};

class StructuringOptimizationPass {
public:
  explicit StructuringOptimizationPass(StructuringOptimizationOptions Options =
                                           StructuringOptimizationOptions())
      : Options(Options) {}
  virtual ~StructuringOptimizationPass() = default;

  StructuringOptimizationResult analyze(const StructuredCFG &Cfg,
                                        RegionStructurer &Structurer);

protected:
  virtual bool runOnGraph(StructuredCFG &Graph,
                          const StructuringEvaluation &Current) = 0;

private:
  bool needsInitialEvaluation() const;
  bool acceptsFinalEvaluation(const StructuringEvaluation &Initial,
                              const StructuringEvaluation &Current) const;

  StructuringOptimizationOptions Options;
};

} // namespace notdec::backend::structuring

#endif
