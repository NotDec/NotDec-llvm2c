#ifndef NOTDEC_BACKENDS_STRUCTURING_SAILRDEOPTIMIZATION_H
#define NOTDEC_BACKENDS_STRUCTURING_SAILRDEOPTIMIZATION_H

#include "notdec-backends/Structuring/StructuringOptimizationPipeline.h"

#include <cstddef>

namespace notdec::backend::structuring {

// Shared SAILR deoptimization pass set. These passes operate only on
// StructuredCFG, so C and Solidity see the same copied/virtual block graph.
class CrossJumpReverter : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit CrossJumpReverter(
      StructuringOptimizationOptions Options = defaultOptions(),
      std::size_t MaxDuplicatedStatements = 16)
      : StructuringOptimizationPass(Options),
        MaxDuplicatedStatements(MaxDuplicatedStatements) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;

private:
  std::size_t MaxDuplicatedStatements;
};

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline();

} // namespace notdec::backend::structuring

#endif
