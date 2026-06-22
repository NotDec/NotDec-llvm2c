#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGOPTIMIZATIONPIPELINE_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGOPTIMIZATIONPIPELINE_H

#include "notdec-backends/Structuring/StructuringOptimizationPass.h"

#include <memory>
#include <string>
#include <vector>

namespace notdec::backend::structuring {

// Shared SAILR deoptimization runner. It matches Angr's pass-manager shape:
// each pass gets the current graph, may return a rewritten graph, and rejected
// rewrites are skipped before the next pass runs.
struct StructuringOptimizationPipelineResult {
  bool Changed = false;
  StructuredCFG Output;
  StructuringEvaluation Evaluation;
};

class StructuringOptimizationPipeline {
public:
  void addPass(std::unique_ptr<StructuringOptimizationPass> Pass);
  std::vector<std::string> passNames() const;

  StructuringOptimizationPipelineResult run(const StructuredCFG &Cfg,
                                            RegionStructurer &Structurer);

private:
  std::vector<std::unique_ptr<StructuringOptimizationPass>> Passes;
};

} // namespace notdec::backend::structuring

#endif
