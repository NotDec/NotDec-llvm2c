#include "notdec-backends/Structuring/StructuringOptimizationPipeline.h"

#include <utility>

namespace notdec::backend::structuring {

void StructuringOptimizationPipeline::addPass(
    std::unique_ptr<StructuringOptimizationPass> Pass) {
  if (Pass != nullptr) {
    Passes.push_back(std::move(Pass));
  }
}

StructuringOptimizationPipelineResult
StructuringOptimizationPipeline::run(const StructuredCFG &Cfg,
                                     RegionStructurer &Structurer) {
  StructuringOptimizationPipelineResult PipelineResult;
  PipelineResult.Output = Cfg;

  for (const std::unique_ptr<StructuringOptimizationPass> &Pass : Passes) {
    StructuringOptimizationResult PassResult =
        Pass->analyze(PipelineResult.Output, Structurer);
    if (!PassResult.Succeeded || !PassResult.Changed) {
      continue;
    }

    PipelineResult.Changed = true;
    PipelineResult.Output = std::move(PassResult.Output);
    PipelineResult.Evaluation = std::move(PassResult.Evaluation);
  }

  return PipelineResult;
}

} // namespace notdec::backend::structuring
