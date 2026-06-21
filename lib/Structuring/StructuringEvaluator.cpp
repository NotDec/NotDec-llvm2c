#include "notdec-backends/Structuring/StructuringEvaluator.h"

#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

namespace notdec::backend::structuring {

StructuringEvaluation
StructuringEvaluator::evaluate(const StructuredCFG &Cfg,
                               RegionStructurer &Structurer) const {
  StructuringEvaluation Result;
  OverlayManager Manager = RegionIdentifier::identifyOverlay(Cfg);
  Result.Tree = RecursiveStructurer().structure(Cfg, Manager, Structurer);
  Result.Succeeded = Result.Tree.root() != InvalidNodeId;
  if (Result.Succeeded) {
    Result.Gotos = GotoManager::collect(Result.Tree);
    Result.Quality = ControlFlowStructureCounter::collect(Result.Tree);
  }
  return Result;
}

} // namespace notdec::backend::structuring
