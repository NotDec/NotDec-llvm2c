#include "notdec-backends/Structuring/StructuringEvaluator.h"

#include "notdec-backends/Structuring/RecursiveStructurer.h"
#include "notdec-backends/Structuring/RegionIdentifier.h"

#include <algorithm>

namespace notdec::backend::structuring {
namespace {

StructuredCFG withoutEdges(const StructuredCFG &Cfg,
                           const std::vector<StructuringEdge> &Edges) {
  if (Edges.empty()) {
    return Cfg;
  }

  StructuredCFG Copy = Cfg;
  for (CFGBlock &Block : Copy.blocks()) {
    for (const StructuringEdge &Edge : Edges) {
      if (Block.Id != Edge.first) {
        continue;
      }
      Block.Successors.erase(std::remove(Block.Successors.begin(),
                                         Block.Successors.end(), Edge.second),
                             Block.Successors.end());
      Block.Cases.erase(
          std::remove_if(Block.Cases.begin(), Block.Cases.end(),
                         [&](const SwitchCase &Case) {
                           return Case.Target == Edge.second;
                         }),
          Block.Cases.end());
    }

    if (Block.Successors.empty() && Block.Terminator != TerminatorKind::Return &&
        Block.Terminator != TerminatorKind::Unreachable) {
      Block.Terminator = TerminatorKind::Unreachable;
    }
  }
  return Copy;
}

} // namespace

StructuringEvaluation
StructuringEvaluator::evaluate(const StructuredCFG &Cfg,
                               RegionStructurer &Structurer) const {
  static const std::vector<StructuringEdge> NoRemovedEdges;
  return evaluate(Cfg, Structurer, NoRemovedEdges);
}

StructuringEvaluation StructuringEvaluator::evaluate(
    const StructuredCFG &Cfg, RegionStructurer &Structurer,
    const std::vector<StructuringEdge> &EdgesToRemove) const {
  StructuringEvaluation Result;
  StructuredCFG View = withoutEdges(Cfg, EdgesToRemove);
  OverlayManager Manager = RegionIdentifier::identifyOverlay(View);
  Result.Tree = RecursiveStructurer().structure(View, Manager, Structurer);
  Result.Succeeded = Result.Tree.root() != InvalidNodeId;
  if (Result.Succeeded) {
    Result.Gotos = GotoManager::collect(Result.Tree);
    Result.Quality = ControlFlowStructureCounter::collect(Result.Tree);
  }
  return Result;
}

} // namespace notdec::backend::structuring
