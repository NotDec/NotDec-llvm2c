#include "notdec-backends/Structuring/StructuringOptimizationPass.h"

#include <algorithm>

namespace notdec::backend::structuring {
namespace {

ControlFlowStructureCounter
qualityWithGotos(ControlFlowStructureCounter Quality,
                 const GotoManager &Gotos,
                 const ControlFlowStructureCounter &InitialQuality) {
  Quality.GotoTargets.clear();
  for (const StructuredGoto &Goto : Gotos.gotos()) {
    if (Goto.Target != InvalidBlockId) {
      ++Quality.GotoTargets[Goto.Target];
    }
  }

  for (auto It = Quality.GotoTargets.begin();
       It != Quality.GotoTargets.end();) {
    bool HasCurrentLabel =
        std::find(Quality.OrderedLabels.begin(), Quality.OrderedLabels.end(),
                  It->first) != Quality.OrderedLabels.end();
    bool WasInitialTarget = InitialQuality.GotoTargets.count(It->first) != 0;
    if (!HasCurrentLabel && !WasInitialTarget) {
      It = Quality.GotoTargets.erase(It);
      continue;
    }
    ++It;
  }

  Quality.OrderedLabels.erase(
      std::remove_if(Quality.OrderedLabels.begin(), Quality.OrderedLabels.end(),
                     [&](BlockId Label) {
                       return Quality.GotoTargets.count(Label) == 0;
                     }),
      Quality.OrderedLabels.end());
  return Quality;
}

} // namespace

GotoManager StructuringOptimizationPass::getNewGotos(
    const StructuredCFG &Cfg, const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  (void)Cfg;
  (void)Initial;
  return Current.Gotos;
}

bool StructuringOptimizationPass::needsInitialEvaluation() const {
  return Options.EvaluateInputBeforeRun &&
         (Options.RequireStructurableGraph || Options.RequireGotos ||
          Options.PreventNewGotos || Options.StrictlyLessGotos ||
          Options.MustImproveRelativeQuality);
}

bool StructuringOptimizationPass::acceptsFinalEvaluation(
    const StructuredCFG &Cfg, const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  GotoManager FinalGotos = getNewGotos(Cfg, Initial, Current);
  if (Options.PreventNewGotos || Options.StrictlyLessGotos) {
    std::size_t InitialGotos = Initial.Gotos.size();
    std::size_t CurrentGotos = FinalGotos.size();
    if (Options.StrictlyLessGotos) {
      if (CurrentGotos >= InitialGotos) {
        return false;
      }
    } else if (CurrentGotos > InitialGotos) {
      return false;
    }
  }

  if (Options.MustImproveRelativeQuality) {
    ControlFlowStructureCounter FilteredCurrentQuality =
        qualityWithGotos(Current.Quality, FinalGotos, Initial.Quality);
    if (!improvesRelativeStructuringQuality(Initial.Quality,
                                            FilteredCurrentQuality)) {
      return false;
    }
  }

  return true;
}

StructuringOptimizationResult
StructuringOptimizationPass::analyze(const StructuredCFG &Cfg,
                                     RegionStructurer &Structurer) {
  StructuringOptimizationResult Result;
  if (Options.MaxInputBlocks != 0 &&
      Cfg.blocks().size() > Options.MaxInputBlocks) {
    return Result;
  }

  StructuringEvaluator Evaluator;
  StructuringEvaluation Initial;

  bool HasInitialEvaluation = needsInitialEvaluation();
  if (HasInitialEvaluation) {
    Initial = Evaluator.evaluate(Cfg, Structurer, Options.EdgesToRemove);
  } else {
    Initial.Succeeded = true;
  }

  if (Options.RequireStructurableGraph && !Initial.Succeeded) {
    return Result;
  }
  if (Options.RequireGotos && Initial.Gotos.empty()) {
    return Result;
  }

  StructuredCFG Candidate = Cfg;
  bool HadChanges = false;
  StructuringEvaluation Current = Initial;

  std::size_t Iterations = Options.MaxOptIters == 0 ? 1 : Options.MaxOptIters;
  for (std::size_t Iter = 0; Iter < Iterations; ++Iter) {
    if (Options.RequireGotos && Current.Succeeded && Current.Gotos.empty()) {
      break;
    }

    StructuredCFG Previous = Candidate;
    bool Changed = runOnGraph(Candidate, Current);
    if (!Changed) {
      break;
    }

    if (!HasInitialEvaluation) {
      Current = Evaluator.evaluate(Previous, Structurer,
                                   Options.EdgesToRemove);
      Initial = Current;
      HasInitialEvaluation = true;
      if (Options.RequireStructurableGraph && !Current.Succeeded) {
        return Result;
      }
    }

    Current =
        Evaluator.evaluate(Candidate, Structurer, Options.EdgesToRemove);
    if (!Current.Succeeded) {
      if (!Options.RecoverStructureFails) {
        return Result;
      }
      Candidate = Previous;
      Current =
          Evaluator.evaluate(Candidate, Structurer, Options.EdgesToRemove);
      continue;
    }
    HadChanges = true;

    if (Options.MaxOptIters <= 1) {
      break;
    }
  }

  if (!HadChanges) {
    return Result;
  }

  if (Options.RequireStructurableGraph && !Current.Succeeded) {
    return Result;
  }
  if (!acceptsFinalEvaluation(Candidate, Initial, Current)) {
    return Result;
  }

  Result.Succeeded = true;
  Result.Changed = true;
  Result.Output = std::move(Candidate);
  Result.Evaluation = std::move(Current);
  return Result;
}

} // namespace notdec::backend::structuring
