#include "notdec-backends/Structuring/StructuringOptimizationPass.h"

namespace notdec::backend::structuring {

GotoManager StructuringOptimizationPass::getNewGotos(
    const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  (void)Initial;
  return Current.Gotos;
}

bool StructuringOptimizationPass::needsInitialEvaluation() const {
  return Options.RequireStructurableGraph || Options.RequireGotos ||
         Options.PreventNewGotos || Options.StrictlyLessGotos ||
         Options.MustImproveRelativeQuality;
}

bool StructuringOptimizationPass::acceptsFinalEvaluation(
    const StructuringEvaluation &Initial,
    const StructuringEvaluation &Current) const {
  if (Options.PreventNewGotos || Options.StrictlyLessGotos) {
    std::size_t InitialGotos = Initial.Gotos.size();
    GotoManager NewGotos = getNewGotos(Initial, Current);
    std::size_t CurrentGotos = NewGotos.size();
    if (Options.StrictlyLessGotos) {
      if (CurrentGotos >= InitialGotos) {
        return false;
      }
    } else if (CurrentGotos > InitialGotos) {
      return false;
    }
  }

  if (Options.MustImproveRelativeQuality &&
      !improvesRelativeStructuringQuality(Initial.Quality, Current.Quality)) {
    return false;
  }

  return true;
}

StructuringOptimizationResult
StructuringOptimizationPass::analyze(const StructuredCFG &Cfg,
                                     RegionStructurer &Structurer) {
  StructuringOptimizationResult Result;
  StructuringEvaluator Evaluator;
  StructuringEvaluation Initial;

  if (needsInitialEvaluation()) {
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

    Current = Evaluator.evaluate(Candidate, Structurer, Options.EdgesToRemove);
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
  if (!acceptsFinalEvaluation(Initial, Current)) {
    return Result;
  }

  Result.Succeeded = true;
  Result.Changed = true;
  Result.Output = std::move(Candidate);
  Result.Evaluation = std::move(Current);
  return Result;
}

} // namespace notdec::backend::structuring
