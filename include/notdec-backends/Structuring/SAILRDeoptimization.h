#ifndef NOTDEC_BACKENDS_STRUCTURING_SAILRDEOPTIMIZATION_H
#define NOTDEC_BACKENDS_STRUCTURING_SAILRDEOPTIMIZATION_H

#include "notdec-backends/Structuring/StructuringOptimizationPipeline.h"

#include <cstddef>

namespace notdec::backend::structuring {

// Shared SAILR deoptimization pass set. These passes operate only on
// StructuredCFG, so C and Solidity see the same copied/virtual block graph.
class DuplicationReverter : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit DuplicationReverter(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class ReturnDuplicatorLow : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit ReturnDuplicatorLow(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class SwitchDefaultCaseDuplicator : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit SwitchDefaultCaseDuplicator(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class SwitchReusedEntryRewriter : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit SwitchReusedEntryRewriter(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class LoweredSwitchSimplifier : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit LoweredSwitchSimplifier(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

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
