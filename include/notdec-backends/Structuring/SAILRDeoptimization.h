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

  const char *name() const override { return "DuplicationReverter"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class ReturnDuplicatorLow : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit ReturnDuplicatorLow(
      StructuringOptimizationOptions Options = defaultOptions(),
      std::size_t MaxFunctionBlocks = 500)
      : StructuringOptimizationPass(Options), MaxFunctionBlocks(MaxFunctionBlocks) {}

  const char *name() const override { return "ReturnDuplicatorLow"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;

private:
  std::size_t MaxFunctionBlocks;
};

class SwitchDefaultCaseDuplicator : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit SwitchDefaultCaseDuplicator(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

  const char *name() const override { return "SwitchDefaultCaseDuplicator"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
};

class SwitchReusedEntryRewriter : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit SwitchReusedEntryRewriter(
      StructuringOptimizationOptions Options = defaultOptions(),
      std::size_t MaxEntryReuseCount = 10,
      std::size_t MaxReusedEntries = 20)
      : StructuringOptimizationPass(Options),
        MaxEntryReuseCount(MaxEntryReuseCount),
        MaxReusedEntries(MaxReusedEntries) {}

  const char *name() const override { return "SwitchReusedEntryRewriter"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;

private:
  std::size_t MaxEntryReuseCount;
  std::size_t MaxReusedEntries;
};

class LoweredSwitchSimplifier : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit LoweredSwitchSimplifier(
      StructuringOptimizationOptions Options = defaultOptions())
      : StructuringOptimizationPass(Options) {}

  const char *name() const override { return "LoweredSwitchSimplifier"; }

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

  const char *name() const override { return "CrossJumpReverter"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;

private:
  std::size_t MaxDuplicatedStatements;
};

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline();

} // namespace notdec::backend::structuring

#endif
