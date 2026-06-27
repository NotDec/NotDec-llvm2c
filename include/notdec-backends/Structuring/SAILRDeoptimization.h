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
  GotoManager getNewGotos(const StructuredCFG &Cfg,
                          const StructuringEvaluation &Initial,
                          const StructuringEvaluation &Current) const override;
};

class ReturnDuplicatorLow : public StructuringOptimizationPass {
public:
  static StructuringOptimizationOptions defaultOptions();

  explicit ReturnDuplicatorLow(
      StructuringOptimizationOptions Options = defaultOptions(),
      std::size_t MaxFunctionBlocks = 500,
      std::size_t MaxDuplicatedStatements = 16)
      : StructuringOptimizationPass(Options),
        MaxFunctionBlocks(MaxFunctionBlocks),
        MaxDuplicatedStatements(MaxDuplicatedStatements) {}

  const char *name() const override { return "ReturnDuplicatorLow"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;
  GotoManager getNewGotos(const StructuredCFG &Cfg,
                          const StructuringEvaluation &Initial,
                          const StructuringEvaluation &Current) const override;

private:
  std::size_t MaxFunctionBlocks;
  std::size_t MaxDuplicatedStatements;
};

class SwitchDefaultCaseDuplicator : public StructuringOptimizationPass {
public:
  // Angr models a reused switch default as a synthetic goto block. The
  // forwarder mode keeps the older NotDec behavior available for comparison.
  enum class SharedDefaultRewriteMode {
    SyntheticGoto,
    SyntheticForwarder,
  };

  static StructuringOptimizationOptions defaultOptions();

  explicit SwitchDefaultCaseDuplicator(
      StructuringOptimizationOptions Options = defaultOptions(),
      SharedDefaultRewriteMode SharedDefaultMode =
          SharedDefaultRewriteMode::SyntheticGoto)
      : StructuringOptimizationPass(Options),
        SharedDefaultMode(SharedDefaultMode) {}

  const char *name() const override { return "SwitchDefaultCaseDuplicator"; }

protected:
  bool runOnGraph(StructuredCFG &Graph,
                  const StructuringEvaluation &Current) override;

private:
  SharedDefaultRewriteMode SharedDefaultMode;
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

struct SAILRDeoptimizationPipelineOptions {
  SwitchDefaultCaseDuplicator::SharedDefaultRewriteMode SharedDefaultMode =
      SwitchDefaultCaseDuplicator::SharedDefaultRewriteMode::SyntheticGoto;
};

SAILRDeoptimizationPipelineOptions &
defaultSAILRDeoptimizationPipelineOptions();
void setDefaultSAILRDeoptimizationPipelineOptions(
    SAILRDeoptimizationPipelineOptions Options);

StructuringOptimizationPipeline buildSAILRDeoptimizationPipeline(
    SAILRDeoptimizationPipelineOptions Options =
        SAILRDeoptimizationPipelineOptions());

} // namespace notdec::backend::structuring

#endif
