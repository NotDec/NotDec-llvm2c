#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGQUALITY_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURINGQUALITY_H

#include "notdec-backends/Structuring/StructuredCFG.h"

#include <map>
#include <vector>

namespace notdec::backend::structuring {

// Shared structure-quality summary used by SAILR deoptimization checks.
// Angr computes the same kind of facts after recursive structuring so an
// optimization can reject rewrites that keep the graph structurable but make
// the resulting control flow worse.
struct ControlFlowStructureCounter {
  unsigned WhileLoops = 0;
  unsigned DoWhileLoops = 0;
  unsigned InfiniteLoops = 0;

  // NotDec currently has no structured for-loop node. Keep the field explicit
  // so the later Angr quality comparison can stay one-to-one.
  unsigned ForLoops = 0;

  std::map<BlockId, unsigned> GotoTargets;
  std::vector<BlockId> OrderedLabels;

  static ControlFlowStructureCounter collect(const StructuredTree &Tree);
};

bool improvesRelativeStructuringQuality(
    const ControlFlowStructureCounter &Initial,
    const ControlFlowStructureCounter &Current);

} // namespace notdec::backend::structuring

#endif
