#ifndef _NOTDEC_BACKEND_COMPOUNDCONDITIONBUILDER_H_
#define _NOTDEC_BACKEND_COMPOUNDCONDITIONBUILDER_H_

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/structural-analysis.h"

namespace notdec::llvm2c {

/// find and build logical and/or expressions
class CompoundConditionBuilder : IStructuralAnalysis {
public:
  CompoundConditionBuilder(SAFuncContext &Ctx) : IStructuralAnalysis(Ctx) {}

  bool maybeCoalesce(CFGBlock *block);
  void rebuildGraph(CFGBlock *head, CFGBlock *redundant, CFGBlock *body,
                    CFGBlock *succ);
  void execute() override;
};

} // namespace notdec::llvm2c

#endif
