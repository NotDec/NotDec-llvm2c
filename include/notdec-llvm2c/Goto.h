#ifndef _NOTDEC_BACKEND_GOTO_H_
#define _NOTDEC_BACKEND_GOTO_H_

#include "notdec-llvm2c/StructuralAnalysis.h"
#include <clang/AST/Stmt.h>
#include <map>
#include <vector>

namespace notdec::llvm2c {

class Goto : IStructuralAnalysis {


public:
  Goto(SAFuncContext &ctx) : IStructuralAnalysis(ctx) {}

  void execute() override;
  void simplifyBlock(CFGBlock &Block);
};

} // namespace notdec::llvm2c

#endif
