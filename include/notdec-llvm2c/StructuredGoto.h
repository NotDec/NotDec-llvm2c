#ifndef _NOTDEC_BACKEND_STRUCTURED_GOTO_H_
#define _NOTDEC_BACKEND_STRUCTURED_GOTO_H_

#include "notdec-llvm2c/StructuralAnalysis.h"

namespace notdec::llvm2c {

// Trial adapter for the shared structuring layer. It keeps C-specific Clang AST
// lowering in the old CFGBuilder path, converts that CFG to StructuredCFG, runs
// GotoStructurer, then renders the tree back to the old C CFG.
class StructuredGoto : IStructuralAnalysis {
public:
  StructuredGoto(SAFuncContext &ctx) : IStructuralAnalysis(ctx) {}

  void execute() override;

  clang::LabelDecl *getOrCreateBlockLabel(CFGBlock *Block) {
    return getBlockLabel(Block);
  }
  clang::LabelStmt *getOrCreateBlockLabelStmt(CFGBlock *Block) {
    getBlockLabel(Block);
    return Block->getLabelStmt();
  }
  clang::GotoStmt *makeGotoStmt(clang::LabelDecl *Label) {
    return createGotoStmt(Label);
  }
};

} // namespace notdec::llvm2c

#endif
