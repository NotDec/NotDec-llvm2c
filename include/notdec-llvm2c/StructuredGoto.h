#ifndef _NOTDEC_BACKEND_STRUCTURED_GOTO_H_
#define _NOTDEC_BACKEND_STRUCTURED_GOTO_H_

#include "notdec-llvm2c/StructuralAnalysis.h"

#include <string_view>

namespace notdec::llvm2c {

// Adapter for the shared structuring layer. It keeps C-specific Clang AST
// lowering in the old CFGBuilder path, converts that CFG to StructuredCFG, runs
// the selected shared structurer, then renders the tree back to the old C CFG.
class StructuredGoto : IStructuralAnalysis {
  std::string_view StructurerName;

public:
  StructuredGoto(SAFuncContext &ctx, std::string_view StructurerName = "goto")
      : IStructuralAnalysis(ctx), StructurerName(StructurerName) {}

  void execute() override;
  std::string_view getStructurerName() const { return StructurerName; }

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
