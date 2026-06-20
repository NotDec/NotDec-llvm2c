#ifndef _NOTDEC_BACKEND_STRUCTURING_CONTEXT_H_
#define _NOTDEC_BACKEND_STRUCTURING_CONTEXT_H_

#include <set>
#include <vector>

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/Analysis/CFG.h>
#include <clang/Basic/Specifiers.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/Support/Casting.h>

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/Utils.h"

namespace notdec::llvm2c {

class SAFuncContext;
struct ValueNamer;

clang::Stmt *getStmt(CFGElement Element);

// This is the minimal C-backend bridge that shared structuring still needs
// while the old StructuralAnalysis monolith is being split apart. Keep the CFG
// editing helpers here so StructuredGoto/Goto/CFG cleanup stop depending on the
// legacy Phoenix implementation header.
class IStructuralAnalysis {
protected:
  std::set<CFGBlock *> toRemove;
  SAFuncContext &FCtx;
  CFG &Cfg;
  clang::ASTContext &Ctx;

  clang::Expr *invertCond(clang::Expr *cond);
  clang::GotoStmt *createGotoStmt(clang::LabelDecl *label);
  void replaceAllUsesWith(clang::LabelDecl *label, clang::LabelDecl *newLabel);

public:
  IStructuralAnalysis(SAFuncContext &ctx);
  virtual ~IStructuralAnalysis() = default;
  virtual void execute() = 0;

  clang::ASTContext &getASTContext() { return Ctx; }
  class CFG &getCurrentCFG() { return Cfg; }
  clang::LabelDecl *getBlockLabel(CFGBlock *blk, bool prepend = false);
  void mergeBlock(llvm::BasicBlock &bb, llvm::BasicBlock &next);
  ValueNamer &getValueNamer();

  clang::Expr *castSwitchConditionToInt(clang::Expr *Cond);
  void removeLabelUse(clang::LabelStmt *Label, clang::GotoStmt *Goto);
  bool eraseLabelUseIfEmpty(clang::LabelStmt *Label);
  clang::Expr *takeBinaryCond(CFGBlock &B);
  static void addAllStmtTo(CFGBlock *From, CFGBlock *To);
  static void addAllStmtTo(CFGBlock *B, std::vector<clang::Stmt *> &stmts,
                           bool noAssert = false);
  clang::Stmt *makeCompoundStmt(CFGBlock *B, clang::Stmt *tail = nullptr);
  clang::Stmt *createWhileTrue(clang::Stmt *body);
  void replaceSuccessors(CFGBlock *From, CFGBlock *To);
  CFGBlock *linearSuccessor(CFGBlock *Block);
  CFGBlock *singlePredecessor(CFGBlock *Block);
  bool onlyPred(CFGBlock *Block, CFGBlock *Pred);
  void deferredRemove(CFGBlock *B);
  bool doRemoveBlocks();
};

class CFGCleaner : public IStructuralAnalysis {
public:
  CFGCleaner(SAFuncContext &Ctx);
  void execute() override;
};

} // namespace notdec::llvm2c

#endif
