#include "notdec-llvm2c/StructuringContext.h"

#include <cassert>
#include <set>
#include <vector>

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/Analysis/CFG.h>
#include <llvm/ADT/APInt.h>
#include <llvm/Support/Casting.h>

#include "notdec-llvm2c/StructuralAnalysis.h"

namespace notdec::llvm2c {

IStructuralAnalysis::IStructuralAnalysis(SAFuncContext &ctx)
    : FCtx(ctx), Cfg(FCtx.getCFG()), Ctx(ctx.getASTContext()) {}

clang::Expr *IStructuralAnalysis::invertCond(clang::Expr *cond) {
  if (auto *BO = llvm::dyn_cast<clang::BinaryOperator>(cond)) {
    switch (BO->getOpcode()) {
    case clang::BO_EQ:
      BO->setOpcode(clang::BO_NE);
      return BO;
    case clang::BO_NE:
      BO->setOpcode(clang::BO_EQ);
      return BO;
    case clang::BO_LT:
      BO->setOpcode(clang::BO_GE);
      return BO;
    case clang::BO_GT:
      BO->setOpcode(clang::BO_LE);
      return BO;
    case clang::BO_LE:
      BO->setOpcode(clang::BO_GT);
      return BO;
    case clang::BO_GE:
      BO->setOpcode(clang::BO_LT);
      return BO;
    default:
      break;
    }
  }
  return createUnaryOperator(FCtx.getASTContext(), cond, clang::UO_LNot,
                             FCtx.getASTContext().IntTy, clang::VK_PRValue);
}

clang::GotoStmt *IStructuralAnalysis::createGotoStmt(clang::LabelDecl *label) {
  return FCtx.createGotoStmt(label);
}

void IStructuralAnalysis::replaceAllUsesWith(clang::LabelDecl *label,
                                             clang::LabelDecl *newLabel) {
  FCtx.replaceAllUsesWith(label, newLabel);
}

ValueNamer &IStructuralAnalysis::getValueNamer() {
  return FCtx.getSAContext().getValueNamer();
}

clang::Expr *IStructuralAnalysis::takeBinaryCond(CFGBlock &B) {
  assert(B.succ_size() == 2 && "getBinaryCond: block should have 2 successors!");
  auto ret = llvm::cast<clang::Expr>(B.getTerminatorStmt());
  B.setTerminator(nullptr);
  return ret;
}

void IStructuralAnalysis::addAllStmtTo(CFGBlock *From, CFGBlock *To) {
  auto label = llvm::cast_or_null<clang::LabelStmt>(From->getLabel());
  if (label != nullptr) {
    To->appendStmt(label);
  }
  for (auto elem = From->begin(); elem != From->end(); ++elem) {
    if (auto stmt = getStmt(*elem)) {
      To->appendStmt(stmt);
    }
  }
  if (From->getTerminatorStmt()) {
    assert(false && "Terminator should be handled first!");
  }
  From->clear();
}

void IStructuralAnalysis::addAllStmtTo(CFGBlock *B,
                                       std::vector<clang::Stmt *> &stmts,
                                       bool noAssert) {
  auto label = llvm::cast_or_null<clang::LabelStmt>(B->getLabel());
  if (label != nullptr) {
    stmts.push_back(label);
  }
  for (auto elem = B->begin(); elem != B->end(); ++elem) {
    if (auto stmt = getStmt(*elem)) {
      stmts.push_back(stmt);
    }
  }
  if (B->getTerminatorStmt() && !noAssert) {
    assert(false && "Terminator should be handled first!");
  }
  B->clear();
}

clang::Stmt *IStructuralAnalysis::makeCompoundStmt(CFGBlock *B,
                                                   clang::Stmt *tail) {
  std::vector<clang::Stmt *> stmts;
  addAllStmtTo(B, stmts);
  if (tail != nullptr) {
    stmts.push_back(tail);
  }
  B->clear();
  if (stmts.size() == 1) {
    return stmts[0];
  }
  return clang::CompoundStmt::Create(FCtx.getASTContext(), stmts,
                                     clang::FPOptionsOverride(),
                                     clang::SourceLocation(),
                                     clang::SourceLocation());
}

clang::Stmt *IStructuralAnalysis::createWhileTrue(clang::Stmt *body) {
  auto &AstCtx = FCtx.getASTContext();
  auto tru = clang::IntegerLiteral::Create(
      AstCtx, llvm::APInt(32, 1, true), AstCtx.IntTy, clang::SourceLocation());
  return clang::WhileStmt::Create(
      AstCtx, nullptr, tru, body, clang::SourceLocation(),
      clang::SourceLocation(), clang::SourceLocation());
}

void IStructuralAnalysis::replaceSuccessors(CFGBlock *From, CFGBlock *To) {
  assert(To->succ_size() == 0);
  To->moveSuccFrom(From);
  for (auto Ent : std::set<CFGBlock *>(To->succ_begin(), To->succ_end())) {
    Ent->replacePred(From, To);
  }
}

CFGBlock *IStructuralAnalysis::linearSuccessor(CFGBlock *Block) {
  if (Block->succ_size() != 1) {
    return nullptr;
  }
  return *Block->succ_begin();
}

CFGBlock *IStructuralAnalysis::singlePredecessor(CFGBlock *Block) {
  if (Block->pred_size() != 1) {
    return nullptr;
  }
  return *Block->pred_begin();
}

bool IStructuralAnalysis::onlyPred(CFGBlock *Block, CFGBlock *Pred) {
  for (auto &P : Block->preds()) {
    if (P.getBlock() != Pred) {
      return false;
    }
  }
  return true;
}

void IStructuralAnalysis::deferredRemove(CFGBlock *B) {
  assert(B->succ_size() == 0);
  assert(B->pred_size() == 0);
  assert(B->getLabel() == nullptr);
  assert(B->size() == 0);
  assert(B->getTerminatorStmt() == nullptr);
  toRemove.insert(B);
}

bool IStructuralAnalysis::doRemoveBlocks() {
  bool changed = false;
  Cfg.sanityCheck();
  for (auto block : toRemove) {
    Cfg.remove(block);
    Cfg.sanityCheck();
    changed = true;
  }
  Cfg.sanityCheck();
  toRemove.clear();
  return changed;
}

CFGCleaner::CFGCleaner(SAFuncContext &Ctx) : IStructuralAnalysis(Ctx) {}

void CFGCleaner::execute() {
  std::vector<CFGBlock *> blks(Cfg.begin(), Cfg.end());
  for (auto Block : blks) {
    if (Block->size() == 0 && Block->succ_size() == 1 &&
        Block->pred_size() > 0) {
      auto succ = linearSuccessor(Block);
      if (succ == Block) {
        continue;
      }
      removeAllEdge(Block, succ);
      std::vector<CFGBlock *> Preds(Block->pred_begin(), Block->pred_end());
      for (auto pred : Preds) {
        replaceAllSucc(pred, Block, succ);
      }
      deferredRemove(Block);
      Cfg.sanityCheck();
    }
  }
  doRemoveBlocks();
}

} // namespace notdec::llvm2c
