#include <cstdlib>

#include <llvm/Support/Casting.h>
#include <llvm/Support/Debug.h>

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/Analysis/CFG.h>
#include <clang/Basic/Specifiers.h>
#include <vector>

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/Goto.h"
#include "notdec-llvm2c/Utils.h"

namespace notdec::llvm2c {

#define DEBUG_TYPE "structural-goto"

template <typename ForwardIt>
ForwardIt
next(ForwardIt it,
     typename std::iterator_traits<ForwardIt>::difference_type n = 1) {
  std::advance(it, n);
  return it;
}

// 1. Look at the CFG edges, instead of the basic block terminator edges.
void Goto::execute() {
  auto &ASTCtx = FCtx.getASTContext();
  // for each block, insert goto statement to represent outgoing edges.

  for (auto it = CFG.begin(); it != CFG.end(); ++it) {
    auto Current = (*it);
    auto succ_size = Current->succ_size();
    // for unconditional branch
    if (succ_size == 1) {
      // if is the next block in the list
      // auto nextBlock = &(*next(it));
      // if (nextBlock == br->getSuccessor(0)) {
      // }
      auto Succ = Current->succ_begin();
      auto label = getBlockLabel(Succ->getBlock());
      auto Goto = createGotoStmt(label);
      Current->appendStmt(Goto);
    } else if (succ_size == 2) {
      auto tmp = Current->succ_begin();
      auto b1 = (*tmp).getBlock();
      ++tmp;
      auto b2 = (*tmp).getBlock();
      bool invert = false;

      // Make b1 the fallthrough block
      auto nextBlock = *std::next(it);
      // if false block == fall through, then can eliminate the goto
      if (nextBlock == b1) {
        std::swap(b1, b2);
        invert = true;
      }
      clang::Expr *cond = takeBinaryCond(*Current);
      if (invert) {
        cond = invertCond(cond);
      }
      auto GotoB1 = createGotoStmt(getBlockLabel(b1));
      auto IfGotoB1 = clang::IfStmt::Create(
          ASTCtx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
          nullptr, nullptr, cond, clang::SourceLocation(),
          clang::SourceLocation(), GotoB1, clang::SourceLocation());
      Current->appendStmt(IfGotoB1);
      auto GotoB2 = createGotoStmt(getBlockLabel(b2));
      Current->appendStmt(GotoB2);
    } else if (succ_size > 2) {
      // create SwitchStmt
      auto &term = std::get<SwitchTerminator>(Current->getTerminator());
      auto cond = llvm::cast<clang::Expr>(term.getStmt());
      auto sw = clang::SwitchStmt::Create(Ctx, nullptr, nullptr, cond,
                                          clang::SourceLocation(),
                                          clang::SourceLocation());
      std::vector<clang::Stmt *> stmts;
      stmts.reserve(succ_size);
      auto succIt = Current->succ_begin();

      // handle default case
      auto caseBlock = *succIt;
      auto goto_ = createGotoStmt(getBlockLabel(caseBlock));
      clang::DefaultStmt *DS = new (Ctx) clang::DefaultStmt(
          clang::SourceLocation(), clang::SourceLocation(), goto_);
      succIt++;

      // handle each case
      for (auto caseVal : term.cases()) {
        assert(succIt != Current->succ_end());
        auto succBlock = *succIt;
        auto CS = clang::CaseStmt::Create(Ctx, llvm::cast<clang::Expr>(caseVal),
                                          nullptr, clang::SourceLocation(),
                                          clang::SourceLocation(),
                                          clang::SourceLocation());
        CS->setSubStmt(createGotoStmt(getBlockLabel(succBlock)));
        sw->addSwitchCase(CS);
        stmts.push_back(CS);
        succIt++;
      }
      // add default as the lase case
      sw->addSwitchCase(DS);
      stmts.push_back(DS);
      auto body = clang::CompoundStmt::Create(
          Ctx, stmts, clang::SourceLocation(), clang::SourceLocation());
      sw->setBody(body);
      Current->appendStmt(sw);
    }
  }
  // merge all blocks into one. Remove all other blocks and edges.
  CFGBlock &Entry = CFG.getEntry();
  // because we only need one block, instead of maintaining all edges, we remove
  // all succs and preds of entry block, and ignore other blocks' edges.
  Entry.succ_clear();
  Entry.pred_clear();
  for (auto it = CFG.begin(); it != CFG.end(); ++it) {
    // push all statements into entry block
    auto Current = (*it);
    if (Current == &Entry) {
      continue;
    }

    // add if there is a label statement
    // we do not fold the label even if it contains a null stmt,
    // because we may delete the label stmt according to its usages.
    if (Current->getLabel() != nullptr) {
      Entry.appendStmt(Current->getLabel());
    }

    for (auto elem = Current->begin(); elem != Current->end(); ++elem) {
      Entry.appendElement(*elem);
    }
    if (Current->getTerminatorStmt() != nullptr) {
      Entry.appendStmt(Current->getTerminatorStmt());
    }
  }

  // remove other blocks from the cfg
  for (auto Current : std::vector<CFGBlock *>(CFG.begin(), CFG.end())) {
    if (Current == &Entry) {
      continue;
    }
    Current->succ_clear();
    Current->pred_clear();
    CFG.remove(Current);
  }

  simplifyBlock(Entry);
  // The exit block is set to the first stub block created. We currently do not
  // care about it.
}

// remote all NullStmt and merge GotoStmt with LabelStmt
void Goto::simplifyBlock(CFGBlock &Block) {
  // remove two adjacent goto + label
  for (auto it = Block.begin(); it != Block.end();) {
    if (auto stmt = getStmt(*it)) {
      if (auto gotoStmt = llvm::dyn_cast<clang::GotoStmt>(stmt)) {
        auto next = std::next(it);
        if (next != Block.end()) {
          if (auto label =
                  llvm::dyn_cast_or_null<clang::LabelStmt>(getStmt(*next))) {
            if (label->getDecl() == gotoStmt->getLabel()) {
              LLVM_DEBUG(llvm::dbgs() << "Removing redundant goto: "
                                      << label->getName() << "\n");
              FCtx.removeLabelUse(label, gotoStmt);
              it = Block.erase(it);
              continue;
            }
          }
        }
      }
    }
    // increment the iterator if not continue
    ++it;
  }

  // replace adjacent label with the last label
  for (auto it = Block.begin(); it != Block.end();) {
    if (auto stmt = getStmt(*it)) {
      // for each label stmt
      if ((llvm::isa<clang::LabelStmt>(stmt)) && std::next(it) != Block.end()) {
        clang::LabelStmt *label = llvm::cast<clang::LabelStmt>(stmt);
        if (auto nextLabel =
                llvm::dyn_cast<clang::LabelStmt>(getStmt(*std::next(it)))) {
          LLVM_DEBUG(llvm::dbgs()
                     << "Merge adjacent label: " << label->getName() << " and "
                     << nextLabel->getName() << "\n");

          assert(llvm::isa<clang::NullStmt>(label->getSubStmt()));
          assert(llvm::isa<clang::NullStmt>(nextLabel->getSubStmt()));
          replaceAllUsesWith(label->getDecl(), nextLabel->getDecl());
          it = Block.erase(it);
          continue;
        }
      }
    }
    // increment the iterator if not continue
    ++it;
  }

  // remove label without users, fold label with NullStmt and remove NullStmt
  for (auto it = Block.begin(); it != Block.end();) {
    if (auto stmt = getStmt(*it)) {
      if (auto label = llvm::dyn_cast<clang::LabelStmt>(stmt)) {
        if (FCtx.eraseLabelUseIfEmpty(label)) {
          LLVM_DEBUG(llvm::dbgs() << "Removing unused LabelStmt: "
                                  << label->getName() << "\n");
          it = Block.erase(it);
          continue;
        } else if (llvm::isa<clang::NullStmt>(label->getSubStmt()) &&
                   std::next(it) != Block.end()) {
          if (auto next = getStmt(*std::next(it))) {
            LLVM_DEBUG(llvm::dbgs()
                       << "Folding LabelStmt " << label->getName() << "\n");
            label->setSubStmt(next);
            Block.erase(std::next(it));
          }
        }
      }

      // remove NullStmt
      if (auto term = llvm::dyn_cast<clang::NullStmt>(stmt)) {
        LLVM_DEBUG(llvm::dbgs() << "Removing NullStmt\n");
        it = Block.erase(it);
        continue;
      }
    }
    // increment the iterator if not continue
    ++it;
  }
}

} // namespace notdec::llvm2c
