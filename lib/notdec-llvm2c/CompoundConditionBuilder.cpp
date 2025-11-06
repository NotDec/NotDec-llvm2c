#include <clang/AST/OperationKinds.h>
#include <clang/Basic/Specifiers.h>

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/CompoundConditionBuilder.h"
#include "notdec-llvm2c/PostOrderCFGView.h"
#include "notdec-llvm2c/Utils.h"

namespace notdec::llvm2c {

void CompoundConditionBuilder::execute() {
  bool changed = false;
  do {
    changed = false;
    auto postView = PostOrderCFGView::create(&CFG);

    for (auto block : *postView) {
      if (toRemove.count(const_cast<CFGBlock *>(block))) {
        continue;
      }
      if (block->succ_size() == 2) {
        changed |= maybeCoalesce(const_cast<CFGBlock *>(block));
      }
    }
    doRemoveBlocks();
  } while (changed);
}

/// returns if the block has only condition expr and only one pred
bool isSubordinate(CFGBlock *block) {
  return block->pred_size() == 1 && block->size() == 0 &&
         block->succ_size() == 2;
}

void CompoundConditionBuilder::rebuildGraph(CFGBlock *head, CFGBlock *redundant,
                                            CFGBlock *replacement,
                                            CFGBlock *common) {
  removeAllEdge(redundant, replacement);
  removeAllEdge(redundant, common);
  replaceAllSucc(head, redundant, replacement);
  deferredRemove(redundant);
  CFG.sanityCheck();
}

bool CompoundConditionBuilder::maybeCoalesce(CFGBlock *Head) {
  bool changed = false;
  auto ss = Head->getTwoSuccs();
  auto BThen = ss.first;
  auto BElse = ss.second;
  if (isSubordinate(BThen)) {
    auto ss2 = BThen->getTwoSuccs();
    if (ss2.second == BElse) {
      // fold to X && Y
      //  ┌─── Head───┐
      //  ▼ F       T │
      // Else         │
      //  │     T     ▼
      //  ├───────► Then
      // F│          │
      //  ▼          │
      // Follow ◄────┘
      changed = true;
      auto condX = takeBinaryCond(*Head);
      auto condY = takeBinaryCond(*BThen);
      auto result = createBinaryOperator(Ctx, condX, condY, clang::BO_LAnd,
                                         condX->getType(), clang::VK_PRValue);
      Head->setTerminator(result);
      rebuildGraph(Head, BThen, ss2.first, ss2.second);
    } else if (ss2.first == BElse) {
      // fold to X && !Y
      changed = true;
      auto condX = takeBinaryCond(*Head);
      auto condY = takeBinaryCond(*BThen);
      condY = invertCond(condY);
      auto result = createBinaryOperator(Ctx, condX, condY, clang::BO_LAnd,
                                         condX->getType(), clang::VK_PRValue);
      Head->setTerminator(result);
      rebuildGraph(Head, BThen, ss2.second, ss2.first);
    }
  } else if (isSubordinate(BElse)) {
    auto ss2 = BElse->getTwoSuccs();
    if (ss2.first == BThen) {
      // fold to X || Y
      changed = true;
      auto condX = takeBinaryCond(*Head);
      auto condY = takeBinaryCond(*BElse);
      auto result = createBinaryOperator(Ctx, condX, condY, clang::BO_LOr,
                                         condX->getType(), clang::VK_PRValue);
      Head->setTerminator(result);
      rebuildGraph(Head, BElse, ss2.second, ss2.first);
    } else if (ss2.second == BThen) {
      // fold to X || !Y
      changed = true;
      auto condX = takeBinaryCond(*Head);
      auto condY = takeBinaryCond(*BElse);
      condY = invertCond(condY);
      auto result = createBinaryOperator(Ctx, condX, condY, clang::BO_LOr,
                                         condX->getType(), clang::VK_PRValue);
      Head->setTerminator(result);
      rebuildGraph(Head, BElse, ss2.first, ss2.second);
    }
  }
  return changed;
}

} // namespace notdec::llvm2c
