#ifndef _NOTDEC_BACKEND_PHOENIX_H_
#define _NOTDEC_BACKEND_PHOENIX_H_

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/Dominators.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include <utility>
#include <vector>

namespace notdec::llvm2c {

class Phoenix : IStructuralAnalysis {
public:
  const int StmtExprThreshold = 4;
  enum LoopType {
    While,
    DoWhile,
  };
  enum VirtualEdgeType {
    Goto,
    Break,
    Continue,
  };

  /// Represent an edge that is going to be eliminated.
  /// However, it can represent multiple edges in switch statement, because
  /// multiple edges have the same From and To.
  struct VirtualEdge {
    CFGBlock *From;
    CFGBlock *To;
    VirtualEdgeType Ty;
    VirtualEdge(CFGBlock *From, CFGBlock *To, VirtualEdgeType Ty)
        : From(From), To(To), Ty(Ty) {}
    // compare for order.
    bool operator<(const VirtualEdge &other) const {
      if (From == other.From)
        return To < other.To;
      return (From < other.From);
    }
  };

protected:
  bool isCanceled = false;
  CFGDomTree Dom;
  std::vector<CFGBlock *> unresolvedSwitches;
  std::queue<std::pair<CFGBlock *, std::set<CFGBlock *>>> unresolvedCycles;
  bool ReduceAcyclic(CFGBlock *Block, bool UseTail = true);
  bool isCyclic(CFGBlock *Block);
  bool ReduceCyclic(CFGBlock *Block);
  bool reduceSequence(CFGBlock *Block);
  bool reduceIfRegion(CFGBlock *Block, bool UseTail = true);
  bool reduceSwitchRegion(CFGBlock *Block);
  bool ProcessUnresolvedRegions();
  bool refineLoop(CFGBlock *head, std::set<CFGBlock *> &loopNodes);
  void refineIncSwitch(CFGBlock *switchHead);
  bool virtualizeReturn(CFGBlock *B);
  bool coalesceTailRegion(CFGBlock *n, std::set<CFGBlock *> &lexicalNodes);
  bool lastResort(CFGBlock *B);
  bool lastResort(std::set<CFGBlock *> &n);
  std::set<CFGBlock *> getLexicalNodes(CFGBlock *head, CFGBlock *follow,
                                       std::set<CFGBlock *> &loopNodes);
  bool isBackEdge(CFGBlock *A, CFGBlock *B);
  std::pair<CFGBlock *, CFGBlock *>
  determineFollowLatch(CFGBlock *head, std::set<CFGBlock *> &loopNodes);
  bool virtualizeEdge(const VirtualEdge &edge);
  bool collapseToTailRegion(CFGBlock *From, CFGBlock *To, clang::Stmt *stm);
  bool virtualizeIrregularExits(CFGBlock *head, CFGBlock *latch,
                                CFGBlock *follow,
                                std::set<CFGBlock *> &lexicalNodes);
  bool virtualizeIrregularSwitchEntries(CFGBlock *n);
  CFGBlock *findIrregularSwitchFollowRegion(CFGBlock *n);
  std::map<CFGBlock *, std::set<CFGBlock *>> findSwitchBody(CFGBlock *n,
                                                            CFGBlock *follow);
  bool
  virtualizeIrregularSwitchExits(CFGBlock *head,
                                 std::map<CFGBlock *, std::set<CFGBlock *>> map,
                                 CFGBlock *follow);
  bool virtualizeIrregularCaseExits(CFGBlock *head, CFGBlock *caseEntry,
                                    const std::set<CFGBlock *> &caseBody,
                                    CFGBlock *follow);
  llvm::Optional<Phoenix::VirtualEdge>
  findLastResortEdge(std::set<CFGBlock *> &blocks);
  CFGBlock *getSwitchFollow(CFGBlock *n);
  bool reduceIncSwitch(CFGBlock *n, CFGBlock *follow);

  clang::Expr *createBlockCondExpr(CFGBlock *Block, clang::Expr *Cond);

public:
  Phoenix(SAFuncContext &Ctx) : IStructuralAnalysis(Ctx) {}

  void execute() override;
};

} // namespace notdec::llvm2c

#endif
