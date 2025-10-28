// Phoenix: Native x86 Decompilation Using Semantics-Preserving Structural
// Analysis and Iterative Control-Flow Structuring

#include <cassert>
#include <iostream>
#include <llvm/Support/Casting.h>
#include <memory>
#include <queue>
#include <set>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/Optional.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Stmt.h>
#include <clang/Basic/SourceLocation.h>

#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/Dominators.h"
#include "notdec-llvm2c/Phoenix.h"
#include "notdec-llvm2c/PostOrderCFGView.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include "notdec-llvm2c/Utils.h"

#define DEBUG_TYPE "structural-phoenix"

namespace notdec::llvm2c {

bool isComplexStmt(clang::Stmt *St) {
  if (llvm::isa<CompoundStmt, DoStmt, IfStmt, ForStmt>(St)) {
    return true;
  }
  return false;
}

void Phoenix::execute() {
  int iterations = 0;
  do {
    if (isCanceled) {
      break;
    }
    ++iterations;
    if (iterations > 1000) {
      llvm::errs() << "Structure analysis stopped making progress, quitting."
                   << " Func: " << FCtx.getFunction().getName() << "\n";
      break;
    }

    // this will maintain a list, so modification during iteration is OK
    auto postView = PostOrderCFGView::create(&CFG);
    Dom.buildDominatorTree(&CFG);
    // equivalent of unresolvedCycles.clear()
    std::queue<std::pair<CFGBlock *, std::set<CFGBlock *>>>().swap(
        unresolvedCycles);
    unresolvedSwitches.clear();
    // Dom.dump();

    for (const CFGBlock *CBlock : *postView) {
      CFGBlock *Block = const_cast<CFGBlock *>(CBlock);
      if (toRemove.count(Block)) {
        continue;
      }

      bool Changed;
      // int round = 0;
      do {
        Changed = false;
        Changed |= ReduceAcyclic(Block, false);
        if (!Changed && isCyclic(Block)) {
          Changed |= ReduceCyclic(Block);
        }
        Changed |= ReduceAcyclic(Block, true);
        // round += 1;
      } while (Changed);
    }
    if (toRemove.empty() && CFG.size() > 1) {
      // Didn't make any progress this round,
      // try refining unstructured regions
      ProcessUnresolvedRegions();
    }
    if (!toRemove.empty()) {
      iterations = 0;
    }
    doRemoveBlocks();
  } while (CFG.size() > 1);
}

bool Phoenix::isBackEdge(CFGBlock *A, CFGBlock *B) {
  return Dom.properlyDominates(B, A);
}

bool Phoenix::isCyclic(CFGBlock *Block) {
  for (auto &pred : Block->preds()) {
    // 1 self loop
    if (pred.getBlock() == Block) {
      return true;
    }
    // 2 back edge: edge target dominates source
    if (isBackEdge(pred.getBlock(), Block)) {
      return true;
    }
  }
  return false;
}

void backwardVisit(std::set<CFGBlock *> &visited, CFGBlock *Head,
                   CFGBlock *Current) {
  // can include loop head
  assert(visited.insert(Current).second);
  if (Current == Head) {
    return;
  }
  for (auto &pred : Current->preds()) {
    auto P = pred.getBlock();
    if (visited.find(P) == visited.end()) {
      backwardVisit(visited, Head, P);
    }
  }
}

/// Visits given node and recursively visits all its GRAY successors.
/// All the visited nodes are painted Black.
void forwardVisit(std::set<CFGBlock *> &visited, std::set<CFGBlock *> &gray,
                  CFGBlock *Current) {
  assert(gray.count(Current) == 1);
  assert(visited.insert(Current).second);
  for (auto &pred : Current->succs()) {
    auto P = pred.getBlock();
    if (visited.find(P) == visited.end() && gray.count(P) == 1) {
      forwardVisit(visited, gray, P);
    }
  }
}

/// This class finds all nodes on the paths from a given node to itself
/// ending with a back edge to the given node. All the discovered nodes
/// are expected to belong to the loop with the given node being its entry.
std::unique_ptr<std::set<CFGBlock *>> getLoopNodes(CFGBlock *Block,
                                                   CFGDomTree &Dom) {
  std::set<CFGBlock *> Gray;
  std::set<CFGBlock *> Black;
  std::unique_ptr<std::set<CFGBlock *>> ret =
      std::make_unique<std::set<CFGBlock *>>();
  // Find all nodes that can reach the back-edge predecessors without passing
  // loop head and paint them gray.
  for (auto &pred : Block->preds()) {
    auto P = pred.getBlock();
    if (Dom.properlyDominates(Block, P)) {
      // backward visit from back edge node
      // because Block dom P, back visit from P will find Block eventually.
      if (Gray.count(P) == 0) {
        backwardVisit(Gray, Block, P);
      }
    } else if (P == Block) {
      // self loop
      ret->insert(P);
    }
  }

  // Find all gray nodes that can be visited from the loop entry and color them
  // black. Black nodes belong to the loop.
  if (Gray.find(Block) != Gray.end()) {
    forwardVisit(Black, Gray, Block);
  }
  ret->insert(Black.begin(), Black.end());
  return ret;
}

clang::Expr *Phoenix::createBlockCondExpr(CFGBlock *Block, clang::Expr *Cond) {
  if (Block->size() == 0) {
    return Cond;
  }
  if (Block->size() < StmtExprThreshold) {
    bool hasNoCompound = true;
    for (auto Elem = Block->begin(); Elem != Block->end(); ++Elem) {
      // TODO what if there are other kinds of CFGElement
      if (auto Stmt = getStmt(*Elem)) {
        if (isComplexStmt(Stmt)) {
          hasNoCompound = false;
        }
      }
    }
    if (hasNoCompound) {
      auto CS = llvm::cast<CompoundStmt>(makeCompoundStmt(Block, Cond));
      auto Ret =
          new (Ctx) StmtExpr(CS, Cond->getType(), clang::SourceLocation(),
                             clang::SourceLocation(), 0);
      return Ret;
    }
  }
  return nullptr;
}

bool Phoenix::ReduceCyclic(CFGBlock *Block) {
  bool changed = false;
  std::unique_ptr<std::set<CFGBlock *>> loopNodes = getLoopNodes(Block, Dom);
  while (true) {
    if (Block->succ_size() != 1 || !reduceSequence(Block)) {
      break;
    }
    changed = true;
  }
  if (Block->succ_size() > 2) {
    return changed;
  }
  // handle any self loop edges, and convert to while loop
  for (auto &succ : Block->succs()) {
    auto S = succ.getBlock();
    if (S == Block) {
      clang::Stmt *whil;
      if (Block->succ_size() == 1) {
        // infinite loop
        // create while(true){stmts}
        auto body = makeCompoundStmt(Block);
        whil = createWhileTrue(body);
      } else {
        assert(Block->succ_size() == 2);
        // Do while loop
        auto cond = takeBinaryCond(*Block);
        auto body = makeCompoundStmt(Block);
        // invert if loop is the false branch
        if (Block->isFalseBrSucc(S)) {
          cond = invertCond(cond);
        }
        // create do while stmt
        whil = new (Ctx)
            clang::DoStmt(body, cond, clang::SourceLocation(),
                          clang::SourceLocation(), clang::SourceLocation());
      }
      // organize edges: now it has only one successor.
      Block->appendStmt(whil);
      removeEdge(Block, Block);
      return true;
    }
  }
  // Should be condition. Switches should not match a cyclic pattern
  if (Block->succ_size() > 2) {
    return changed;
  }
  for (auto &succ : Block->succs()) {
    auto S = succ.getBlock();
    // While loop: Block -> S -> Block, and S has no other succ or pred
    if (linearSuccessor(S) == Block && singlePredecessor(S) == Block) {
      auto cond = takeBinaryCond(*Block);
      // invert cond if the enter the
      if (Block->isFalseBrSucc(S)) {
        cond = invertCond(cond);
      }
      clang::Stmt *whil;
      if (auto CondStmtExpr = createBlockCondExpr(Block, cond)) {
        // S is the only body block
        auto body = makeCompoundStmt(S);
        whil = clang::WhileStmt::Create(
            Ctx, nullptr, CondStmtExpr, body, clang::SourceLocation(),
            clang::SourceLocation(), clang::SourceLocation());
      } else {
        // Wrap in a big while true, and use break to leave
        // while (true) {Block; if(cond) break; S;}
        std::vector<clang::Stmt *> stmts;
        addAllStmtTo(Block, stmts);
        Block->clear();
        // add a if-break stmt.
        auto if_break = clang::IfStmt::Create(
            Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
            nullptr, nullptr, cond, clang::SourceLocation(),
            clang::SourceLocation(),
            new (Ctx) clang::BreakStmt(clang::SourceLocation()));
        stmts.push_back(if_break);
        // add stmts in the S block
        addAllStmtTo(S, stmts);
        // create the while true
        auto body = clang::CompoundStmt::Create(
            Ctx, stmts, clang::SourceLocation(), clang::SourceLocation());
        whil = createWhileTrue(body);
      }
      // now the while is the only stmt in Block
      Block->appendStmt(whil);
      removeEdge(Block, S);
      removeEdge(S, Block);
      deferredRemove(S);
      return true;
    } // end Block -> S -> Block
  }
  // It's a cyclic region, but we are unable to collapse it.
  // Schedule it for refinement after the whole graph has been
  // traversed.
  // enqueueUnresolvedLoop(Block, loopNodes);
  // Do not refine cycle if there are unresolved switches
  if (unresolvedSwitches.size() == 0) {
    unresolvedCycles.emplace(Block, std::move(*loopNodes));
  }
  return changed;
}

// count the number of preds of block, that is not in the loopNodes.
int countIncomingEdges(CFGBlock *block, std::set<CFGBlock *> &loopNodes) {
  int count = 0;
  for (auto &pred : block->preds()) {
    // not in the set
    if (loopNodes.count(pred.getBlock()) == 0) {
      count += 1;
    }
  }
  return count;
}

/// Ensure the loop has a single entrance (nodes with
/// incoming edges from outside the loop).  If there are
/// multiple entrances to the loop, we select the one with
/// the most incoming edges, and virtualize the other
/// entrance edges.
/// Currently the virtualization part is not implemented.
/// TODO any negative impact?
CFGBlock *ensureSingleEntry(CFGBlock *head, std::set<CFGBlock *> &loopNodes) {
  // first ensure there is only one entry
  auto cinMax = countIncomingEdges(head, loopNodes);
  CFGBlock *headMax = head;
  for (auto n : loopNodes) {
    auto cin = countIncomingEdges(n, loopNodes);
    if (cin > cinMax) {
      cinMax = cin;
      headMax = n;
    }
  }
  return headMax;
}

/// get blocks reachable from n, not via head.
/// blocks contains the visited result.
void findReachableBlocks(CFGBlock *n, CFGBlock *head,
                         std::set<CFGBlock *> &blocks) {
  blocks.insert(n);
  for (auto &succ : n->succs()) {
    if (blocks.count(succ.getBlock()) == 0 && succ.getBlock() != head) {
      findReachableBlocks(succ.getBlock(), head, blocks);
    }
  }
}

/// in Section 3.6, the updated version of lexical nodes.
/// original lexical nodes(i.e. body of the loop):
/// - 一个反向边的Natural Loop（即，natural loop严格说并不是一个单独的概念，
/// 反而是对一条反向边而言的。），是指，最小的，包括反向边头和尾节点的节点集合，
/// 集合内所有节点的前驱，要么在集合内，要么这个节点是entry节点，前驱是entry的前驱。
///
/// new additional lexical nodes: (additional nodes besides the above nodes.)
/// - dominated by the loop header, excluding any nodes reachable from the
/// loop’s successor without going through the loop header
std::set<CFGBlock *> Phoenix::getLexicalNodes(CFGBlock *head, CFGBlock *follow,
                                              std::set<CFGBlock *> &loopNodes) {
  std::set<CFGBlock *> excluded;
  // find reachable nodes, starting from follow, and without passing through
  // head node. these nodes are excluded.
  findReachableBlocks(follow, head, excluded);
  std::set<CFGBlock *> lexNodes;
  // starting from loopNodes.
  std::set<CFGBlock *> pushed;
  std::queue<CFGBlock *> queue;
  for (auto node : loopNodes) {
    pushed.insert(node);
    queue.push(node);
  }
  while (!queue.empty()) {
    CFGBlock *fr = queue.front();
    queue.pop();
    if (loopNodes.count(fr)) {
      // if in loopNodes, insert and push succs to queue.
      lexNodes.insert(fr);
      for (auto &succ : fr->succs()) {
        auto succBlock = succ.getBlock();
        if (lexNodes.count(succBlock) == 0 && !pushed.count(succ.getBlock())) {
          pushed.insert(succBlock);
          queue.push(succBlock);
        }
      }
    } else if (Dom.properlyDominates(head, fr) && (excluded.count(fr) == 0)) {
      // if dominated by head, and not in exclude list
      lexNodes.insert(fr);
      for (auto &succ : fr->succs()) {
        auto succBlock = succ.getBlock();
        if (lexNodes.count(succBlock) == 0 && !pushed.count(succBlock)) {
          pushed.insert(succBlock);
          queue.push(succ.getBlock());
        }
      }
    }
  }
  return lexNodes;
}

std::pair<CFGBlock *, CFGBlock *>
Phoenix::determineFollowLatch(CFGBlock *head, std::set<CFGBlock *> &loopNodes) {
  if (head->succ_size() == 2) {
    auto succs = head->getTwoSuccs();
    // If the head is a Conditional node and one of the edges
    // leaves the loop, the head of that edge is the follow
    // node of the loop.
    CFGBlock *follow = nullptr;
    if (loopNodes.count(succs.first) == 0) {
      follow = succs.first;
    } else if (loopNodes.count(succs.second) == 0) {
      follow = succs.second;
    }
    if (follow != nullptr) {
      for (auto &latch : head->succs()) {
        if (isBackEdge(latch, head) && linearSuccessor(latch) == head) {
          return std::make_pair(follow, latch);
        }
      }
    }
  }
  // find back edges: latch -> head, and check other succ of latch that is not
  // in the loop.
  for (auto &latch : head->preds()) {
    if (isBackEdge(latch.getBlock(), head)) {
      if (latch->succ_size() == 2) {
        auto succs = latch->getTwoSuccs();
        if (loopNodes.count(succs.first) == 0) {
          return std::make_pair(succs.first, latch);
        }
        if (loopNodes.count(succs.second) == 0) {
          return std::make_pair(succs.second, latch);
        }
      }
    }
  }
  return std::make_pair(nullptr, nullptr);
}

// has any edge from N -> follow
bool hasExitEdgeFrom(CFGBlock *N, CFGBlock *Follow) {
  for (auto &succ : N->succs()) {
    if (succ.getBlock() == Follow) {
      return true;
    }
  }
  return false;
}

Phoenix::LoopType determineLoopType(CFGBlock *head, CFGBlock *latch,
                                    CFGBlock *follow) {
  if (!hasExitEdgeFrom(latch, follow)) {
    return Phoenix::While;
  }
  if (!hasExitEdgeFrom(head, follow)) {
    return Phoenix::DoWhile;
  }
  if (head->size() > 0) {
    return Phoenix::DoWhile;
  }
  return Phoenix::While;
}

bool isPureReturn(CFGBlock *B) {
  if (B->size() != 1) {
    return false;
  }
  auto stmt = B->front().castAs<CFGStmt>().getStmt();
  return llvm::isa<clang::ReturnStmt>(stmt);
}

void Phoenix::collapseToTailRegion(CFGBlock *From, CFGBlock *To,
                                   clang::Stmt *stm) {
  switch (From->succ_size()) {
  case 2: {
    auto cond = takeBinaryCond(*From);
    if (From->isFalseBrSucc(To)) {
      cond = invertCond(cond);
    }
    auto IfStmt = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary, nullptr,
        nullptr, cond, clang::SourceLocation(), clang::SourceLocation(), stm);
    From->appendStmt(IfStmt);
  } break;
  case 1:
    From->appendStmt(stm);
    break;
  case 0:
    llvm::errs() << "======== dumping CFG due to an error =============\n";
    CFG.dump(Ctx.getLangOpts(), FCtx.getOpts().enableColor);
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "Error: Can't collapse edge! function: "
                 << FCtx.getFunction().getName()
                 << ", edge: " << From->getBlockID() << " -> "
                 << To->getBlockID() << "\n";
    std::abort();
  default:
    // Switch statement may have multiple edge with the same from and to.
    // Handle them all.
    assert(std::holds_alternative<SwitchTerminator>(From->getTerminator()));
    // To fold a switch branch, we need to create a new block
    auto newBlock = CFG.createBlock();
    FCtx.mapRedirectBlock(**newBlock, *To);
    (*newBlock)->appendStmt(stm);
    From->replaceAllSucc(To, *newBlock);
    (*newBlock)->addPred(From);
    To->removePred(From);
    break;
  }
  removeEdge(From, To);
}

void Phoenix::virtualizeEdge(const Phoenix::VirtualEdge &Edge) {
  clang::Stmt *Stmt;
  auto To = Edge.To;
  if (isPureReturn(To)) {
    assert(To->succ_size() == 0);
    // Goto to a return statement => just a return statement.
    auto ret = (clang::ReturnStmt *)To->front().castAs<CFGStmt>().getStmt();
    Stmt = clang::ReturnStmt::Create(Ctx, clang::SourceLocation(),
                                     ret->getRetValue(), nullptr);
  } else {
    switch (Edge.Ty) {
    case Break:
      Stmt = new (Ctx) clang::BreakStmt(clang::SourceLocation());
      break;
    case Continue:
      Stmt = new (Ctx) clang::ContinueStmt(clang::SourceLocation());
      break;
    case Goto: {
      auto Label = getBlockLabel(To);
      Stmt = new (Ctx) clang::GotoStmt(Label, clang::SourceLocation(),
                                       clang::SourceLocation());
      break;
    }
    default:
      std::cerr << __FILE__ << ":" << __LINE__ << ": "
                << "Error: unhandled enum of Phoenix::VirtialEdge" << std::endl;

      std::abort();
    }
  }
  collapseToTailRegion(Edge.From, To, Stmt);
  if (To->pred_size() == 0 && To != &CFG.getEntry()) {
    if (isPureReturn(To) && (To->getLabel() == nullptr)) {
      // previous pure return detection has put the return to the From block.
      To->clear();
      deferredRemove(To);
    } else {
      // CFG.dump(Ctx.getLangOpts(), FCtx.getOpts().enableColor);
      std::cerr << __FILE__ << ":" << __LINE__ << ": "
                << "Warning: Removing edge (" << Edge.From->getBlockID() << ", "
                << To->getBlockID() << ") caused loss of some code blocks"
                << std::endl;
      // std::abort();
    }
  }
}

bool Phoenix::virtualizeIrregularExits(CFGBlock *head, CFGBlock *latch,
                                       CFGBlock *follow,
                                       std::set<CFGBlock *> &lexicalNodes) {
  bool changed = false;
  Phoenix::LoopType loopType = determineLoopType(head, latch, follow);
  std::vector<Phoenix::VirtualEdge> vEdges;
  for (auto n : lexicalNodes) {
    vEdges.clear();
    for (auto &s : n->succs()) {
      if (s == head) {
        if (n != latch) {
          vEdges.emplace_back(n, s, Phoenix::VirtualEdgeType::Continue);
        }
      } else if (lexicalNodes.count(s) == 0) {
        if (s == follow) {
          if ((loopType == Phoenix::DoWhile && n != latch) ||
              (loopType == Phoenix::While && n != head)) {
            vEdges.emplace_back(n, s, Phoenix::VirtualEdgeType::Break);
          }
        } else {
          vEdges.emplace_back(n, s, Phoenix::VirtualEdgeType::Goto);
        }
      }
    }
    for (auto &edge : vEdges) {
      changed = true;
      virtualizeEdge(edge);
    }
  }
  return changed;
}

bool Phoenix::coalesceTailRegion(CFGBlock *n, std::set<CFGBlock *> &range) {
  if (n->succ_size() == 2) {
    auto ss = n->getTwoSuccs();
    auto th = ss.first;
    auto el = ss.second;

    if (el->succ_size() == 0 && th->succ_size() == 0 &&
        singlePredecessor(el) == n && singlePredecessor(th) == n) {
      auto cond = takeBinaryCond(*n);
      auto then = makeCompoundStmt(th);
      auto els = makeCompoundStmt(el);
      auto IfStmt = clang::IfStmt::Create(
          Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
          nullptr, nullptr, cond, clang::SourceLocation(),
          clang::SourceLocation(), then, clang::SourceLocation(), els);
      n->appendStmt(IfStmt);
      removeEdge(n, th);
      removeEdge(n, el);
      deferredRemove(th);
      deferredRemove(el);
      return true;
    }
    if (range.count(el) != 0 && el->succ_size() == 0 &&
        singlePredecessor(el) == n) {
      auto cond = takeBinaryCond(*n);
      cond = invertCond(cond);
      auto body = makeCompoundStmt(el);
      auto ifStmt = clang::IfStmt::Create(
          Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
          nullptr, nullptr, cond, clang::SourceLocation(),
          clang::SourceLocation(), body, clang::SourceLocation());
      n->appendStmt(ifStmt);
      removeEdge(n, el);
      deferredRemove(el);
      return true;
    };
    if (range.count(th) != 0 && th->succ_size() == 0 &&
        singlePredecessor(th) == n) {
      auto cond = takeBinaryCond(*n);
      auto body = makeCompoundStmt(th);
      auto ifStmt = clang::IfStmt::Create(
          Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
          nullptr, nullptr, cond, clang::SourceLocation(),
          clang::SourceLocation(), body, clang::SourceLocation());
      n->appendStmt(ifStmt);
      removeEdge(n, th);
      deferredRemove(th);
      return true;
    }
    // TODO?
  }
  return false;
}

bool Phoenix::refineLoop(CFGBlock *head, std::set<CFGBlock *> &loopNodes) {
  // find the single entry.
  head = ensureSingleEntry(head, loopNodes);
  // back edge: latch -> head. follow node is the node after the loop.
  auto follow_latch = determineFollowLatch(head, loopNodes);
  auto follow = follow_latch.first;
  auto latch = follow_latch.second;
  if (follow == nullptr && latch == nullptr) {
    return false;
  }
  auto lexicalNodes = getLexicalNodes(head, follow, loopNodes);
  auto virtualized =
      virtualizeIrregularExits(head, latch, follow, lexicalNodes);
  if (virtualized) {
    return true;
  }
  for (auto n : lexicalNodes) {
    if (coalesceTailRegion(n, lexicalNodes)) {
      return true;
    }
  }
  return lastResort(lexicalNodes);
}

llvm::Optional<Phoenix::VirtualEdge>
Phoenix::findLastResortEdge(std::set<CFGBlock *> &blocks) {
  std::vector<Phoenix::VirtualEdge> edges;
  // get all in region edges.
  for (auto n : blocks) {
    for (auto &s : n->succs()) {
      if (blocks.count(s) != 0) {
        edges.emplace_back(n, s.getBlock(), Phoenix::VirtualEdgeType::Goto);
      }
    }
  }
  for (auto &vEdge : edges) {
    if (!Dom.properlyDominates(vEdge.From, vEdge.To) &&
        !Dom.properlyDominates(vEdge.To, vEdge.From)) {
      return vEdge;
    }
  }
  for (auto &vEdge : edges) {
    if (!Dom.properlyDominates(vEdge.From, vEdge.To)) {
      return vEdge;
    }
  }
  if (edges.empty()) {
    return llvm::None;
  } else {
    return edges.front();
  }
}

bool Phoenix::lastResort(std::set<CFGBlock *> &blocks) {
  auto vEdge = findLastResortEdge(blocks);
  if (vEdge.hasValue()) {
    virtualizeEdge(*vEdge);
    return true;
  } else {
    // Whoa, we're in trouble now....
    return false;
  }
}

void Phoenix::refineIncSwitch(CFGBlock *n) {
  // virtualize all entries that makes a case having multiple preds.
  if (virtualizeIrregularSwitchEntries(n)) {
    return;
  }

  auto follow = findIrregularSwitchFollowRegion(n);
  std::map<CFGBlock *, std::set<CFGBlock *>> switchBody =
      findSwitchBody(n, follow);
  if (virtualizeIrregularSwitchExits(switchBody, follow)) {
    return;
  }

  std::set<CFGBlock *> switchNodes;
  for (auto ent : switchBody) {
    for (auto n : ent.second) {
      switchNodes.insert(n);
    }
  }

  for (auto node : switchNodes) {
    if (coalesceTailRegion(node, switchNodes)) {
      return;
    }
  }
  lastResort(switchNodes);
}

bool Phoenix::virtualizeIrregularSwitchExits(
    std::map<CFGBlock *, std::set<CFGBlock *>> switchBody, CFGBlock *follow) {
  for (auto &caseBody : switchBody) {
    if (virtualizeIrregularCaseExits(follow, caseBody.second)) {
      return true;
    }
  }
  return false;
}

bool Phoenix::virtualizeIrregularCaseExits(
    CFGBlock *follow, const std::set<CFGBlock *> &caseBody) {
  bool virtualized = false;
  std::vector<VirtualEdge> VEdges;
  for (auto n : caseBody) {
    for (auto &s : n->succs()) {
      // find leaving nodes.
      if (caseBody.count(s) == 0 && s != follow) {
        VEdges.emplace_back(n, s, VirtualEdgeType::Goto);
      }
    }
  }
  for (auto &vEdge : VEdges) {
    virtualizeEdge(vEdge);
    virtualized = true;
  }
  return virtualized;
}

std::map<CFGBlock *, std::set<CFGBlock *>>
Phoenix::findSwitchBody(CFGBlock *n, CFGBlock *follow) {
  std::map<CFGBlock *, std::set<CFGBlock *>> caseNodesMap;
  for (auto &C : n->succs()) {
    auto &caseNodes = caseNodesMap[C];
    caseNodes.insert(C);
    getLexicalNodes(C.getBlock(), follow, caseNodes);
  }
  return caseNodesMap;
}

// To find the successor, we first identify the immediate post-dominator of the
// switch head. If this node is the successor of any of the case nodes, we
// select it as the switch successor. If not, we select the node that (1) is a
// successor of a case node, (2) is not a case node itself, and (3) has the
// highest number of incoming edges from case nodes.
CFGBlock *Phoenix::findIrregularSwitchFollowRegion(CFGBlock *n) {
  CFGPostDomTree PostDom;
  PostDom.buildDominatorTree(&CFG);

  auto immPDom = PostDom.getImmediateDominator(n);
  for (auto succ : n->succs()) {
    if (succ.getBlock() == immPDom) {
      return immPDom;
    }
  }
  // collect all successors of all case nodes.
  // find the one with the most incoming edges from case nodes.
  int max_score = 0;
  CFGBlock *follow = nullptr;
  std::set<CFGBlock *> caseNodes;

  for (auto &S : n->succs()) {
    caseNodes.insert(S);
  }

  for (auto C : caseNodes) {
    for (auto &CS : C->succs()) {
      if (caseNodes.count(CS.getBlock()) == 0) {
        int score = 0;
        for (auto &P : CS->preds()) {
          if (caseNodes.count(P.getBlock()) != 0) {
            score += 1;
          }
        }
        if (score > max_score) {
          max_score = score;
          follow = CS.getBlock();
        }
      }
    }
  }
  return follow;
}

/// Find all irregular switch entries and virtualize them.
/// Returns True if one or more irregular entry was virtualized.
/// this opens up the possibility of further refinements.
bool Phoenix::virtualizeIrregularSwitchEntries(CFGBlock *n) {
  std::set<VirtualEdge> VEdges;
  for (auto &S : n->succs()) {
    // find all entries that are not the switch head.

    // some block that dominates the switch cannot be within the switch.
    if (Dom.dominates(S, n)) {
      VEdges.emplace(n, S, VirtualEdgeType::Goto);
      continue;
    }
    for (auto &SP : S->preds()) {
      if (SP != n) {
        VEdges.emplace(SP, S, VirtualEdgeType::Goto);
      }
    }
  }
  if (VEdges.size() == 0) {
    return false;
  }
  for (auto &vEdge : VEdges) {
    virtualizeEdge(vEdge);
  }
  return true;
}

bool Phoenix::virtualizeReturn(CFGBlock *n) {
  llvm::Optional<Phoenix::VirtualEdge> returnEdge;
  for (auto &s : n->succs()) {
    if (n->succ_size() <= 2 && isPureReturn(s)) {
      returnEdge.emplace(n, s, VirtualEdgeType::Goto);
      break;
    }
  }
  if (returnEdge.hasValue()) {
    virtualizeEdge(*returnEdge);
    return true;
  }
  return false;
}

bool Phoenix::ProcessUnresolvedRegions() {
  if (unresolvedCycles.size() > 0) {
    auto cycle = unresolvedCycles.front();
    bool changed = refineLoop(cycle.first, cycle.second);
    unresolvedCycles.pop();
    if (changed) {
      return true;
    }
  }
  if (unresolvedSwitches.size() > 0) {
    auto switchHead = *unresolvedSwitches.erase(unresolvedSwitches.begin());
    refineIncSwitch(switchHead);
    return true;
  }
  auto postView = PostOrderCFGView::create(&CFG);
  for (const CFGBlock *CBlock : *postView) {
    CFGBlock *Block = const_cast<CFGBlock *>(CBlock);
    if (virtualizeReturn(Block)) {
      return true;
    }
  }
  std::set<CFGBlock *> nodes(CFG.nodes_begin(), CFG.nodes_end());
  for (const CFGBlock *CBlock : *postView) {
    CFGBlock *Block = const_cast<CFGBlock *>(CBlock);
    if (coalesceTailRegion(Block, nodes)) {
      return true;
    }
  }
  for (const CFGBlock *CBlock : *postView) {
    CFGBlock *Block = const_cast<CFGBlock *>(CBlock);
    if (lastResort(Block)) {
      return true;
    }
  }
  return false;
}

bool Phoenix::lastResort(CFGBlock *n) {
  llvm::Optional<Phoenix::VirtualEdge> vEdge;
  for (auto &s : n->succs()) {
    if (!Dom.properlyDominates(n, s) && !Dom.properlyDominates(s, n)) {
      vEdge.emplace(n, s, VirtualEdgeType::Goto);
      break;
    }
  }
  if (!vEdge.hasValue()) {
    for (auto &s : n->succs()) {
      if (!Dom.properlyDominates(n, s)) {
        vEdge.emplace(n, s, VirtualEdgeType::Goto);
        break;
      }
    }
  }
  if (!vEdge.hasValue()) {
    for (auto &p : n->preds()) {
      if (!Dom.properlyDominates(p, n)) {
        vEdge.emplace(p, n, VirtualEdgeType::Goto);
      }
    }
  }
  if (vEdge.hasValue()) {
    virtualizeEdge(*vEdge);
    return true;
  } else {
    // Whoa, we're in trouble now....
    return false;
  }
}

bool Phoenix::reduceSequence(CFGBlock *Block) {
  assert(Block->succ_size() == 1);
  CFGBlock *Succ = *Block->succ_begin();
  if (Succ->pred_size() == 1) {
    // merge two blocks
    // 1 add all instructions in succ to current block
    Block->setTerminator(Succ->getTerminator());
    Succ->setTerminator(nullptr);
    addAllStmtTo(Succ, Block);
    // remove this edge
    removeEdge(Block, Succ);
    // add all edges from succ to current block
    replaceSuccessors(Succ, Block);
    // TODO faster remove instead of linear search.
    deferredRemove(Succ);
    return true;
  } else {
    return false;
  }
}

static bool allCasesAreTails(CFGBlock *n) {
  for (auto succ : n->succs()) {
    if (succ->succ_size() > 0) {
      return false;
    }
  }
  return true;
}

//   ┌──────Head─────────┐
//   │       │           │
//   ▼       ▼           ▼
// Case1     Case2     Case...
//   │        │          │
//   │        ▼          │
//   └───► Follow ◄──────┘
/// Matching the pattern (figure above) to find the follow block
/// case block can be a tail block (without any successor);
CFGBlock *Phoenix::getSwitchFollow(CFGBlock *n) {
  CFGBlock *follow = nullptr;
  for (auto s : n->succs()) {
    // for each case block
    if (s == follow) {
      continue;
    }
    if (s->succ_size() != 0) {
      auto ss = linearSuccessor(s);
      if (ss == nullptr) {
        // multiple successor for case block: not a good switch
        return nullptr;
      }
      // this is the follow block
      if (follow == nullptr) {
        follow = ss;
      } else if (ss != follow) {
        // multiple possible follow: not a good switch
        return nullptr;
      }
    }
  }
  if (follow == n) {
    return nullptr;
  }
  return follow;
}

/// Checks if any case block has any predecessor that is not switch head
bool hasIrregularEntries(CFGBlock *n, CFGBlock *follow) {
  for (auto s : n->succs()) {
    if (s == follow)
      continue;
    for (auto p : s->preds()) {
      if (p != n) {
        return true;
      }
    }
  }
  return false;
}

bool Phoenix::reduceIncSwitch(CFGBlock *N, CFGBlock *Follow) {
  // 1. `Follow` is nullptr, and all cases are tails.
  // 2. Some cases are tail region.
  // 3. Other cases have a common follow, and has no other pred other than
  // 4. Some case block is just follow block. As if there is a block with a
  // single break. switch head.
  // LLVM_DEBUG(
  //     llvm::dbgs() << " ====== CFG before Reducing Switch Region ======\n";
  //     CFG.dump(Ctx.getLangOpts(), FCtx.getOpts().enableColor););
  auto &term = std::get<SwitchTerminator>(N->getTerminator());
  auto cond = llvm::cast<clang::Expr>(term.getStmt());
  auto SW = clang::SwitchStmt::Create(Ctx, nullptr, nullptr, cond,
                                      clang::SourceLocation(),
                                      clang::SourceLocation());
  std::vector<clang::Stmt *> Stmts;
  Stmts.reserve(N->succ_size());
  auto SuccIt = N->succ_begin();

  // Handle default stmt, the first successor.
  // Put the default case at the end, so we do not need a break.
  CFGBlock *DefaultTarget = *SuccIt;
  if (*SuccIt == Follow) {
    // Safely ignore the default case;
    DefaultTarget = nullptr;
  }
  SuccIt++;

  // Organize cases according to the target block.
  std::map<CFGBlock *, std::vector<clang::Expr *>> CaseMap;
  for (auto caseVal : term.cases()) {
    assert(SuccIt != N->succ_end());
    auto caseExpr = llvm::cast<clang::Expr>(caseVal);
    auto succBlock = *SuccIt;
    CaseMap[succBlock].push_back(caseExpr);
    SuccIt++;
  }
  assert(SuccIt == N->succ_end());

  // for each case, insert case label, and insert block stmts
  for (auto &Ent : CaseMap) {
    auto CaseBlock = Ent.first;
    auto &CaseExprs = Ent.second;

    // a sequence of CaseStmt is actually nested.
    clang::CaseStmt *FirstCS = nullptr;
    clang::Stmt *LastCS = nullptr;

    for (auto Case : CaseExprs) {
      // CaseStmt can have RHS, but we are not using it:
      // https://gcc.gnu.org/onlinedocs/gcc/Case-Ranges.html
      auto NewCS = clang::CaseStmt::Create(
          Ctx, Case, nullptr, clang::SourceLocation(), clang::SourceLocation(),
          clang::SourceLocation());
      SW->addSwitchCase(NewCS);
      if (FirstCS == nullptr) {
        FirstCS = NewCS;
      }
      if (LastCS == nullptr) {
        LastCS = NewCS;
      } else {
        llvm::cast<clang::CaseStmt>(LastCS)->setSubStmt(NewCS);
        LastCS = NewCS;
      }
    }

    // if default case also goes to this block
    if (CaseBlock == DefaultTarget) {
      auto NewCS = new (Ctx) clang::DefaultStmt(
          clang::SourceLocation(), clang::SourceLocation(), nullptr);
      SW->addSwitchCase(NewCS);
      llvm::cast<clang::CaseStmt>(LastCS)->setSubStmt(NewCS);
      LastCS = NewCS;
      DefaultTarget = nullptr;
    }

    // Create body for the case
    clang::Stmt *Body;
    if (CaseBlock == Follow) {
      Body = new (Ctx) clang::BreakStmt(clang::SourceLocation());
    } else {
      clang::Stmt *Break;
      if (CaseBlock->succ_size() == 0) {
        // tail block
        Break = nullptr;
      } else {
        assert(CaseBlock->getSingleSuccessor() == Follow);
        Break = new (Ctx) clang::BreakStmt(clang::SourceLocation());
      }
      Body = makeCompoundStmt(CaseBlock, Break);
    }

    // Call setSubStmt, but do not know it is CaseStmt or DefaultStmt
    if (auto Case = llvm::dyn_cast<clang::CaseStmt>(LastCS)) {
      Case->setSubStmt(Body);
    } else {
      llvm::cast<clang::DefaultStmt>(LastCS)->setSubStmt(Body);
    }
    Stmts.push_back(FirstCS);
  }

  // If the default case is still not handled
  if (DefaultTarget != nullptr) {
    auto DS = new (Ctx)
        clang::DefaultStmt(clang::SourceLocation(), clang::SourceLocation(),
                           makeCompoundStmt(DefaultTarget));
    SW->addSwitchCase(DS);
    Stmts.push_back(DS);
  }

  auto Body = clang::CompoundStmt::Create(Ctx, Stmts, clang::SourceLocation(),
                                          clang::SourceLocation());
  SW->setBody(Body);
  N->appendStmt(SW);

  // handle all edges
  // remove all switch edges, and edges from case blocks to follow block.
  // we use set because case block can be shared.
  for (auto caseBlock : std::set<CFGBlock *>(N->succ_begin(), N->succ_end())) {
    assert(caseBlock != N);
    removeEdge(N, caseBlock);
    if (caseBlock == Follow) {
      continue;
    } else if (caseBlock->succ_size() == 0) {
      // tail block: do nothing
    } else {
      assert(caseBlock->getSingleSuccessor() == Follow);
      removeEdge(caseBlock, Follow);
    }
    caseBlock->removePred(N);
    deferredRemove(caseBlock);
  }
  // TODO ensure that reachability is not broken.
  if (Follow != nullptr) {
    // add a single linear edge from switch to follow
    addEdge(N, Follow);
    assert(N->succ_size() == 1);
  }

  return true;
}

bool Phoenix::reduceSwitchRegion(CFGBlock *n) {
  CFGBlock *follow = getSwitchFollow(n);
  // check for predecessor that is not switch head.
  bool hasIrregular = hasIrregularEntries(n, follow);
  if (!hasIrregular && (follow != nullptr || allCasesAreTails(n))) {
    return reduceIncSwitch(n, follow);
  }

  // It's a switch region, but we are unable to collapse it.
  // Schedule it for refinement after the whole graph has been
  // traversed.
  // Do not refine switch region if there are unresolved cycles
  if (unresolvedCycles.size() == 0) {
    unresolvedSwitches.push_back(n);
  }
  return false;
}

bool Phoenix::reduceIfRegion(CFGBlock *Block, bool UseTail) {
  auto tmp = Block->getTwoSuccs();
  CFGBlock *th = tmp.first;
  CFGBlock *el = tmp.second;
  assert(th != nullptr && el != nullptr && el != th);
  CFGBlock *elS = linearSuccessor(el);
  CFGBlock *thS = linearSuccessor(th);
  bool elseTail = false;
  bool thenTail = false;
  if (UseTail) {
    elseTail = (el->succ_size() == 0);
    thenTail = (th->succ_size() == 0);
  }

  // the successor of else is then block.
  if ((elS == th || elseTail) && onlyPred(el, Block)) {
    clang::Expr *cond = takeBinaryCond(*Block);
    // collapse the else block: if(!cond){else} elseSucc/then
    cond = invertCond(cond);
    auto then = makeCompoundStmt(el);
    auto IfStmt = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary, nullptr,
        nullptr, cond, clang::SourceLocation(), clang::SourceLocation(), then,
        clang::SourceLocation());
    Block->appendStmt(IfStmt);
    // maintain the edge
    removeEdge(Block, el);
    if (!elseTail) {
      removeEdge(el, elS);
    }
    deferredRemove(el);
    assert(Block->succ_size() == 1);
    return true;
  } else if ((thS == el || thenTail) && onlyPred(th, Block)) {
    clang::Expr *cond = takeBinaryCond(*Block);
    // collapse the then block: if(cond){then} thenSucc/else;
    auto then = makeCompoundStmt(th);
    auto IfStmt = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary, nullptr,
        nullptr, cond, clang::SourceLocation(), clang::SourceLocation(), then,
        clang::SourceLocation());
    Block->appendStmt(IfStmt);
    // maintain the edge
    removeEdge(Block, th);
    if (thS == el) {
      removeEdge(th, thS);
    }
    deferredRemove(th);
    assert(Block->succ_size() == 1);
    return true;
  } else if (elS != nullptr && elS == thS) {
    if (!(onlyPred(el, Block) && onlyPred(th, Block))) {
      return false;
    }
    clang::Expr *cond = takeBinaryCond(*Block);
    // collapse the then and else block: if(cond){thenBlk}else{elseBlk}
    // thenSucc/elseSucc;
    auto then = makeCompoundStmt(th);
    auto els = makeCompoundStmt(el);
    auto IfStmt = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary, nullptr,
        nullptr, cond, clang::SourceLocation(), clang::SourceLocation(), then,
        clang::SourceLocation(), els);
    Block->appendStmt(IfStmt);
    // maintain the edge
    removeEdge(Block, th);
    removeEdge(Block, el);
    removeEdge(th, thS);
    removeEdge(el, elS);
    deferredRemove(th);
    deferredRemove(el);
    addEdge(Block, elS);
    assert(Block->succ_size() == 1);
    return true;
  }
  return false;
}

bool Phoenix::ReduceAcyclic(CFGBlock *Block, bool UseTail) {
  switch (Block->succ_size()) {
  case 0:
    return false;
    break;
  case 1:
    // reduce sequence
    return reduceSequence(Block);
    break;
  case 2:
    // reduce if-else
    return reduceIfRegion(Block, UseTail);
    break;
  default:
    return reduceSwitchRegion(Block);
    break;
  }
  // unreachable
  assert(false && "unreachable");
}

} // namespace notdec::llvm2c
