#include <cassert>
#include <map>
#include <memory>
#include <vector>

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <llvm/ADT/APInt.h>
#include <llvm/Support/Casting.h>

#include "notdec-backends/Structuring/StructurerRegistry.h"
#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/StructuredGoto.h"
#include "notdec-llvm2c/Utils.h"

namespace notdec::llvm2c {

namespace st = notdec::backend::structuring;

namespace {

class StructuredGotoAdapter {
  SAFuncContext &FCtx;
  CFG &Cfg;
  clang::ASTContext &Ctx;
  StructuredGoto &SA;

  std::vector<clang::Stmt *> Payloads;
  std::map<st::BlockId, CFGBlock *> Blocks;

public:
  StructuredGotoAdapter(SAFuncContext &FCtx, StructuredGoto &SA)
      : FCtx(FCtx), Cfg(FCtx.getCFG()), Ctx(FCtx.getASTContext()), SA(SA) {}

  void execute() {
    std::unique_ptr<st::Structurer> Structurer =
        st::createStructurer(SA.getStructurerName());
    assert(Structurer != nullptr);
    st::StructuredTree Tree = Structurer->structure(buildCFG());
    std::vector<clang::Stmt *> Stmts;
    renderNode(Tree, Tree.root(), Stmts);
    replaceCFG(std::move(Stmts));
  }

private:
  st::PayloadRef addPayload(clang::Stmt *Stmt) {
    if (Stmt == nullptr) {
      return {};
    }
    Payloads.push_back(Stmt);
    return {Payloads.size() - 1};
  }

  clang::Stmt *getPayload(st::PayloadRef Ref) const {
    if (!Ref.isValid()) {
      return nullptr;
    }
    assert(Ref.Id < Payloads.size());
    return Payloads[Ref.Id];
  }

  CFGBlock *getBlock(st::BlockId Id) const {
    auto It = Blocks.find(Id);
    assert(It != Blocks.end());
    return It->second;
  }

  st::StructuredCFG buildCFG() {
    st::StructuredCFG Ret;

    for (CFGBlock *Block : Cfg) {
      st::CFGBlock NewBlock;
      NewBlock.Id = Block->getBlockID();
      Blocks[NewBlock.Id] = Block;

      for (auto It = Block->begin(); It != Block->end(); ++It) {
        if (clang::Stmt *Stmt = getStmt(*It)) {
          if (Stmt == Block->getTerminatorStmt()) {
            continue;
          }
          NewBlock.Statements.push_back(addPayload(Stmt));
        }
      }

      for (CFGBlock *Succ : Block->succs()) {
        NewBlock.Successors.push_back(Succ->getBlockID());
      }

      if (Block->succ_size() == 2) {
        NewBlock.Terminator = st::TerminatorKind::Branch;
        NewBlock.Condition = addPayload(Block->getTerminatorStmt());
      } else if (Block->succ_size() > 2) {
        NewBlock.Terminator = st::TerminatorKind::Switch;
        auto &Term = std::get<SwitchTerminator>(Block->getTerminator());
        NewBlock.Condition = addPayload(Term.getStmt());
        auto SuccIt = Block->succ_begin();
        if (SuccIt != Block->succ_end()) {
          ++SuccIt;
        }
        for (clang::Stmt *Case : Term.cases()) {
          assert(SuccIt != Block->succ_end());
          NewBlock.Cases.push_back({addPayload(Case), (*SuccIt)->getBlockID()});
          ++SuccIt;
        }
      }

      Ret.addBlock(std::move(NewBlock));
    }

    return Ret;
  }

  void renderNode(const st::StructuredTree &Tree, st::NodeId Id,
                  std::vector<clang::Stmt *> &Stmts) {
    const st::StructuredNode *Node = Tree.getNode(Id);
    assert(Node != nullptr);

    switch (Node->Kind) {
    case st::StructuredNodeKind::Sequence:
      for (st::NodeId Child : Node->Children) {
        renderNode(Tree, Child, Stmts);
      }
      break;
    case st::StructuredNodeKind::Label:
      Stmts.push_back(SA.getOrCreateBlockLabelStmt(getBlock(Node->Block)));
      break;
    case st::StructuredNodeKind::BasicBlock:
      for (st::PayloadRef Ref : Node->Statements) {
        Stmts.push_back(getPayload(Ref));
      }
      break;
    case st::StructuredNodeKind::If:
      if (Node->Children.empty() && (Node->Then != st::InvalidNodeId ||
                                     Node->Else != st::InvalidNodeId)) {
        renderIf(Tree, *Node, Stmts);
      } else {
        renderIf(*Node, Stmts);
      }
      break;
    case st::StructuredNodeKind::Switch:
      if (Node->Children.empty() && (!Node->StructuredCases.empty() ||
                                     Node->Default != st::InvalidNodeId)) {
        Stmts.push_back(renderSwitch(Tree, *Node));
      } else {
        Stmts.push_back(renderSwitch(*Node));
      }
      break;
    case st::StructuredNodeKind::Goto:
      Stmts.push_back(
          SA.makeGotoStmt(SA.getOrCreateBlockLabel(getBlock(Node->Target))));
      break;
    case st::StructuredNodeKind::Return:
    case st::StructuredNodeKind::Unreachable:
      break;
    case st::StructuredNodeKind::Break:
      Stmts.push_back(new (Ctx) clang::BreakStmt(clang::SourceLocation()));
      break;
    case st::StructuredNodeKind::Continue:
      Stmts.push_back(new (Ctx) clang::ContinueStmt(clang::SourceLocation()));
      break;
    case st::StructuredNodeKind::While:
      Stmts.push_back(renderWhile(Tree, *Node));
      break;
    case st::StructuredNodeKind::DoWhile:
      Stmts.push_back(renderDoWhile(Tree, *Node));
      break;
    case st::StructuredNodeKind::InfiniteLoop:
      Stmts.push_back(renderInfiniteLoop(Tree, *Node));
      break;
    }
  }

  clang::Stmt *renderCompound(const st::StructuredTree &Tree, st::NodeId Id) {
    std::vector<clang::Stmt *> Stmts;
    renderNode(Tree, Id, Stmts);
    return clang::CompoundStmt::Create(Ctx, Stmts, clang::FPOptionsOverride(),
                                       clang::SourceLocation(),
                                       clang::SourceLocation());
  }

  clang::Expr *invertCond(clang::Expr *Cond) {
    if (auto *BO = llvm::dyn_cast<clang::BinaryOperator>(Cond)) {
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
    return createUnaryOperator(Ctx, Cond, clang::UO_LNot, Ctx.IntTy,
                               clang::VK_PRValue);
  }

  clang::Expr *conditionExpr(const st::StructuredNode &Node) {
    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    return Node.ConditionNegated ? invertCond(Cond) : Cond;
  }

  clang::Expr *trueExpr() {
    return clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, 1, false),
                                         Ctx.IntTy, clang::SourceLocation());
  }

  void renderIf(const st::StructuredTree &Tree, const st::StructuredNode &Node,
                std::vector<clang::Stmt *> &Stmts) {
    auto *Cond = conditionExpr(Node);
    clang::Stmt *Then = Node.Then == st::InvalidNodeId
                            ? nullptr
                            : renderCompound(Tree, Node.Then);
    clang::Stmt *Else = Node.Else == st::InvalidNodeId
                            ? nullptr
                            : renderCompound(Tree, Node.Else);
    auto *If = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary, nullptr,
        nullptr, Cond, clang::SourceLocation(), clang::SourceLocation(), Then,
        clang::SourceLocation(), Else);
    Stmts.push_back(If);
  }

  void renderIf(const st::StructuredNode &Node,
                std::vector<clang::Stmt *> &Stmts) {
    CFGBlock *Block = getBlock(Node.Block);
    assert(Block->succ_size() == 2);

    auto SuccIt = Block->succ_begin();
    CFGBlock *TrueBlock = *SuccIt;
    ++SuccIt;
    CFGBlock *FalseBlock = *SuccIt;

    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    auto *ThenGoto = SA.makeGotoStmt(SA.getOrCreateBlockLabel(TrueBlock));
    auto *If = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
        nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation(), ThenGoto, clang::SourceLocation());
    Stmts.push_back(If);
    Stmts.push_back(SA.makeGotoStmt(SA.getOrCreateBlockLabel(FalseBlock)));
  }

  clang::Stmt *renderSwitch(const st::StructuredTree &Tree,
                            const st::StructuredNode &Node) {
    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    if (Cond->getType()->isPointerType()) {
      Cond = FCtx.getTypeBuilder().checkCast(
          Cond, Ctx.getIntTypeForBitwidth(
                    FCtx.getTypeBuilder().getPointerSizeInBits(), 1));
    }

    auto *Switch = clang::SwitchStmt::Create(
        Ctx, nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation());
    std::vector<clang::Stmt *> Cases;

    for (const auto &Case : Node.StructuredCases) {
      auto *CaseStmt = clang::CaseStmt::Create(
          Ctx, llvm::cast<clang::Expr>(getPayload(Case.Value)), nullptr,
          clang::SourceLocation(), clang::SourceLocation(),
          clang::SourceLocation());
      CaseStmt->setSubStmt(renderCompound(Tree, Case.Body));
      Switch->addSwitchCase(CaseStmt);
      Cases.push_back(CaseStmt);
    }

    if (Node.Default != st::InvalidNodeId) {
      auto *Default = new (Ctx)
          clang::DefaultStmt(clang::SourceLocation(), clang::SourceLocation(),
                             renderCompound(Tree, Node.Default));
      Switch->addSwitchCase(Default);
      Cases.push_back(Default);
    }

    auto *Body = clang::CompoundStmt::Create(
        Ctx, Cases, clang::FPOptionsOverride(), clang::SourceLocation(),
        clang::SourceLocation());
    Switch->setBody(Body);
    return Switch;
  }

  clang::Stmt *renderSwitch(const st::StructuredNode &Node) {
    CFGBlock *Block = getBlock(Node.Block);
    assert(Block->succ_size() > 2);

    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    if (Cond->getType()->isPointerType()) {
      Cond = FCtx.getTypeBuilder().checkCast(
          Cond, Ctx.getIntTypeForBitwidth(
                    FCtx.getTypeBuilder().getPointerSizeInBits(), 1));
    }

    auto *Switch = clang::SwitchStmt::Create(
        Ctx, nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation());
    std::vector<clang::Stmt *> Cases;

    auto SuccIt = Block->succ_begin();
    CFGBlock *DefaultBlock = *SuccIt;
    auto *DefaultGoto = SA.makeGotoStmt(SA.getOrCreateBlockLabel(DefaultBlock));
    auto *Default = new (Ctx) clang::DefaultStmt(
        clang::SourceLocation(), clang::SourceLocation(), DefaultGoto);
    ++SuccIt;

    for (const auto &Case : Node.Cases) {
      assert(SuccIt != Block->succ_end());
      auto *CaseStmt = clang::CaseStmt::Create(
          Ctx, llvm::cast<clang::Expr>(getPayload(Case.Value)), nullptr,
          clang::SourceLocation(), clang::SourceLocation(),
          clang::SourceLocation());
      CaseStmt->setSubStmt(
          SA.makeGotoStmt(SA.getOrCreateBlockLabel(*SuccIt)));
      Switch->addSwitchCase(CaseStmt);
      Cases.push_back(CaseStmt);
      ++SuccIt;
    }

    Switch->addSwitchCase(Default);
    Cases.push_back(Default);
    auto *Body = clang::CompoundStmt::Create(
        Ctx, Cases, clang::FPOptionsOverride(), clang::SourceLocation(),
        clang::SourceLocation());
    Switch->setBody(Body);
    return Switch;
  }

  clang::Stmt *renderWhile(const st::StructuredTree &Tree,
                           const st::StructuredNode &Node) {
    return clang::WhileStmt::Create(
        Ctx, nullptr, conditionExpr(Node), renderCompound(Tree, Node.Body),
        clang::SourceLocation(), clang::SourceLocation(),
        clang::SourceLocation());
  }

  clang::Stmt *renderDoWhile(const st::StructuredTree &Tree,
                             const st::StructuredNode &Node) {
    return new (Ctx)
        clang::DoStmt(renderCompound(Tree, Node.Body), conditionExpr(Node),
                      clang::SourceLocation(), clang::SourceLocation(),
                      clang::SourceLocation());
  }

  clang::Stmt *renderInfiniteLoop(const st::StructuredTree &Tree,
                                  const st::StructuredNode &Node) {
    return clang::WhileStmt::Create(
        Ctx, nullptr, trueExpr(), renderCompound(Tree, Node.Body),
        clang::SourceLocation(), clang::SourceLocation(),
        clang::SourceLocation());
  }

  void replaceCFG(std::vector<clang::Stmt *> Stmts) {
    CFGBlock &Entry = Cfg.getEntry();
    Entry.clear();
    Entry.succ_clear();
    Entry.pred_clear();
    for (clang::Stmt *Stmt : Stmts) {
      Entry.appendStmt(Stmt);
    }

    for (CFGBlock *Block : std::vector<CFGBlock *>(Cfg.begin(), Cfg.end())) {
      if (Block == &Entry) {
        continue;
      }
      Block->succ_clear();
      Block->pred_clear();
      Cfg.remove(Block);
    }
  }
};

} // namespace

void StructuredGoto::execute() {
  StructuredGotoAdapter Adapter(FCtx, *this);
  Adapter.execute();
}

} // namespace notdec::llvm2c
