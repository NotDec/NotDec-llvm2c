#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <llvm/ADT/APInt.h>
#include <llvm/Support/Casting.h>

#include "notdec-backends/Structuring/StructurerRegistry.h"
#include "notdec-llvm2c/CFG.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include "notdec-llvm2c/StructuredGoto.h"
#include "notdec-llvm2c/Utils.h"

namespace notdec::llvm2c {

namespace st = notdec::backend::structuring;

namespace {

class StructuredGotoAdapter {
  CFG &Cfg;
  clang::ASTContext &Ctx;
  StructuredGoto &SA;

  std::vector<clang::Stmt *> Payloads;
  st::StructuredCFG SharedCfg;
  std::map<st::BlockId, clang::LabelStmt *> Labels;
  std::set<st::BlockId> TargetedLabels;

public:
  StructuredGotoAdapter(StructuredGoto &SA)
      : Cfg(SA.getRenderCFG()), Ctx(SA.getRenderASTContext()), SA(SA) {}

  void execute() {
    std::unique_ptr<st::Structurer> Structurer =
        st::createStructurer(SA.getStructurerName());
    assert(Structurer != nullptr);
    SharedCfg = buildCFG();
    // Keep copied payload ids distinct in the shared CFG even when the C path
    // still reuses the same underlying Clang nodes.
    SharedCfg.setPayloadMaterializeHook(
        [this](const st::PayloadMaterializeContext &, st::PayloadMaterializeKind,
               st::PayloadRef Payload, std::size_t) -> std::optional<st::PayloadRef> {
          if (!Payload.isValid()) {
            return Payload;
          }
          Payloads.push_back(Payloads[Payload.Id]);
          return st::PayloadRef{Payloads.size() - 1};
        });
    st::StructuredTree Tree = Structurer->structure(SharedCfg);
    collectGotoTargets(Tree, Tree.root());
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

  const st::CFGBlock *getSharedBlock(st::BlockId Id) const {
    const st::CFGBlock *Block = SharedCfg.getBlock(Id);
    assert(Block != nullptr);
    return Block;
  }

  clang::LabelStmt *getOrCreateLabelStmt(st::BlockId Id) {
    auto &Label = Labels[Id];
    if (Label == nullptr) {
      Label = SA.createStructuredBlockLabelStmt(Id);
    }
    return Label;
  }

  clang::LabelDecl *getOrCreateLabel(st::BlockId Id) {
    return getOrCreateLabelStmt(Id)->getDecl();
  }

  void collectGotoTargets(const st::StructuredTree &Tree, st::NodeId Id) {
    const st::StructuredNode *Node = Tree.getNode(Id);
    if (Node == nullptr) {
      return;
    }
    if (Node->Kind == st::StructuredNodeKind::Goto &&
        Node->Target != st::InvalidBlockId) {
      TargetedLabels.insert(Node->Target);
    }
    for (st::NodeId Child : Node->Children) {
      collectGotoTargets(Tree, Child);
    }
    for (const st::StructuredSwitchCase &Case : Node->StructuredCases) {
      collectGotoTargets(Tree, Case.Body);
    }
    collectGotoTargets(Tree, Node->Then);
    collectGotoTargets(Tree, Node->Else);
    collectGotoTargets(Tree, Node->Body);
    collectGotoTargets(Tree, Node->Default);
  }

  st::StructuredCFG buildCFG() {
    st::StructuredCFG Ret;

    for (CFGBlock *Block : Cfg) {
      st::CFGBlock NewBlock;
      NewBlock.Id = Block->getBlockID();
      bool HasReturnStmt = false;
      if (Block->isSAILRDephicationEdge()) {
        NewBlock.Origin = st::CFGBlockOrigin::Synthetic;
        NewBlock.CopyKind = st::CFGBlockCopyKind::SyntheticForwarder;
        NewBlock.CreatedBy = st::CFGBlockCreator::SAILRDephication;
        NewBlock.SyntheticSource = Block->getSAILRDephicationSourceBlock();
        NewBlock.SyntheticTarget = Block->getSAILRDephicationTargetBlock();
      }

      for (auto It = Block->begin(); It != Block->end(); ++It) {
        if (clang::Stmt *Stmt = getStmt(*It)) {
          if (Stmt == Block->getTerminatorStmt()) {
            continue;
          }
          HasReturnStmt |= llvm::isa<clang::ReturnStmt>(Stmt);
          NewBlock.Statements.push_back(addPayload(Stmt));
        }
      }

      if (HasReturnStmt ||
          llvm::isa_and_nonnull<clang::ReturnStmt>(Block->getTerminatorStmt())) {
        NewBlock.Terminator = st::TerminatorKind::Return;
      } else {
        for (CFGBlock *Succ : Block->succs()) {
          NewBlock.Successors.push_back(Succ->getBlockID());
        }
      }

      if (NewBlock.Terminator == st::TerminatorKind::Return) {
        // keep terminal blocks closed so structuring can treat them as exits
      } else if (Block->succ_size() == 2) {
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
      if (TargetedLabels.count(Node->Block) != 0) {
        Stmts.push_back(getOrCreateLabelStmt(Node->Block));
      }
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
      Stmts.push_back(SA.makeGotoStmt(getOrCreateLabel(Node->Target)));
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
      clang::BinaryOperatorKind Inverted;
      switch (BO->getOpcode()) {
      case clang::BO_EQ:
        Inverted = clang::BO_NE;
        break;
      case clang::BO_NE:
        Inverted = clang::BO_EQ;
        break;
      case clang::BO_LT:
        Inverted = clang::BO_GE;
        break;
      case clang::BO_GT:
        Inverted = clang::BO_LE;
        break;
      case clang::BO_LE:
        Inverted = clang::BO_GT;
        break;
      case clang::BO_GE:
        Inverted = clang::BO_LT;
        break;
      default:
        return createUnaryOperator(Ctx, Cond, clang::UO_LNot, Ctx.IntTy,
                                   clang::VK_PRValue);
      }
      return createBinaryOperator(Ctx, BO->getLHS(), BO->getRHS(), Inverted,
                                  BO->getType(), BO->getValueKind());
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
    const st::CFGBlock *Block = getSharedBlock(Node.Block);
    assert(Block->Successors.size() == 2);

    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    auto *ThenGoto = SA.makeGotoStmt(getOrCreateLabel(Block->Successors[0]));
    auto *If = clang::IfStmt::Create(
        Ctx, clang::SourceLocation(), clang::IfStatementKind::Ordinary,
        nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation(), ThenGoto, clang::SourceLocation());
    Stmts.push_back(If);
    Stmts.push_back(SA.makeGotoStmt(getOrCreateLabel(Block->Successors[1])));
  }

  clang::Stmt *renderSwitch(const st::StructuredTree &Tree,
                            const st::StructuredNode &Node) {
    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    Cond = SA.castRenderSwitchCondition(Cond);

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
    const st::CFGBlock *Block = getSharedBlock(Node.Block);
    assert(Block->Successors.size() > 1);

    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    Cond = SA.castRenderSwitchCondition(Cond);

    auto *Switch = clang::SwitchStmt::Create(
        Ctx, nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation());
    std::vector<clang::Stmt *> Cases;

    auto *DefaultGoto =
        SA.makeGotoStmt(getOrCreateLabel(Block->Successors.front()));
    auto *Default = new (Ctx) clang::DefaultStmt(
        clang::SourceLocation(), clang::SourceLocation(), DefaultGoto);

    for (const auto &Case : Node.Cases) {
      auto *CaseStmt = clang::CaseStmt::Create(
          Ctx, llvm::cast<clang::Expr>(getPayload(Case.Value)), nullptr,
          clang::SourceLocation(), clang::SourceLocation(),
          clang::SourceLocation());
      assert(Case.Target != st::InvalidBlockId);
      CaseStmt->setSubStmt(SA.makeGotoStmt(getOrCreateLabel(Case.Target)));
      Switch->addSwitchCase(CaseStmt);
      Cases.push_back(CaseStmt);
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
  StructuredGotoAdapter Adapter(*this);
  Adapter.execute();
}

clang::LabelStmt *
StructuredGoto::createStructuredBlockLabelStmt(unsigned BlockId) {
  std::string Name = "structured_block_" + std::to_string(BlockId);
  clang::IdentifierInfo *II = FCtx.getIdentifierInfo(Name);
  auto *Decl = clang::LabelDecl::Create(Ctx, FCtx.getFunctionDecl(),
                                        clang::SourceLocation(), II);
  return new (Ctx)
      clang::LabelStmt(clang::SourceLocation(), Decl,
                       new (Ctx) clang::NullStmt(clang::SourceLocation()));
}

} // namespace notdec::llvm2c
