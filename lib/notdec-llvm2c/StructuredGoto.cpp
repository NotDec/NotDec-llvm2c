#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <llvm/ADT/StringRef.h>
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

clang::VarDecl *assignmentTargetVar(clang::Stmt *Stmt) {
  auto *Assign = llvm::dyn_cast_or_null<clang::BinaryOperator>(Stmt);
  if (Assign == nullptr || Assign->getOpcode() != clang::BO_Assign) {
    return nullptr;
  }
  auto *Target =
      llvm::dyn_cast_or_null<clang::DeclRefExpr>(getNoCast(Assign->getLHS()));
  return Target == nullptr ? nullptr
                           : llvm::dyn_cast<clang::VarDecl>(Target->getDecl());
}

struct SwitchBodyKey {
  st::StructuredNodeKind Kind = st::StructuredNodeKind::Sequence;
  st::BlockId Target = st::InvalidBlockId;
  st::NodeId Body = st::InvalidNodeId;

  friend bool operator<(const SwitchBodyKey &Lhs,
                        const SwitchBodyKey &Rhs) {
    return std::tie(Lhs.Kind, Lhs.Target, Lhs.Body) <
           std::tie(Rhs.Kind, Rhs.Target, Rhs.Body);
  }
};

struct SwitchBodyLabel {
  // A switch body may have several case/default labels. Keep only the last
  // label here; new labels are chained after it, and the body is attached last.
  clang::SwitchCase *LastLabel = nullptr;
  st::NodeId Body = st::InvalidNodeId;
};

class DeclRefRewriter : public StmtTransform<DeclRefRewriter> {
  std::map<clang::ValueDecl *, clang::ValueDecl *> Replacements;

public:
  DeclRefRewriter(
      clang::ASTContext &Ctx,
      std::map<clang::ValueDecl *, clang::ValueDecl *> Replacements)
      : StmtTransform<DeclRefRewriter>(Ctx),
        Replacements(std::move(Replacements)) {}

  ExprResult TransformDeclRefExpr(clang::DeclRefExpr *E) {
    auto It = Replacements.find(E->getDecl());
    if (It == Replacements.end()) {
      return StmtTransform<DeclRefRewriter>::TransformDeclRefExpr(E);
    }
    clang::ValueDecl *NewDecl = It->second;
    return clang::DeclRefExpr::Create(
        this->Context, E->getQualifierLoc(), E->getTemplateKeywordLoc(),
        NewDecl, E->refersToEnclosingVariableOrCapture(), E->getLocation(),
        NewDecl->getType().getNonReferenceType(), E->getValueKind());
  }
};

class DeclRefCollector
    : public clang::RecursiveASTVisitor<DeclRefCollector> {
  std::set<clang::ValueDecl *> &Decls;

public:
  explicit DeclRefCollector(std::set<clang::ValueDecl *> &Decls)
      : Decls(Decls) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr *Ref) {
    if (auto *Decl = Ref->getDecl()) {
      Decls.insert(Decl);
    }
    return true;
  }
};

class CallExprCounter : public clang::RecursiveASTVisitor<CallExprCounter> {
public:
  bool VisitCallExpr(clang::CallExpr *) {
    ++Count;
    return true;
  }

  std::size_t Count = 0;
};

std::size_t callExprCount(clang::Stmt *Stmt) {
  if (Stmt == nullptr) {
    return 0;
  }

  CallExprCounter Counter;
  Counter.TraverseStmt(Stmt);
  return Counter.Count;
}

clang::CallExpr *simpleCallExpr(clang::Stmt *Stmt) {
  if (auto *Call = llvm::dyn_cast_or_null<clang::CallExpr>(Stmt)) {
    return Call;
  }
  if (auto *Cleanups = llvm::dyn_cast_or_null<clang::ExprWithCleanups>(Stmt)) {
    return llvm::dyn_cast_or_null<clang::CallExpr>(Cleanups->getSubExpr());
  }
  return nullptr;
}

clang::FunctionDecl *directZeroArgCallee(clang::CallExpr *Call) {
  if (Call == nullptr || Call->getNumArgs() != 0) {
    return nullptr;
  }

  auto *DeclRef = llvm::dyn_cast_or_null<clang::DeclRefExpr>(
      getNoCast(Call->getCallee()));
  if (DeclRef == nullptr) {
    return nullptr;
  }

  return llvm::dyn_cast_or_null<clang::FunctionDecl>(DeclRef->getDecl());
}

class StructuredGotoAdapter {
  CFG &Cfg;
  clang::ASTContext &Ctx;
  StructuredGoto &SA;

  std::vector<clang::Stmt *> Payloads;
  st::StructuredCFG SharedCfg;
  std::map<st::VVarId, clang::VarDecl *> DephicationVarDecls;
  std::map<clang::VarDecl *, clang::DeclStmt *> CopiedDephicationDecls;
  std::map<st::BlockId, clang::LabelStmt *> Labels;
  std::map<clang::ValueDecl *, st::PayloadRef> ConditionDeclPayloads;
  std::map<std::string, st::PayloadRef> ConditionIntegerPayloads;
  // RetDupPass can clone one default return into two AST nodes; keep the
  // payloads separate for rendering, but share origin for simple return values.
  std::optional<st::PayloadRef> SimpleVoidReturnPayload;
  std::map<clang::ValueDecl *, st::PayloadRef> SimpleReturnDeclPayloads;
  std::map<std::string, st::PayloadRef> SimpleReturnIntegerPayloads;
  // P1 merge-graph work needs a shared identity for obviously identical call
  // statements. Keep this intentionally narrow: direct zero-argument calls only.
  std::map<clang::FunctionDecl *, st::PayloadRef> SimpleCallPayloads;
  std::set<st::BlockId> TargetedLabels;
  // A targeted break can be rendered as an early return.  Remember the original
  // terminal block so the later sequence walk does not print that return again.
  std::set<st::BlockId> InlinedTerminalTargets;

  // Some cleanup rules can leave loop-control nodes outside real loops; this
  // tracks where break/continue can be emitted as C syntax.
  struct RenderContext {
    bool Breakable = false;
    bool Continuable = false;
    st::BlockId BreakTarget = st::InvalidBlockId;
  };

public:
  StructuredGotoAdapter(StructuredGoto &SA)
      : Cfg(SA.getRenderCFG()), Ctx(SA.getRenderASTContext()), SA(SA) {}

  void execute() {
    std::unique_ptr<st::Structurer> Structurer =
        st::createStructurer(SA.getStructurerName());
    assert(Structurer != nullptr);
    SharedCfg = buildCFG();
    SharedCfg.setPayloadMaterializeHook(
        [this](const st::PayloadMaterializeContext &Context,
               st::PayloadMaterializeKind, st::PayloadRef Payload,
               std::size_t) -> std::optional<st::PayloadRef> {
          return materializePayload(Context, Payload);
        },
        /*SupportsPredecessorRewrite=*/true,
        /*SupportsGroupedPredecessorRewrite=*/true);
    st::StructuredTree Tree = Structurer->structure(SharedCfg);
    collectGotoTargets(Tree, Tree.root(), {});
    std::vector<clang::Stmt *> Stmts;
    renderNode(Tree, Tree.root(), Stmts, {});
    prependUsedCopiedDephicationDecls(Stmts);
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

  void setSimpleReturnOrigin(st::StructuredCFG &Cfg, st::PayloadRef Payload,
                             clang::ReturnStmt *Ret) {
    if (!Payload.isValid() || Ret == nullptr) {
      return;
    }

    clang::Expr *Value = Ret->getRetValue();
    if (Value == nullptr) {
      if (!SimpleVoidReturnPayload.has_value()) {
        SimpleVoidReturnPayload = Payload;
        return;
      }
      Cfg.setPayloadOrigin(Payload.Id, SimpleVoidReturnPayload->Id);
      return;
    }

    clang::Expr *NoCastValue = getNoCast(Value);
    if (auto *Literal =
            llvm::dyn_cast_or_null<clang::IntegerLiteral>(NoCastValue)) {
      std::string Key = llvm::toString(Literal->getValue(), 10,
                                       /*isSigned=*/false);
      auto It = SimpleReturnIntegerPayloads.find(Key);
      if (It == SimpleReturnIntegerPayloads.end()) {
        SimpleReturnIntegerPayloads.emplace(std::move(Key), Payload);
        return;
      }

      Cfg.setPayloadOrigin(Payload.Id, It->second.Id);
      return;
    }

    auto *DeclRef =
        llvm::dyn_cast_or_null<clang::DeclRefExpr>(NoCastValue);
    if (DeclRef == nullptr || DeclRef->getDecl() == nullptr) {
      return;
    }

    clang::ValueDecl *Decl = DeclRef->getDecl();
    auto It = SimpleReturnDeclPayloads.find(Decl);
    if (It == SimpleReturnDeclPayloads.end()) {
      SimpleReturnDeclPayloads.emplace(Decl, Payload);
      return;
    }
    Cfg.setPayloadOrigin(Payload.Id, It->second.Id);
  }

  void setSimpleCallOrigin(st::StructuredCFG &Cfg, st::PayloadRef Payload,
                           clang::Stmt *Stmt) {
    if (!Payload.isValid()) {
      return;
    }

    clang::FunctionDecl *Callee = directZeroArgCallee(simpleCallExpr(Stmt));
    if (Callee == nullptr) {
      return;
    }

    auto It = SimpleCallPayloads.find(Callee);
    if (It == SimpleCallPayloads.end()) {
      SimpleCallPayloads.emplace(Callee, Payload);
      return;
    }
    Cfg.setPayloadOrigin(Payload.Id, It->second.Id);
  }

  st::PayloadRef addConditionComparedPayload(clang::Expr *Expr) {
    auto *DeclRef =
        llvm::dyn_cast_or_null<clang::DeclRefExpr>(getNoCast(Expr));
    if (DeclRef == nullptr) {
      return addPayload(Expr);
    }

    clang::ValueDecl *Decl = DeclRef->getDecl();
    auto It = ConditionDeclPayloads.find(Decl);
    if (It != ConditionDeclPayloads.end()) {
      return It->second;
    }

    st::PayloadRef Payload = addPayload(Expr);
    ConditionDeclPayloads.emplace(Decl, Payload);
    return Payload;
  }

  st::PayloadRef addConditionIntegerPayload(clang::IntegerLiteral *Literal) {
    std::string Key = llvm::toString(Literal->getValue(), 10,
                                    /*isSigned=*/false);
    auto It = ConditionIntegerPayloads.find(Key);
    if (It != ConditionIntegerPayloads.end()) {
      return It->second;
    }

    st::PayloadRef Payload = addPayload(Literal);
    ConditionIntegerPayloads.emplace(std::move(Key), Payload);
    return Payload;
  }

  clang::Stmt *getPayload(st::PayloadRef Ref) const {
    if (!Ref.isValid()) {
      return nullptr;
    }
    assert(Ref.Id < Payloads.size());
    return Payloads[Ref.Id];
  }

  std::optional<st::PayloadRef> materializePayload(
      const st::PayloadMaterializeContext &Context, st::PayloadRef Payload) {
    if (!Payload.isValid()) {
      return Payload;
    }

    clang::Stmt *Stmt = Payloads[Payload.Id];
    std::map<clang::ValueDecl *, clang::ValueDecl *> Replacements =
        dephicationDeclReplacements(Context);
    if (!Replacements.empty()) {
      clang::StmtResult Rewritten =
          rewriteDeclRefs(Stmt, std::move(Replacements));
      if (Rewritten.isInvalid()) {
        return std::nullopt;
      }
      Stmt = Rewritten.get();
    }

    Payloads.push_back(Stmt);
    return st::PayloadRef{Payloads.size() - 1};
  }

  std::optional<st::ConditionCompareKind>
  conditionCompareKindFromBinaryOperator(clang::BinaryOperatorKind Opcode) {
    switch (Opcode) {
    case clang::BO_EQ:
      return st::ConditionCompareKind::Equal;
    case clang::BO_NE:
      return st::ConditionCompareKind::NotEqual;
    case clang::BO_GT:
      return st::ConditionCompareKind::GreaterThan;
    case clang::BO_GE:
      return st::ConditionCompareKind::GreaterEqual;
    case clang::BO_LT:
      return st::ConditionCompareKind::LessThan;
    case clang::BO_LE:
      return st::ConditionCompareKind::LessEqual;
    default:
      return std::nullopt;
    }
  }

  st::ConditionCompareKind
  swappedConditionCompareKind(st::ConditionCompareKind Kind) {
    switch (Kind) {
    case st::ConditionCompareKind::GreaterThan:
      return st::ConditionCompareKind::LessThan;
    case st::ConditionCompareKind::GreaterEqual:
      return st::ConditionCompareKind::LessEqual;
    case st::ConditionCompareKind::LessThan:
      return st::ConditionCompareKind::GreaterThan;
    case st::ConditionCompareKind::LessEqual:
      return st::ConditionCompareKind::GreaterEqual;
    case st::ConditionCompareKind::Equal:
    case st::ConditionCompareKind::NotEqual:
      return Kind;
    }
    return Kind;
  }

  std::optional<st::ConditionCompare>
  conditionCompareFromBranchCondition(clang::Stmt *Stmt) {
    auto *Expr = llvm::dyn_cast_or_null<clang::Expr>(Stmt);
    auto *BO =
        llvm::dyn_cast_or_null<clang::BinaryOperator>(getNoCast(Expr));
    if (BO == nullptr) {
      return std::nullopt;
    }

    std::optional<st::ConditionCompareKind> InitialKind =
        conditionCompareKindFromBinaryOperator(BO->getOpcode());
    if (!InitialKind.has_value()) {
      return std::nullopt;
    }
    st::ConditionCompareKind Kind = *InitialKind;

    clang::Expr *Compared = BO->getLHS();
    auto *Constant =
        llvm::dyn_cast_or_null<clang::IntegerLiteral>(getNoCast(BO->getRHS()));
    if (Constant == nullptr) {
      Constant =
          llvm::dyn_cast_or_null<clang::IntegerLiteral>(getNoCast(BO->getLHS()));
      Compared = BO->getRHS();
      Kind = swappedConditionCompareKind(Kind);
    }
    if (Constant == nullptr) {
      return std::nullopt;
    }

    bool HasIntegerValue = Constant->getValue().getBitWidth() <= 64;
    // Keep the true edge aligned with this builder's successor order. Later
    // passes should not need to infer polarity from payload text.
    std::size_t TrueTargetIndex = 0;
    return st::ConditionCompare{
        .ComparedValue = addConditionComparedPayload(Compared),
        .ConstantValue = addConditionIntegerPayload(Constant),
        .HasIntegerValue = HasIntegerValue,
        .SignedPredicate = true,
        .SignedIntegerValue = HasIntegerValue ? Constant->getValue().getSExtValue() : 0,
        .UnsignedIntegerValue = HasIntegerValue ? Constant->getValue().getZExtValue() : 0,
        .TrueTargetIndex = TrueTargetIndex,
        .EqualTargetIndex = Kind == st::ConditionCompareKind::NotEqual
                                ? std::size_t{1}
                                : TrueTargetIndex,
        .Kind = Kind};
  }

  std::map<clang::ValueDecl *, clang::ValueDecl *>
  dephicationDeclReplacements(
      const st::PayloadMaterializeContext &Context) {
    std::map<clang::ValueDecl *, clang::ValueDecl *> Replacements;
    for (const auto &Copy : Context.DephicationVVarCopies) {
      clang::VarDecl *Source = dephicationVarDecl(Copy.first);
      clang::VarDecl *Target =
          copiedDephicationVarDecl(Copy.second, Context);
      if (Source != nullptr && Target != nullptr && Source != Target) {
        Replacements.emplace(Source, Target);
      }
    }
    if (Context.CurrentDephicationIncoming.has_value()) {
      const st::DephicationIncoming &Incoming =
          *Context.CurrentDephicationIncoming;
      if (Incoming.SourceTarget != Incoming.Target) {
        clang::VarDecl *Source = dephicationVarDecl(Incoming.SourceTarget);
        clang::VarDecl *Target =
            copiedDephicationVarDecl(Incoming.Target, Context);
        if (Source != nullptr && Target != nullptr && Source != Target) {
          Replacements.emplace(Source, Target);
        }
      }
    }
    return Replacements;
  }

  clang::VarDecl *dephicationVarDecl(st::VVarId VVar) const {
    auto It = DephicationVarDecls.find(VVar);
    return It == DephicationVarDecls.end() ? nullptr : It->second;
  }

  clang::StmtResult rewriteDeclRefs(
      clang::Stmt *Stmt,
      std::map<clang::ValueDecl *, clang::ValueDecl *> Replacements) {
    DeclRefRewriter Rewriter(Ctx, std::move(Replacements));
    if (auto *Expr = llvm::dyn_cast_or_null<clang::Expr>(Stmt)) {
      return Rewriter.TransformExpr(Expr).get();
    }
    if (llvm::isa_and_nonnull<clang::ReturnStmt>(Stmt)) {
      return Rewriter.TransformStmt(Stmt);
    }
    return Stmt;
  }

  clang::VarDecl *copiedDephicationVarDecl(
      st::VVarId VVar, const st::PayloadMaterializeContext &Context) {
    auto Existing = DephicationVarDecls.find(VVar);
    if (Existing != DephicationVarDecls.end()) {
      return Existing->second;
    }

    const st::DephicationVVar *Record = nullptr;
    for (const st::DephicationVVar &Candidate : Context.DephicationVVars) {
      if (Candidate.Id == VVar) {
        Record = &Candidate;
        break;
      }
    }
    for (const st::DephicationVVar &Candidate : SharedCfg.dephicationVVars()) {
      if (Candidate.Id == VVar) {
        Record = &Candidate;
        break;
      }
    }
    if (Record == nullptr || Record->SourceId == st::InvalidVVarId) {
      return nullptr;
    }

    clang::VarDecl *Source = dephicationVarDecl(Record->SourceId);
    if (Source == nullptr) {
      return nullptr;
    }

    std::string Name = Record->Name + "_copy" + std::to_string(Record->Id);
    clang::IdentifierInfo *II = &Ctx.Idents.get(llvm::StringRef(Name));
    clang::VarDecl *Copy = clang::VarDecl::Create(
        Ctx, Source->getDeclContext(), clang::SourceLocation(),
        clang::SourceLocation(), II, Source->getType(), nullptr,
        clang::SC_None);
    clang::DeclStmt *Decl = new (Ctx) clang::DeclStmt(
        clang::DeclGroupRef(Copy), clang::SourceLocation(),
        clang::SourceLocation());
    DephicationVarDecls.emplace(VVar, Copy);
    CopiedDephicationDecls.emplace(Copy, Decl);
    return Copy;
  }

  void prependUsedCopiedDephicationDecls(std::vector<clang::Stmt *> &Stmts) {
    if (CopiedDephicationDecls.empty()) {
      return;
    }

    std::set<clang::ValueDecl *> UsedDecls;
    DeclRefCollector Collector(UsedDecls);
    for (clang::Stmt *Stmt : Stmts) {
      Collector.TraverseStmt(Stmt);
    }

    std::vector<clang::Stmt *> DeclStmts;
    for (const auto &Entry : CopiedDephicationDecls) {
      if (UsedDecls.count(Entry.first) != 0) {
        DeclStmts.push_back(Entry.second);
      }
    }
    Stmts.insert(Stmts.begin(), DeclStmts.begin(), DeclStmts.end());
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

  void collectGotoTargets(const st::StructuredTree &Tree, st::NodeId Id,
                          RenderContext Context) {
    const st::StructuredNode *Node = Tree.getNode(Id);
    if (Node == nullptr) {
      return;
    }
    if (Node->Kind == st::StructuredNodeKind::Goto &&
        Node->Target != st::InvalidBlockId) {
      TargetedLabels.insert(Node->Target);
    }
    if (Node->Kind == st::StructuredNodeKind::Break &&
        !canRenderBreak(Context, Node->Target) &&
        !canInlineTerminalTarget(Node->Target) &&
        Node->Target != st::InvalidBlockId) {
      TargetedLabels.insert(Node->Target);
    }
    if (Node->Kind == st::StructuredNodeKind::Continue &&
        !Context.Continuable && Node->Target != st::InvalidBlockId) {
      TargetedLabels.insert(Node->Target);
    }

    RenderContext ChildContext = Context;
    if (Node->Kind == st::StructuredNodeKind::While ||
        Node->Kind == st::StructuredNodeKind::DoWhile ||
        Node->Kind == st::StructuredNodeKind::InfiniteLoop) {
      ChildContext.Breakable = true;
      ChildContext.Continuable = true;
      if (Node->BreakTarget != st::InvalidBlockId) {
        ChildContext.BreakTarget = Node->BreakTarget;
      }
    }

    for (st::NodeId Child : Node->Children) {
      collectGotoTargets(Tree, Child, ChildContext);
    }
    RenderContext SwitchContext = Context;
    if (Node->Kind == st::StructuredNodeKind::Switch) {
      SwitchContext.Breakable = true;
    }
    for (const st::StructuredSwitchCase &Case : Node->StructuredCases) {
      collectGotoTargets(Tree, Case.Body, SwitchContext);
    }
    collectGotoTargets(Tree, Node->Then, Context);
    collectGotoTargets(Tree, Node->Else, Context);
    collectGotoTargets(Tree, Node->Body, ChildContext);
    collectGotoTargets(Tree, Node->Default, SwitchContext);
  }

  st::StructuredCFG buildCFG() {
    st::StructuredCFG Ret;
    std::map<std::pair<st::BlockId, std::string>, st::VVarId> DephicationVVars;

    for (CFGBlock *Block : Cfg) {
      st::CFGBlock NewBlock;
      NewBlock.Id = Block->getBlockID();
      bool HasReturnStmt = false;
      std::vector<st::PayloadRef> Statements;
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
          NewBlock.CallCount += callExprCount(Stmt);
          st::PayloadRef Payload = addPayload(Stmt);
          NewBlock.Statements.push_back(Payload);
          Statements.push_back(Payload);
          setSimpleReturnOrigin(Ret, Payload,
                                llvm::dyn_cast<clang::ReturnStmt>(Stmt));
          setSimpleCallOrigin(Ret, Payload, Stmt);
        }
      }

      if (HasReturnStmt ||
          llvm::isa_and_nonnull<clang::ReturnStmt>(Block->getTerminatorStmt())) {
        NewBlock.Terminator = st::TerminatorKind::Return;
      } else if (Block->succ_size() == 0 && Block->size() == 0 &&
                 Block->getTerminatorStmt() == nullptr) {
        // LLVM unreachable lowers to an empty, closed CFG block in this C CFG.
        // Mark only that narrow shape as a SAILR terminal.
        NewBlock.Terminator = st::TerminatorKind::Unreachable;
      } else {
        for (CFGBlock *Succ : Block->succs()) {
          NewBlock.Successors.push_back(Succ->getBlockID());
        }
      }

      if (NewBlock.Terminator == st::TerminatorKind::Return) {
        // keep terminal blocks closed so structuring can treat them as exits
      } else if (NewBlock.Terminator == st::TerminatorKind::Unreachable) {
        // keep trap blocks closed for the same reason
      } else if (Block->succ_size() == 2) {
        NewBlock.Terminator = st::TerminatorKind::Branch;
        NewBlock.Condition = addPayload(Block->getTerminatorStmt());
        if (std::optional<st::ConditionCompare> Compare =
                conditionCompareFromBranchCondition(
                    Block->getTerminatorStmt())) {
          Ret.setConditionCompare(NewBlock.Condition, *Compare);
        }
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
      if (Block->isSAILRDephicationEdge()) {
        for (const auto &Assignment : Block->getSAILRDephicationAssignments()) {
          if (Assignment.StatementIndex >= Statements.size()) {
            continue;
          }
          st::BlockId Merge = Block->getSAILRDephicationTargetBlock();
          auto Key = std::make_pair(Merge, Assignment.TargetName);
          auto It = DephicationVVars.find(Key);
          if (It == DephicationVVars.end()) {
            It = DephicationVVars
                     .emplace(Key, Ret.addDephicationVVar(Assignment.TargetName,
                                                          Merge))
                     .first;
          }
          Ret.addDephicationIncoming(
              It->second, Block->getSAILRDephicationSourceBlock(), Merge,
              Block->getBlockID(), Statements[Assignment.StatementIndex],
              Assignment.IncomingName);
          if (clang::VarDecl *Decl =
                  assignmentTargetVar(Payloads[Statements[Assignment.StatementIndex]
                                            .Id])) {
            DephicationVarDecls.emplace(It->second, Decl);
          }
        }
      }
    }

    return Ret;
  }

  void renderNode(const st::StructuredTree &Tree, st::NodeId Id,
                  std::vector<clang::Stmt *> &Stmts, RenderContext Context) {
    const st::StructuredNode *Node = Tree.getNode(Id);
    assert(Node != nullptr);

    switch (Node->Kind) {
    case st::StructuredNodeKind::Sequence:
      for (st::NodeId Child : Node->Children) {
        renderNode(Tree, Child, Stmts, Context);
      }
      break;
    case st::StructuredNodeKind::Label:
      if (TargetedLabels.count(Node->Block) != 0) {
        Stmts.push_back(getOrCreateLabelStmt(Node->Block));
      }
      break;
    case st::StructuredNodeKind::BasicBlock:
      if (InlinedTerminalTargets.count(Node->Block) != 0 &&
          TargetedLabels.count(Node->Block) == 0) {
        break;
      }
      for (st::PayloadRef Ref : Node->Statements) {
        Stmts.push_back(getPayload(Ref));
      }
      break;
    case st::StructuredNodeKind::If:
      if (Node->Children.empty() && (Node->Then != st::InvalidNodeId ||
                                     Node->Else != st::InvalidNodeId)) {
        renderIf(Tree, *Node, Stmts, Context);
      } else {
        renderIf(*Node, Stmts);
      }
      break;
    case st::StructuredNodeKind::Switch:
      if (Node->Children.empty() && (!Node->StructuredCases.empty() ||
                                     Node->Default != st::InvalidNodeId)) {
        Stmts.push_back(renderSwitch(Tree, *Node, Context));
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
      if (!canRenderBreak(Context, Node->Target) &&
          Node->Target != st::InvalidBlockId) {
        if (!renderTargetedTerminal(Node->Target, Stmts)) {
          Stmts.push_back(SA.makeGotoStmt(getOrCreateLabel(Node->Target)));
        }
      } else {
        Stmts.push_back(new (Ctx) clang::BreakStmt(clang::SourceLocation()));
      }
      break;
    case st::StructuredNodeKind::Continue:
      if (!Context.Continuable && Node->Target != st::InvalidBlockId) {
        Stmts.push_back(SA.makeGotoStmt(getOrCreateLabel(Node->Target)));
      } else {
        Stmts.push_back(new (Ctx)
                            clang::ContinueStmt(clang::SourceLocation()));
      }
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

  bool canRenderBreak(RenderContext Context, st::BlockId Target) const {
    if (!Context.Breakable) {
      return false;
    }
    return Target == st::InvalidBlockId ||
           Context.BreakTarget == st::InvalidBlockId ||
           Target == Context.BreakTarget;
  }

  bool canInlineTerminalTarget(st::BlockId Target) const {
    const st::CFGBlock *Block = SharedCfg.getBlock(Target);
    return Block != nullptr && Block->Terminator == st::TerminatorKind::Return;
  }

  bool renderTargetedTerminal(st::BlockId Target,
                              std::vector<clang::Stmt *> &Stmts) {
    if (!canInlineTerminalTarget(Target)) {
      return false;
    }

    const st::CFGBlock *Block = SharedCfg.getBlock(Target);
    const st::CFGBlock *BodyBlock = SharedCfg.getBodyBlock(Target);
    if (BodyBlock == nullptr) {
      BodyBlock = Block;
    }
    for (st::PayloadRef Ref : BodyBlock->Statements) {
      Stmts.push_back(getPayload(Ref));
    }
    InlinedTerminalTargets.insert(Target);
    return true;
  }

  clang::Stmt *renderCompound(const st::StructuredTree &Tree, st::NodeId Id,
                              RenderContext Context) {
    std::vector<clang::Stmt *> Stmts;
    renderNode(Tree, Id, Stmts, Context);
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
                std::vector<clang::Stmt *> &Stmts, RenderContext Context) {
    auto *Cond = conditionExpr(Node);
    clang::Stmt *Then = Node.Then == st::InvalidNodeId
                            ? nullptr
                            : renderCompound(Tree, Node.Then, Context);
    clang::Stmt *Else = Node.Else == st::InvalidNodeId
                            ? nullptr
                            : renderCompound(Tree, Node.Else, Context);
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
                            const st::StructuredNode &Node,
                            RenderContext Context) {
    auto *Cond = llvm::cast<clang::Expr>(getPayload(Node.Condition));
    Cond = SA.castRenderSwitchCondition(Cond);
    Context.Breakable = true;

    auto *Switch = clang::SwitchStmt::Create(
        Ctx, nullptr, nullptr, Cond, clang::SourceLocation(),
        clang::SourceLocation());
    std::vector<clang::Stmt *> Cases;
    std::vector<SwitchBodyLabel> BodyLabels;
    std::map<SwitchBodyKey, std::size_t> BodyLabelIndex;
    const st::CFGBlock *Block = getSharedBlock(Node.Block);
    st::BlockId DefaultTarget = Node.DefaultTarget;
    if (DefaultTarget == st::InvalidBlockId && !Block->Successors.empty()) {
      DefaultTarget = Block->Successors.front();
    }

    auto BodyKey = [&](st::NodeId Body, st::BlockId Target) {
      if (Target != st::InvalidBlockId && Target == DefaultTarget) {
        return SwitchBodyKey{st::StructuredNodeKind::Goto, Target,
                             st::InvalidNodeId};
      }
      return SwitchBodyKey{st::StructuredNodeKind::Sequence,
                           st::InvalidBlockId, Body};
    };

    auto AppendSwitchLabel = [&](st::NodeId Body,
                                 st::BlockId Target,
                                 clang::SwitchCase *Label) {
      SwitchBodyKey Key = BodyKey(Body, Target);
      auto It = BodyLabelIndex.find(Key);
      if (It != BodyLabelIndex.end()) {
        SwitchBodyLabel &BodyLabel = BodyLabels[It->second];
        if (auto *CaseLabel =
                llvm::dyn_cast<clang::CaseStmt>(BodyLabel.LastLabel)) {
          CaseLabel->setSubStmt(Label);
        } else {
          llvm::cast<clang::DefaultStmt>(BodyLabel.LastLabel)->setSubStmt(Label);
        }
        BodyLabel.LastLabel = Label;
      } else {
        BodyLabelIndex.emplace(Key, BodyLabels.size());
        BodyLabels.push_back({Label, Body});
        Cases.push_back(Label);
      }
      Switch->addSwitchCase(Label);
    };

    for (const auto &Case : Node.StructuredCases) {
      auto *CaseStmt = clang::CaseStmt::Create(
          Ctx, llvm::cast<clang::Expr>(getPayload(Case.Value)), nullptr,
          clang::SourceLocation(), clang::SourceLocation(),
          clang::SourceLocation());
      AppendSwitchLabel(Case.Body, Case.Target, CaseStmt);
    }

    if (Node.Default != st::InvalidNodeId) {
      auto *Default = new (Ctx) clang::DefaultStmt(
          clang::SourceLocation(), clang::SourceLocation(), nullptr);
      AppendSwitchLabel(Node.Default, DefaultTarget, Default);
    }

    for (const SwitchBodyLabel &BodyLabel : BodyLabels) {
      if (auto *CaseLabel = llvm::dyn_cast<clang::CaseStmt>(BodyLabel.LastLabel)) {
        CaseLabel->setSubStmt(renderCompound(Tree, BodyLabel.Body, Context));
      } else {
        llvm::cast<clang::DefaultStmt>(BodyLabel.LastLabel)->setSubStmt(
            renderCompound(Tree, BodyLabel.Body, Context));
      }
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
    RenderContext BodyContext{/*Breakable=*/true, /*Continuable=*/true,
                              Node.BreakTarget};
    return clang::WhileStmt::Create(
        Ctx, nullptr, conditionExpr(Node),
        renderCompound(Tree, Node.Body, BodyContext),
        clang::SourceLocation(), clang::SourceLocation(),
        clang::SourceLocation());
  }

  clang::Stmt *renderDoWhile(const st::StructuredTree &Tree,
                             const st::StructuredNode &Node) {
    RenderContext BodyContext{/*Breakable=*/true, /*Continuable=*/true,
                              Node.BreakTarget};
    return new (Ctx)
        clang::DoStmt(renderCompound(Tree, Node.Body, BodyContext),
                      conditionExpr(Node),
                      clang::SourceLocation(), clang::SourceLocation(),
                      clang::SourceLocation());
  }

  clang::Stmt *renderInfiniteLoop(const st::StructuredTree &Tree,
                                  const st::StructuredNode &Node) {
    RenderContext BodyContext{/*Breakable=*/true, /*Continuable=*/true,
                              Node.BreakTarget};
    return clang::WhileStmt::Create(
        Ctx, nullptr, trueExpr(), renderCompound(Tree, Node.Body, BodyContext),
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
