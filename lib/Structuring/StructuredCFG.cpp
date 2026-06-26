#include "notdec-backends/Structuring/StructuredCFG.h"

#include <algorithm>

namespace notdec::backend::structuring {
namespace {

bool hasTarget(const CFGBlock &Block, BlockId Target) {
  return std::find(Block.Successors.begin(), Block.Successors.end(), Target) !=
             Block.Successors.end() ||
         std::find_if(Block.Cases.begin(), Block.Cases.end(),
                      [Target](const SwitchCase &Case) {
                        return Case.Target == Target;
                      }) != Block.Cases.end();
}

void replaceTarget(CFGBlock &Block, BlockId OldTarget, BlockId NewTarget) {
  for (BlockId &Succ : Block.Successors) {
    if (Succ == OldTarget) {
      Succ = NewTarget;
    }
  }
  for (SwitchCase &Case : Block.Cases) {
    if (Case.Target == OldTarget) {
      Case.Target = NewTarget;
    }
  }
}

void appendUniqueTarget(std::vector<BlockId> &Targets, BlockId Target) {
  if (std::find(Targets.begin(), Targets.end(), Target) == Targets.end()) {
    Targets.push_back(Target);
  }
}

void appendGeneratedPayload(std::vector<PayloadRef> &Generated,
                            PayloadRef Original, PayloadRef Rewritten) {
  if (Rewritten.isValid() && Rewritten.Id != Original.Id) {
    Generated.push_back(Rewritten);
  }
}

BlockId copyOf(const DuplicatedRegion &Region, BlockId Original) {
  return Region.copyOf(Original);
}

} // namespace

BlockId StructuredCFG::addBlock(CFGBlock Block) {
  if (Block.Id == InvalidBlockId) {
    Block.Id = static_cast<BlockId>(Blocks.size());
  }
  if (Block.BodyBlock == InvalidBlockId) {
    Block.BodyBlock = Block.Id;
  }
  if (Block.SourceBlock == InvalidBlockId) {
    Block.SourceBlock = Block.Id;
  }
  if (Block.BodyBlock == Block.Id) {
    Block.BodyMaterialized = true;
  }
  BlockId Id = Block.Id;
  Blocks.push_back(std::move(Block));
  return Id;
}

BlockId StructuredCFG::createSyntheticBlock(std::vector<BlockId> Successors,
                                            CFGBlockCreator Creator) {
  CFGBlock Block;
  Block.Id = nextBlockId();
  Block.BodyBlock = Block.Id;
  Block.Origin = CFGBlockOrigin::Synthetic;
  Block.SourceBlock = Block.Id;
  Block.CopyKind = CFGBlockCopyKind::SyntheticForwarder;
  Block.CreatedBy = Creator;
  Block.Terminator = TerminatorKind::Fallthrough;
  Block.Successors = std::move(Successors);
  return addBlock(std::move(Block));
}

BlockId StructuredCFG::createSyntheticForwarder(BlockId Source, BlockId Target,
                                                CFGBlockCreator Creator) {
  BlockId Id = createSyntheticBlock({Target}, Creator);
  CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return InvalidBlockId;
  }

  Block->SyntheticSource = Source;
  Block->SyntheticTarget = Target;
  return Id;
}

BlockId StructuredCFG::createSyntheticGoto(BlockId Source, BlockId Target,
                                           CFGBlockCreator Creator) {
  BlockId Id = createSyntheticBlock({}, Creator);
  CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return InvalidBlockId;
  }

  Block->CopyKind = CFGBlockCopyKind::SyntheticGoto;
  Block->SyntheticSource = Source;
  Block->SyntheticTarget = Target;
  return Id;
}

BlockId StructuredCFG::createSyntheticGotoEdge(BlockId Source, BlockId Target,
                                               CFGBlockCreator Creator) {
  BlockId Id = createSyntheticGoto(Source, Target, Creator);
  CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return InvalidBlockId;
  }

  Block->Successors = {Target};
  return Id;
}

BlockId StructuredCFG::duplicateBlock(BlockId Source,
                                      std::vector<BlockId> Successors,
                                      CFGBlockCopyKind Kind,
                                      CFGBlockCreator Creator) {
  const CFGBlock *SourceBlock = getBlock(Source);
  if (SourceBlock == nullptr) {
    return InvalidBlockId;
  }
  if (SourceBlock->Terminator == TerminatorKind::Switch &&
      Successors != SourceBlock->Successors) {
    return InvalidBlockId;
  }

  CFGBlock Copy = *SourceBlock;
  Copy.Id = nextBlockId();
  Copy.Origin = CFGBlockOrigin::Copied;
  Copy.SourceBlock = SourceBlock->SourceBlock == InvalidBlockId
                         ? SourceBlock->Id
                         : SourceBlock->SourceBlock;
  Copy.CopiedFromBlock = SourceBlock->Id;
  Copy.CopyKind = Kind;
  Copy.CreatedBy = Creator;
  Copy.BodyMaterialized = false;
  Copy.BodyBlock = SourceBlock->BodyBlock == InvalidBlockId
                       ? SourceBlock->Id
                       : SourceBlock->BodyBlock;
  Copy.Statements.clear();
  Copy.Condition = {};
  for (SwitchCase &Case : Copy.Cases) {
    Case.Value = {};
  }
  Copy.Successors = std::move(Successors);
  BlockId SourceId = SourceBlock->Id;
  BlockId CopyId = addBlock(std::move(Copy));
  duplicateDephicationIncomings(SourceId, CopyId);
  return CopyId;
}

BlockId DuplicatedRegion::copyOf(BlockId Original) const {
  auto It = std::find_if(Blocks.begin(), Blocks.end(),
                         [Original](const std::pair<BlockId, BlockId> &Entry) {
                           return Entry.first == Original;
                         });
  return It == Blocks.end() ? InvalidBlockId : It->second;
}

BlockId DuplicatedRegion::originalOf(BlockId Copy) const {
  auto It = std::find_if(Blocks.begin(), Blocks.end(),
                         [Copy](const std::pair<BlockId, BlockId> &Entry) {
                           return Entry.second == Copy;
                         });
  return It == Blocks.end() ? InvalidBlockId : It->first;
}

const CFGBlock *StructuredCFG::getBlock(BlockId Id) const {
  for (const auto &Block : Blocks) {
    if (Block.Id == Id) {
      return &Block;
    }
  }
  return nullptr;
}

CFGBlock *StructuredCFG::getBlock(BlockId Id) {
  for (auto &Block : Blocks) {
    if (Block.Id == Id) {
      return &Block;
    }
  }
  return nullptr;
}

VVarId StructuredCFG::addDephicationVVar(std::string Name,
                                         BlockId MergeBlock) {
  VVarId Id = static_cast<VVarId>(DephicationVVars.size());
  DephicationVVars.push_back(
      {.Id = Id, .Name = std::move(Name), .MergeBlock = MergeBlock});
  return Id;
}

void StructuredCFG::addDephicationIncoming(VVarId Target,
                                           BlockId IncomingBlock,
                                           BlockId MergeBlock,
                                           BlockId EdgeBlock,
                                           PayloadRef Assignment,
                                           std::string IncomingName) {
  DephicationIncomings.push_back({.Target = Target,
                                  .IncomingBlock = IncomingBlock,
                                  .MergeBlock = MergeBlock,
                                  .EdgeBlock = EdgeBlock,
                                  .SourceIncomingBlock = IncomingBlock,
                                  .SourceMergeBlock = MergeBlock,
                                  .SourceEdgeBlock = EdgeBlock,
                                  .Assignment = Assignment,
                                  .IncomingName = std::move(IncomingName)});
}

void StructuredCFG::setPayloadMaterializeHook(
    PayloadMaterializeHook Hook, bool SupportsPredecessorRewrite,
    bool SupportsGroupedPredecessorRewrite) {
  MaterializeHook = std::move(Hook);
  MaterializeHookSupportsPredecessorRewrite =
      SupportsPredecessorRewrite && static_cast<bool>(MaterializeHook);
  MaterializeHookSupportsGroupedPredecessorRewrite =
      SupportsGroupedPredecessorRewrite && static_cast<bool>(MaterializeHook);
}

void StructuredCFG::setPayloadMaterializeResultHook(
    PayloadMaterializeResultHook Hook) {
  MaterializeResultHook = std::move(Hook);
}

bool StructuredCFG::hasPayloadMaterializeHook() const {
  return static_cast<bool>(MaterializeHook);
}

bool StructuredCFG::hasPredecessorRewritePayloadMaterializeHook() const {
  return MaterializeHookSupportsPredecessorRewrite;
}

bool StructuredCFG::hasGroupedPredecessorRewritePayloadMaterializeHook()
    const {
  return MaterializeHookSupportsGroupedPredecessorRewrite;
}

BlockId StructuredCFG::bodyBlock(BlockId Id) const {
  const CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return InvalidBlockId;
  }
  return Block->BodyBlock == InvalidBlockId ? Block->Id : Block->BodyBlock;
}

const CFGBlock *StructuredCFG::getBodyBlock(BlockId Id) const {
  return getBlock(bodyBlock(Id));
}

bool StructuredCFG::materializeBlockBody(BlockId Id) {
  return materializeBlockBodyImpl(Id, {}, {});
}

bool StructuredCFG::materializeBlockBody(BlockId Id,
                                         BlockId OriginalPredecessor,
                                         BlockId NewPredecessor) {
  if (OriginalPredecessor != InvalidBlockId &&
      NewPredecessor != InvalidBlockId) {
    return materializeBlockBodyImpl(Id, {OriginalPredecessor},
                                    {NewPredecessor});
  }
  return materializeBlockBodyImpl(Id, {}, {});
}

bool StructuredCFG::materializeBlockBody(
    BlockId Id, std::vector<BlockId> OriginalPredecessors,
    std::vector<BlockId> NewPredecessors) {
  return materializeBlockBodyImpl(Id, std::move(OriginalPredecessors),
                                  std::move(NewPredecessors));
}

bool StructuredCFG::materializeBlockBodyImpl(
    BlockId Id, std::vector<BlockId> OriginalPredecessors,
    std::vector<BlockId> NewPredecessors) {
  if (OriginalPredecessors.size() != NewPredecessors.size()) {
    return false;
  }

  CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return false;
  }
  BlockId BodyId = bodyBlock(Id);
  PayloadMaterializeContext Context;
  Context.SourceBlock = Block->SourceBlock;
  Context.BodyBlock = BodyId;
  Context.CopyBlock = Id;
  Context.CopiedFromBlock = Block->CopiedFromBlock;
  Context.SyntheticSource = Block->SyntheticSource;
  Context.SyntheticTarget = Block->SyntheticTarget;
  Context.CopyKind = Block->CopyKind;
  Context.CreatedBy = Block->CreatedBy;
  Context.DephicationIncomings = dephicationIncomingsForEdge(Id);
  Context.DephicationVVars =
      dephicationVVarsForIncomings(Context.DephicationIncomings);
  if (BodyId == Id) {
    Context.OriginalCases = Block->Cases;
    Context.NewCases = Block->Cases;
    Context.OriginalSuccessors = Block->Successors;
    Context.NewSuccessors = Block->Successors;
    Context.OriginalTerminator = Block->Terminator;
    Context.NewTerminator = Block->Terminator;
    Block->BodyBlock = Id;
    Block->BodyMaterialized = true;
    if (MaterializeResultHook) {
      MaterializeResultHook(Context, PayloadMaterializeResult::Committed, {});
    }
    return true;
  }

  const CFGBlock *Body = getBlock(BodyId);
  if (Body == nullptr) {
    return false;
  }
  if (Block->Cases.size() != Body->Cases.size()) {
    return false;
  }

  if (!MaterializeHook) {
    // Fast path for current production backends. CFG identity stays on this
    // block: successors and switch targets must keep any copied-region rewrites
    // that were already applied.
    Context.OriginalCases = Body->Cases;
    Context.NewCases = Block->Cases;
    Context.OriginalSuccessors = Body->Successors;
    Context.NewSuccessors = Block->Successors;
    Context.OriginalTerminator = Body->Terminator;
    Context.NewTerminator = Block->Terminator;
    Block->Statements = Body->Statements;
    Block->Terminator = Body->Terminator;
    Block->Condition = Body->Condition;
    for (std::size_t I = 0, E = Block->Cases.size(); I != E; ++I) {
      Block->Cases[I].Value = Body->Cases[I].Value;
    }

    Block->BodyBlock = Id;
    Block->BodyMaterialized = true;
    if (MaterializeResultHook) {
      MaterializeResultHook(Context, PayloadMaterializeResult::Committed, {});
    }
    return true;
  }

  Context.SourceBlock = Body->SourceBlock;
  if (!OriginalPredecessors.empty()) {
    Context.OriginalPredecessor = OriginalPredecessors.front();
  }
  if (!NewPredecessors.empty()) {
    Context.NewPredecessor = NewPredecessors.front();
  }
  Context.OriginalPredecessors = std::move(OriginalPredecessors);
  Context.NewPredecessors = std::move(NewPredecessors);
  Context.OriginalCases = Body->Cases;
  Context.NewCases = Block->Cases;
  Context.OriginalSuccessors = Body->Successors;
  Context.NewSuccessors = Block->Successors;
  Context.OriginalTerminator = Body->Terminator;
  Context.NewTerminator = Block->Terminator;
  Context.CopyKind = Block->CopyKind;
  Context.CreatedBy = Block->CreatedBy;
  Context.DephicationIncomings = dephicationIncomingsForEdge(Id);
  Context.DephicationVVars =
      dephicationVVarsForIncomings(Context.DephicationIncomings);

  std::vector<PayloadRef> GeneratedPayloads;
  auto AbortMaterialize = [&]() {
    if (MaterializeResultHook) {
      MaterializeResultHook(Context, PayloadMaterializeResult::Aborted,
                            GeneratedPayloads);
    }
    return false;
  };

  std::vector<PayloadRef> Statements;
  Statements.reserve(Body->Statements.size());
  for (std::size_t I = 0; I < Body->Statements.size(); ++I) {
    PayloadRef Payload = Body->Statements[I];
    if (MaterializeHook) {
      std::optional<PayloadRef> Rewritten = MaterializeHook(
          Context, PayloadMaterializeKind::Statement, Payload, I);
      if (!Rewritten.has_value()) {
        return AbortMaterialize();
      }
      appendGeneratedPayload(GeneratedPayloads, Payload, *Rewritten);
      Payload = *Rewritten;
    }
    Statements.push_back(Payload);
  }

  PayloadRef Condition = Body->Condition;
  if (MaterializeHook) {
    std::optional<PayloadRef> Rewritten = MaterializeHook(
        Context, PayloadMaterializeKind::Condition, Condition, 0);
    if (!Rewritten.has_value()) {
      return AbortMaterialize();
    }
    appendGeneratedPayload(GeneratedPayloads, Condition, *Rewritten);
    Condition = *Rewritten;
  }

  std::vector<PayloadRef> CaseValues;
  CaseValues.reserve(Body->Cases.size());
  for (std::size_t I = 0; I < Body->Cases.size(); ++I) {
    PayloadRef Payload = Body->Cases[I].Value;
    if (MaterializeHook) {
      Context.OriginalTarget = Body->Cases[I].Target;
      Context.NewTarget = Block->Cases[I].Target;
      std::optional<PayloadRef> Rewritten = MaterializeHook(
          Context, PayloadMaterializeKind::SwitchCaseValue, Payload, I);
      if (!Rewritten.has_value()) {
        return AbortMaterialize();
      }
      appendGeneratedPayload(GeneratedPayloads, Payload, *Rewritten);
      Payload = *Rewritten;
    }
    CaseValues.push_back(Payload);
  }
  Context.OriginalTarget = InvalidBlockId;
  Context.NewTarget = InvalidBlockId;

  // Materializing copies renderer payload from the body source. CFG identity
  // stays on this block: successors and switch targets must keep any copied
  // region rewrites that were already applied.
  std::vector<PayloadRef> OriginalStatements = Body->Statements;
  Block->Statements = std::move(Statements);
  Block->Terminator = Body->Terminator;
  Block->Condition = Condition;
  for (std::size_t I = 0, E = Block->Cases.size(); I != E; ++I) {
    Block->Cases[I].Value = CaseValues[I];
  }

  Block->BodyBlock = Id;
  Block->BodyMaterialized = true;
  rewriteDephicationIncomingAssignments(Id, OriginalStatements,
                                        Block->Statements);
  if (MaterializeResultHook) {
    MaterializeResultHook(Context, PayloadMaterializeResult::Committed,
                          GeneratedPayloads);
  }
  return true;
}

bool StructuredCFG::hasEdge(BlockId From, BlockId To) const {
  const CFGBlock *Block = getBlock(From);
  return Block != nullptr && hasTarget(*Block, To);
}

std::vector<BlockId> StructuredCFG::successorsOf(BlockId From) const {
  std::vector<BlockId> Succs;
  const CFGBlock *Block = getBlock(From);
  if (Block == nullptr) {
    return Succs;
  }
  Succs.reserve(Block->Successors.size() + Block->Cases.size());
  for (BlockId Succ : Block->Successors) {
    appendUniqueTarget(Succs, Succ);
  }
  for (const SwitchCase &Case : Block->Cases) {
    appendUniqueTarget(Succs, Case.Target);
  }
  return Succs;
}

std::vector<BlockId> StructuredCFG::predecessorsOf(BlockId Target) const {
  std::vector<BlockId> Preds;
  for (const CFGBlock &Block : Blocks) {
    if (hasTarget(Block, Target)) {
      Preds.push_back(Block.Id);
    }
  }
  return Preds;
}

std::optional<DuplicatedRegion>
StructuredCFG::duplicateRegion(const std::vector<BlockId> &RegionBlocks,
                               CFGBlockCopyKind Kind,
                               CFGBlockCreator Creator) {
  DuplicatedRegion Region;
  std::vector<BlockId> Copies;
  Region.CopyKind = Kind;
  Region.CreatedBy = Creator;
  Region.Blocks.reserve(RegionBlocks.size());
  Copies.reserve(RegionBlocks.size());

  auto DropCopies = [&]() {
    removeDephicationBlockReferences(Copies);
    Blocks.erase(std::remove_if(Blocks.begin(), Blocks.end(),
                                [&](const CFGBlock &Block) {
                                  return std::find(Copies.begin(), Copies.end(),
                                                   Block.Id) != Copies.end();
                                }),
                 Blocks.end());
  };

  for (BlockId Original : RegionBlocks) {
    const CFGBlock *OriginalBlock = getBlock(Original);
    if (OriginalBlock == nullptr) {
      DropCopies();
      return std::nullopt;
    }

    BlockId Copy = duplicateBlock(Original, OriginalBlock->Successors, Kind,
                                  Creator);
    if (Copy == InvalidBlockId) {
      DropCopies();
      return std::nullopt;
    }
    Region.Blocks.push_back({Original, Copy});
    Copies.push_back(Copy);
  }

  for (BlockId Original : RegionBlocks) {
    const CFGBlock *OriginalBlock = getBlock(Original);
    CFGBlock *CopyBlock = getBlock(copyOf(Region, Original));
    if (OriginalBlock == nullptr || CopyBlock == nullptr) {
      DropCopies();
      return std::nullopt;
    }

    CopyBlock->Successors.clear();
    for (BlockId Succ : OriginalBlock->Successors) {
      BlockId CopiedSucc = copyOf(Region, Succ);
      CopyBlock->Successors.push_back(CopiedSucc == InvalidBlockId ? Succ
                                                                   : CopiedSucc);
    }
    for (SwitchCase &Case : CopyBlock->Cases) {
      BlockId CopiedTarget = copyOf(Region, Case.Target);
      if (CopiedTarget != InvalidBlockId) {
        Case.Target = CopiedTarget;
      }
    }
  }

  rewriteCopiedDephicationIncomings(Region);
  return Region;
}

bool StructuredCFG::replaceEdge(BlockId From, BlockId OldTarget,
                                BlockId NewTarget) {
  CFGBlock *Block = getBlock(From);
  if (Block == nullptr || !hasTarget(*Block, OldTarget)) {
    return false;
  }
  replaceTarget(*Block, OldTarget, NewTarget);
  return true;
}

bool StructuredCFG::redirectPredecessors(BlockId OldTarget, BlockId NewTarget,
                                         const std::vector<BlockId> &Preds) {
  for (BlockId Pred : Preds) {
    const CFGBlock *PredBlock = getBlock(Pred);
    if (PredBlock == nullptr || !hasTarget(*PredBlock, OldTarget)) {
      return false;
    }
  }

  for (BlockId Pred : Preds) {
    replaceEdge(Pred, OldTarget, NewTarget);
  }
  return true;
}

bool StructuredCFG::removeBlock(BlockId Id) {
  StructuredCFG Candidate = *this;
  if (!Candidate.removeBlockInPlace(Id)) {
    return false;
  }

  Blocks = std::move(Candidate.Blocks);
  DephicationIncomings = std::move(Candidate.DephicationIncomings);
  return true;
}

bool StructuredCFG::removeBlockInPlace(BlockId Id) {
  auto It = std::find_if(Blocks.begin(), Blocks.end(),
                         [Id](const CFGBlock &Block) {
                           return Block.Id == Id;
                         });
  if (It == Blocks.end()) {
    return false;
  }

  std::vector<BlockId> BodyUsers;
  for (const CFGBlock &Block : Blocks) {
    if (Block.Id != Id && Block.BodyBlock == Id) {
      if (Block.Cases.size() != It->Cases.size()) {
        return false;
      }
      BodyUsers.push_back(Block.Id);
    }
  }

  for (BlockId BodyUser : BodyUsers) {
    if (!materializeBlockBody(BodyUser)) {
      return false;
    }
  }

  for (CFGBlock &Block : Blocks) {
    if (Block.Id != Id && Block.BodyBlock == Id) {
      return false;
    }
    Block.Successors.erase(
        std::remove(Block.Successors.begin(), Block.Successors.end(), Id),
        Block.Successors.end());
    Block.Cases.erase(
        std::remove_if(Block.Cases.begin(), Block.Cases.end(),
                       [Id](const SwitchCase &Case) {
                         return Case.Target == Id;
                       }),
        Block.Cases.end());
  }

  Blocks.erase(It);
  removeDephicationBlockReferences({Id});
  return true;
}

bool StructuredCFG::removeBlocks(const std::vector<BlockId> &Ids) {
  StructuredCFG Candidate = *this;
  for (BlockId Id : Ids) {
    if (!Candidate.removeBlock(Id)) {
      return false;
    }
  }

  Blocks = std::move(Candidate.Blocks);
  DephicationIncomings = std::move(Candidate.DephicationIncomings);
  return true;
}

void StructuredCFG::duplicateDephicationIncomings(BlockId SourceEdgeBlock,
                                                  BlockId CopyEdgeBlock) {
  std::vector<DephicationIncoming> Copies;
  for (const DephicationIncoming &Incoming : DephicationIncomings) {
    if (Incoming.EdgeBlock != SourceEdgeBlock) {
      continue;
    }
    DephicationIncoming Copy = Incoming;
    Copy.SourceIncomingBlock = Incoming.IncomingBlock;
    Copy.SourceMergeBlock = Incoming.MergeBlock;
    Copy.SourceEdgeBlock = Incoming.EdgeBlock;
    Copy.EdgeBlock = CopyEdgeBlock;
    Copies.push_back(std::move(Copy));
  }
  DephicationIncomings.insert(DephicationIncomings.end(), Copies.begin(),
                              Copies.end());
}

void StructuredCFG::rewriteCopiedDephicationIncomings(
    const DuplicatedRegion &Region) {
  for (DephicationIncoming &Incoming : DephicationIncomings) {
    const CFGBlock *EdgeBlock = getBlock(Incoming.EdgeBlock);
    if (EdgeBlock == nullptr || EdgeBlock->Origin != CFGBlockOrigin::Copied) {
      continue;
    }
    BlockId SourceIncoming = Incoming.SourceIncomingBlock;
    if (SourceIncoming == InvalidBlockId) {
      SourceIncoming = Incoming.IncomingBlock;
    }
    BlockId SourceMerge = Incoming.SourceMergeBlock;
    if (SourceMerge == InvalidBlockId) {
      SourceMerge = Incoming.MergeBlock;
    }

    BlockId CopiedIncoming = Region.copyOf(SourceIncoming);
    if (CopiedIncoming != InvalidBlockId) {
      Incoming.IncomingBlock = CopiedIncoming;
    }
    BlockId CopiedMerge = Region.copyOf(SourceMerge);
    if (CopiedMerge != InvalidBlockId) {
      Incoming.MergeBlock = CopiedMerge;
    }
  }
}

void StructuredCFG::rewriteDephicationIncomingAssignments(
    BlockId EdgeBlock, const std::vector<PayloadRef> &OriginalStatements,
    const std::vector<PayloadRef> &RewrittenStatements) {
  if (OriginalStatements.size() != RewrittenStatements.size()) {
    return;
  }

  for (DephicationIncoming &Incoming : DephicationIncomings) {
    if (Incoming.EdgeBlock != EdgeBlock) {
      continue;
    }
    for (std::size_t I = 0; I < OriginalStatements.size(); ++I) {
      if (Incoming.Assignment.Id == OriginalStatements[I].Id) {
        Incoming.Assignment = RewrittenStatements[I];
        break;
      }
    }
  }
}

void StructuredCFG::removeDephicationBlockReferences(
    const std::vector<BlockId> &Ids) {
  auto MentionsRemovedBlock = [&](const DephicationIncoming &Incoming) {
    return std::find(Ids.begin(), Ids.end(), Incoming.IncomingBlock) !=
               Ids.end() ||
           std::find(Ids.begin(), Ids.end(), Incoming.MergeBlock) !=
               Ids.end() ||
           std::find(Ids.begin(), Ids.end(), Incoming.EdgeBlock) != Ids.end();
  };
  DephicationIncomings.erase(
      std::remove_if(DephicationIncomings.begin(), DephicationIncomings.end(),
                     MentionsRemovedBlock),
      DephicationIncomings.end());
}

std::vector<DephicationIncoming>
StructuredCFG::dephicationIncomingsForEdge(BlockId EdgeBlock) const {
  std::vector<DephicationIncoming> Result;
  for (const DephicationIncoming &Incoming : DephicationIncomings) {
    if (Incoming.EdgeBlock == EdgeBlock) {
      Result.push_back(Incoming);
    }
  }
  return Result;
}

std::vector<DephicationVVar>
StructuredCFG::dephicationVVarsForIncomings(
    const std::vector<DephicationIncoming> &Incomings) const {
  std::vector<DephicationVVar> Result;
  for (const DephicationIncoming &Incoming : Incomings) {
    auto It = std::find_if(
        DephicationVVars.begin(), DephicationVVars.end(),
        [&](const DephicationVVar &VVar) { return VVar.Id == Incoming.Target; });
    if (It != DephicationVVars.end()) {
      Result.push_back(*It);
    }
  }
  return Result;
}

BlockId StructuredCFG::nextBlockId() const {
  BlockId Next = 0;
  for (const CFGBlock &Block : Blocks) {
    if (Block.Id != InvalidBlockId && Block.Id >= Next) {
      Next = Block.Id + 1;
    }
  }
  return Next;
}

NodeId StructuredTree::addNode(StructuredNode Node) {
  NodeId Id = static_cast<NodeId>(Nodes.size());
  Nodes.push_back(std::move(Node));
  return Id;
}

const StructuredNode *StructuredTree::getNode(NodeId Id) const {
  if (Id >= Nodes.size()) {
    return nullptr;
  }
  return &Nodes[Id];
}

} // namespace notdec::backend::structuring
