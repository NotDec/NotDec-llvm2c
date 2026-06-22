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
  return addBlock(std::move(Copy));
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
  CFGBlock *Block = getBlock(Id);
  if (Block == nullptr) {
    return false;
  }
  BlockId BodyId = bodyBlock(Id);
  if (BodyId == Id) {
    Block->BodyBlock = Id;
    Block->BodyMaterialized = true;
    return true;
  }

  const CFGBlock *Body = getBlock(BodyId);
  if (Body == nullptr) {
    return false;
  }
  if (Block->Cases.size() != Body->Cases.size()) {
    return false;
  }

  // Materializing copies renderer payload from the body source. CFG identity
  // stays on this block: successors and switch targets must keep any copied
  // region rewrites that were already applied.
  Block->Statements = Body->Statements;
  Block->Terminator = Body->Terminator;
  Block->Condition = Body->Condition;
  for (std::size_t I = 0, E = Block->Cases.size(); I != E; ++I) {
    Block->Cases[I].Value = Body->Cases[I].Value;
  }

  Block->BodyBlock = Id;
  Block->BodyMaterialized = true;
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

  for (BlockId Original : RegionBlocks) {
    const CFGBlock *OriginalBlock = getBlock(Original);
    if (OriginalBlock == nullptr) {
      for (BlockId OldCopy : Copies) {
        removeBlock(OldCopy);
      }
      return std::nullopt;
    }

    BlockId Copy = duplicateBlock(Original, OriginalBlock->Successors, Kind,
                                  Creator);
    if (Copy == InvalidBlockId) {
      for (BlockId OldCopy : Copies) {
        removeBlock(OldCopy);
      }
      return std::nullopt;
    }
    Region.Blocks.push_back({Original, Copy});
    Copies.push_back(Copy);
  }

  for (BlockId Original : RegionBlocks) {
    const CFGBlock *OriginalBlock = getBlock(Original);
    CFGBlock *CopyBlock = getBlock(copyOf(Region, Original));
    if (OriginalBlock == nullptr || CopyBlock == nullptr) {
      for (BlockId OldCopy : Copies) {
        removeBlock(OldCopy);
      }
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
  auto It = std::find_if(Blocks.begin(), Blocks.end(),
                         [Id](const CFGBlock &Block) {
                           return Block.Id == Id;
                         });
  if (It == Blocks.end()) {
    return false;
  }

  for (CFGBlock &Block : Blocks) {
    if (Block.BodyBlock == Id) {
      materializeBlockBody(Block.Id);
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
  return true;
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
