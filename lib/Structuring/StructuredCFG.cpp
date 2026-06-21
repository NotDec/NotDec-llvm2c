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

} // namespace

BlockId StructuredCFG::addBlock(CFGBlock Block) {
  if (Block.Id == InvalidBlockId) {
    Block.Id = static_cast<BlockId>(Blocks.size());
  }
  if (Block.BodyBlock == InvalidBlockId) {
    Block.BodyBlock = Block.Id;
  }
  BlockId Id = Block.Id;
  Blocks.push_back(std::move(Block));
  return Id;
}

BlockId StructuredCFG::duplicateBlock(BlockId Source,
                                      std::vector<BlockId> Successors) {
  const CFGBlock *SourceBlock = getBlock(Source);
  if (SourceBlock == nullptr) {
    return InvalidBlockId;
  }

  CFGBlock Copy = *SourceBlock;
  Copy.Id = nextBlockId();
  Copy.BodyBlock = SourceBlock->BodyBlock == InvalidBlockId
                       ? SourceBlock->Id
                       : SourceBlock->BodyBlock;
  Copy.Successors = std::move(Successors);
  return addBlock(std::move(Copy));
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
    Succs.push_back(Succ);
  }
  for (const SwitchCase &Case : Block->Cases) {
    Succs.push_back(Case.Target);
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
      Block.BodyBlock = Block.Id;
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
