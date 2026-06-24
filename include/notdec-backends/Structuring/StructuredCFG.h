#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTUREDCFG_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTUREDCFG_H

#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <utility>
#include <vector>

namespace notdec::backend::structuring {

using BlockId = std::uint32_t;
using NodeId = std::uint32_t;
using PayloadId = std::size_t;

constexpr BlockId InvalidBlockId = std::numeric_limits<BlockId>::max();
constexpr NodeId InvalidNodeId = std::numeric_limits<NodeId>::max();
constexpr PayloadId InvalidPayloadId = std::numeric_limits<PayloadId>::max();

// Language backends keep their own statement/expression storage and pass stable
// ids here. The structuring layer only moves these ids around, so it does not
// depend on Clang, Solidity AST nodes, or any other frontend-specific type.
struct PayloadRef {
  PayloadId Id = InvalidPayloadId;

  bool isValid() const { return Id != InvalidPayloadId; }
};

enum class TerminatorKind {
  Fallthrough,
  Branch,
  Switch,
  Return,
  Unreachable,
};

enum class CFGBlockOrigin {
  Original,
  Copied,
  Synthetic,
};

enum class CFGBlockCopyKind {
  None,
  RegionCopy,
  SyntheticForwarder,
};

enum class CFGBlockCreator {
  Input,
  StructuredCFG,
  SAILRDeoptimization,
};

struct SwitchCase {
  PayloadRef Value;
  BlockId Target = InvalidBlockId;
};

enum class PayloadMaterializeKind {
  Statement,
  Condition,
  SwitchCaseValue,
};

struct PayloadMaterializeContext {
  BlockId SourceBlock = InvalidBlockId;
  BlockId BodyBlock = InvalidBlockId;
  BlockId CopyBlock = InvalidBlockId;
  BlockId OriginalPredecessor = InvalidBlockId;
  BlockId NewPredecessor = InvalidBlockId;
  BlockId OriginalTarget = InvalidBlockId;
  BlockId NewTarget = InvalidBlockId;
  CFGBlockCopyKind CopyKind = CFGBlockCopyKind::None;
  CFGBlockCreator CreatedBy = CFGBlockCreator::Input;
};

using PayloadMaterializeHook =
    std::function<std::optional<PayloadRef>(const PayloadMaterializeContext &,
                                            PayloadMaterializeKind, PayloadRef,
                                            std::size_t)>;

struct StructuredSwitchCase {
  PayloadRef Value;
  BlockId Target = InvalidBlockId;
  NodeId Body = InvalidNodeId;
};

struct CFGBlock {
  BlockId Id = InvalidBlockId;

  // These fields describe control-flow identity, not renderer behavior.
  // BodyBlock only says where statements come from. Origin/SourceBlock/CopyKind
  // keep copied and synthetic CFG nodes distinct for later SAILR passes.
  CFGBlockOrigin Origin = CFGBlockOrigin::Original;
  BlockId SourceBlock = InvalidBlockId;
  CFGBlockCopyKind CopyKind = CFGBlockCopyKind::None;
  CFGBlockCreator CreatedBy = CFGBlockCreator::Input;
  bool BodyMaterialized = false;
  // Synthetic forwarders stand for a virtualized original edge.
  BlockId SyntheticSource = InvalidBlockId;
  BlockId SyntheticTarget = InvalidBlockId;

  // SAILR deoptimization can duplicate or synthesize control-flow blocks.
  // BodyBlock keeps the backend-neutral identity of the block whose statements
  // and terminator payloads should be rendered for this block. Original blocks
  // point to themselves; copied blocks point to the original body.
  BlockId BodyBlock = InvalidBlockId;
  std::vector<PayloadRef> Statements;
  TerminatorKind Terminator = TerminatorKind::Fallthrough;
  PayloadRef Condition;
  std::vector<BlockId> Successors;
  std::vector<SwitchCase> Cases;
};

// Copied-region bookkeeping stays in the shared CFG so deoptimization passes
// can duplicate blocks once and then retarget the new copy graph in a uniform
// way.
struct DuplicatedRegion {
  CFGBlockCopyKind CopyKind = CFGBlockCopyKind::RegionCopy;
  CFGBlockCreator CreatedBy = CFGBlockCreator::SAILRDeoptimization;
  std::vector<std::pair<BlockId, BlockId>> Blocks;

  BlockId copyOf(BlockId Original) const;
  BlockId originalOf(BlockId Copy) const;
};

class StructuredCFG {
public:
  BlockId addBlock(CFGBlock Block);
  BlockId createSyntheticBlock(
      std::vector<BlockId> Successors,
      CFGBlockCreator Creator = CFGBlockCreator::StructuredCFG);
  BlockId createSyntheticForwarder(
      BlockId Source, BlockId Target,
      CFGBlockCreator Creator = CFGBlockCreator::StructuredCFG);
  BlockId duplicateBlock(
      BlockId Source, std::vector<BlockId> Successors,
      CFGBlockCopyKind CopyKind = CFGBlockCopyKind::RegionCopy,
      CFGBlockCreator Creator = CFGBlockCreator::SAILRDeoptimization);
  std::optional<DuplicatedRegion> duplicateRegion(
      const std::vector<BlockId> &RegionBlocks,
      CFGBlockCopyKind CopyKind = CFGBlockCopyKind::RegionCopy,
      CFGBlockCreator Creator = CFGBlockCreator::SAILRDeoptimization);

  const std::vector<CFGBlock> &blocks() const { return Blocks; }
  std::vector<CFGBlock> &blocks() { return Blocks; }

  const CFGBlock *getBlock(BlockId Id) const;
  CFGBlock *getBlock(BlockId Id);
  void setPayloadMaterializeHook(PayloadMaterializeHook Hook,
                                 bool SupportsPredecessorRewrite = false);
  bool hasPayloadMaterializeHook() const;
  bool hasPredecessorRewritePayloadMaterializeHook() const;
  BlockId bodyBlock(BlockId Id) const;
  const CFGBlock *getBodyBlock(BlockId Id) const;
  bool materializeBlockBody(BlockId Id);
  bool materializeBlockBody(BlockId Id,
                            BlockId OriginalPredecessor,
                            BlockId NewPredecessor);
  bool hasEdge(BlockId From, BlockId To) const;
  std::vector<BlockId> successorsOf(BlockId From) const;
  std::vector<BlockId> predecessorsOf(BlockId Target) const;
  bool replaceEdge(BlockId From, BlockId OldTarget, BlockId NewTarget);
  bool redirectPredecessors(BlockId OldTarget, BlockId NewTarget,
                            const std::vector<BlockId> &Preds);
  bool removeBlock(BlockId Id);
  bool removeBlocks(const std::vector<BlockId> &Ids);

private:
  BlockId nextBlockId() const;

  std::vector<CFGBlock> Blocks;
  PayloadMaterializeHook MaterializeHook;
  bool MaterializeHookSupportsPredecessorRewrite = false;
};

enum class StructuredNodeKind {
  Sequence,
  BasicBlock,
  If,
  Switch,
  Label,
  Goto,
  Break,
  Continue,
  While,
  DoWhile,
  InfiniteLoop,
  Return,
  Unreachable,
};

struct StructuredNode {
  StructuredNodeKind Kind = StructuredNodeKind::Sequence;
  BlockId Block = InvalidBlockId;
  BlockId Target = InvalidBlockId;
  BlockId BreakTarget = InvalidBlockId;
  BlockId ContinueTarget = InvalidBlockId;
  PayloadRef Condition;
  bool ConditionNegated = false;
  std::vector<PayloadRef> Statements;

  // Angr-style semantic children. Sequence still uses Children. Older fallback
  // nodes may also keep using Children until the corresponding reducer is
  // migrated.
  NodeId Then = InvalidNodeId;
  NodeId Else = InvalidNodeId;
  NodeId Body = InvalidNodeId;
  NodeId Default = InvalidNodeId;

  std::vector<SwitchCase> Cases;
  std::vector<StructuredSwitchCase> StructuredCases;
  std::vector<NodeId> Children;
};

class StructuredTree {
public:
  NodeId addNode(StructuredNode Node);

  NodeId root() const { return Root; }
  void setRoot(NodeId Id) { Root = Id; }

  const std::vector<StructuredNode> &nodes() const { return Nodes; }
  std::vector<StructuredNode> &nodes() { return Nodes; }

  const StructuredNode *getNode(NodeId Id) const;

private:
  NodeId Root = InvalidNodeId;
  std::vector<StructuredNode> Nodes;
};

} // namespace notdec::backend::structuring

#endif
