#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTUREDCFG_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTUREDCFG_H

#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <unordered_map>
#include <vector>

namespace notdec::backend::structuring {

using BlockId = std::uint32_t;
using NodeId = std::uint32_t;
using PayloadId = std::size_t;
using VVarId = std::uint32_t;

constexpr BlockId InvalidBlockId = std::numeric_limits<BlockId>::max();
constexpr NodeId InvalidNodeId = std::numeric_limits<NodeId>::max();
constexpr PayloadId InvalidPayloadId = std::numeric_limits<PayloadId>::max();
constexpr VVarId InvalidVVarId = std::numeric_limits<VVarId>::max();

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
  SyntheticGoto,
};

enum class CFGBlockCreator {
  Input,
  StructuredCFG,
  SAILRDeoptimization,
  SAILRDephication,
};

struct SwitchCase {
  PayloadRef Value;
  BlockId Target = InvalidBlockId;
};

enum class PayloadMaterializeKind {
  Statement,
  Condition,
  SwitchCaseValue,
  DephicationAssignment,
};

enum class PayloadMaterializeResult {
  Committed,
  Aborted,
};

// Angr-style dephication needs a shared variable identity before any renderer
// sees the body. This table records the Phi destination as a vvar and keeps
// incoming assignments tied to CFG edge blocks, so later copy/materialize passes
// can rewrite payloads without asking C or Solidity to infer Phi semantics.
struct DephicationVVar {
  VVarId Id = InvalidVVarId;
  // Original logical vvar for copied merge blocks. Originals point to
  // themselves so consumers can build copy maps without matching by name.
  VVarId SourceId = InvalidVVarId;
  std::string Name;
  BlockId MergeBlock = InvalidBlockId;
  BlockId SourceMergeBlock = InvalidBlockId;
  bool Retired = false;
};

struct DephicationIncoming {
  VVarId Target = InvalidVVarId;
  VVarId SourceTarget = InvalidVVarId;
  BlockId IncomingBlock = InvalidBlockId;
  BlockId MergeBlock = InvalidBlockId;
  BlockId EdgeBlock = InvalidBlockId;
  BlockId SourceIncomingBlock = InvalidBlockId;
  BlockId SourceMergeBlock = InvalidBlockId;
  BlockId SourceEdgeBlock = InvalidBlockId;
  PayloadRef Assignment;
  std::string IncomingName;
};

struct DephicationEdgeContext {
  std::map<VVarId, VVarId> VVarCopies;
  std::vector<DephicationVVar> VVars;
  std::vector<DephicationIncoming> Incomings;
};

struct PayloadMaterializeContext {
  BlockId SourceBlock = InvalidBlockId;
  BlockId BodyBlock = InvalidBlockId;
  BlockId CopyBlock = InvalidBlockId;
  BlockId CopiedFromBlock = InvalidBlockId;
  BlockId SyntheticSource = InvalidBlockId;
  BlockId SyntheticTarget = InvalidBlockId;
  BlockId OriginalPredecessor = InvalidBlockId;
  BlockId NewPredecessor = InvalidBlockId;
  std::vector<BlockId> OriginalPredecessors;
  std::vector<BlockId> NewPredecessors;
  BlockId OriginalTarget = InvalidBlockId;
  BlockId NewTarget = InvalidBlockId;
  std::vector<SwitchCase> OriginalCases;
  std::vector<SwitchCase> NewCases;
  std::vector<BlockId> OriginalSuccessors;
  std::vector<BlockId> NewSuccessors;
  TerminatorKind OriginalTerminator = TerminatorKind::Fallthrough;
  TerminatorKind NewTerminator = TerminatorKind::Fallthrough;
  CFGBlockCopyKind CopyKind = CFGBlockCopyKind::None;
  CFGBlockCreator CreatedBy = CFGBlockCreator::Input;
  // Current-edge map from the original vvar id to the copied vvar id.
  std::map<VVarId, VVarId> DephicationVVarCopies;
  std::vector<DephicationVVar> DephicationVVars;
  std::vector<DephicationIncoming> DephicationIncomings;
  std::optional<DephicationIncoming> CurrentDephicationIncoming;
};

using PayloadMaterializeHook =
    std::function<std::optional<PayloadRef>(const PayloadMaterializeContext &,
                                            PayloadMaterializeKind, PayloadRef,
                                            std::size_t)>;
using PayloadMaterializeResultHook =
    std::function<void(const PayloadMaterializeContext &,
                       PayloadMaterializeResult,
                       const std::vector<PayloadRef> &)>;

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
  // Immediate source of this copy. SourceBlock keeps the original semantic
  // source across copy-of-copy chains; CopiedFromBlock records the concrete
  // block that was duplicated this time.
  BlockId CopiedFromBlock = InvalidBlockId;
  // Synthetic control-flow blocks keep the original source-target edge identity.
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
  BlockId createSyntheticGoto(
      BlockId Source, BlockId Target,
      CFGBlockCreator Creator = CFGBlockCreator::StructuredCFG);
  BlockId createSyntheticGotoEdge(
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
  const std::vector<DephicationVVar> &dephicationVVars() const {
    return DephicationVVars;
  }
  const std::vector<DephicationIncoming> &dephicationIncomings() const {
    return DephicationIncomings;
  }
  PayloadId payloadOrigin(PayloadId Id) const;
  DephicationEdgeContext dephicationEdgeContext(BlockId EdgeBlock) const;
  DephicationEdgeContext dephicationBlockContext(BlockId Block) const;

  const CFGBlock *getBlock(BlockId Id) const;
  CFGBlock *getBlock(BlockId Id);
  VVarId addDephicationVVar(std::string Name, BlockId MergeBlock);
  void addDephicationIncoming(VVarId Target, BlockId IncomingBlock,
                              BlockId MergeBlock, BlockId EdgeBlock,
                              PayloadRef Assignment,
                              std::string IncomingName);
  void setPayloadMaterializeHook(PayloadMaterializeHook Hook,
                                 bool SupportsPredecessorRewrite = false,
                                 bool SupportsGroupedPredecessorRewrite = false);
  void setPayloadMaterializeResultHook(PayloadMaterializeResultHook Hook);
  bool hasPayloadMaterializeHook() const;
  bool hasPredecessorRewritePayloadMaterializeHook() const;
  bool hasGroupedPredecessorRewritePayloadMaterializeHook() const;
  void setPayloadOrigin(PayloadId Id, PayloadId SourceId);
  BlockId bodyBlock(BlockId Id) const;
  const CFGBlock *getBodyBlock(BlockId Id) const;
  bool materializeBlockBody(BlockId Id);
  bool materializeBlockBody(BlockId Id,
                            BlockId OriginalPredecessor,
                            BlockId NewPredecessor);
  bool materializeBlockBody(BlockId Id,
                            std::vector<BlockId> OriginalPredecessors,
                            std::vector<BlockId> NewPredecessors);
  bool hasEdge(BlockId From, BlockId To) const;
  std::vector<BlockId> successorsOf(BlockId From) const;
  std::vector<BlockId> predecessorsOf(BlockId Target) const;
  bool replaceEdge(BlockId From, BlockId OldTarget, BlockId NewTarget);
  bool redirectPredecessors(BlockId OldTarget, BlockId NewTarget,
                            const std::vector<BlockId> &Preds);
  bool redirectDephicationIncomingTarget(BlockId EdgeBlock, BlockId OldMerge,
                                         BlockId NewMerge);
  bool removeBlock(BlockId Id);
  bool removeBlocks(const std::vector<BlockId> &Ids);

private:
  bool materializeBlockBodyImpl(BlockId Id,
                                std::vector<BlockId> OriginalPredecessors,
                                std::vector<BlockId> NewPredecessors);
  void duplicateDephicationIncomings(BlockId SourceEdgeBlock,
                                     BlockId CopyEdgeBlock);
  // Copy the vvar identity for a copied merge block so the copied edge can
  // point at its own shared variable record.
  std::map<VVarId, VVarId>
  duplicateDephicationVVars(const DuplicatedRegion &Region);
  void rewriteCopiedDephicationIncomings(const DuplicatedRegion &Region);
  void rewriteCopiedDephicationIncomingTargets(
      const std::map<VVarId, VVarId> &CopiedVVars);
  std::map<VVarId, VVarId>
  dephicationVVarCopiesForIncomings(
      const std::vector<DephicationIncoming> &Incomings) const;
  void rewriteDephicationIncomingAssignments(
      BlockId EdgeBlock, const std::vector<PayloadRef> &OriginalStatements,
      const std::vector<PayloadRef> &RewrittenStatements);
  void removeDephicationVVarReferences(const std::vector<BlockId> &Ids);
  void removeDephicationBlockReferences(const std::vector<BlockId> &Ids);
  std::vector<DephicationIncoming>
  dephicationIncomingsForEdge(BlockId EdgeBlock) const;
  std::vector<DephicationVVar>
  dephicationVVarsForIncomings(const std::vector<DephicationIncoming> &Incomings)
      const;
  std::vector<DephicationVVar> dephicationVVarsForMerge(BlockId MergeBlock)
      const;
  std::map<VVarId, VVarId> dephicationVVarCopiesForVVars(
      const std::vector<DephicationVVar> &VVars) const;
  BlockId nextBlockId() const;
  bool removeBlockInPlace(BlockId Id);

  std::vector<CFGBlock> Blocks;
  std::vector<DephicationVVar> DephicationVVars;
  std::vector<DephicationIncoming> DephicationIncomings;
  PayloadMaterializeHook MaterializeHook;
  PayloadMaterializeResultHook MaterializeResultHook;
  bool MaterializeHookSupportsPredecessorRewrite = false;
  bool MaterializeHookSupportsGroupedPredecessorRewrite = false;
  std::unordered_map<PayloadId, PayloadId> PayloadOrigins;
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
