#include "notdec-backends/Structuring/RegionIdentifier.h"

namespace notdec::backend::structuring {

RegionTree RegionIdentifier::identifyRoot(const StructuredCFG &Cfg) {
  RegionTree Tree;

  Region Root;
  if (!Cfg.blocks().empty()) {
    Root.Head = Cfg.blocks().front().Id;
  }
  Root.Blocks.reserve(Cfg.blocks().size());
  for (const CFGBlock &Block : Cfg.blocks()) {
    Root.Blocks.push_back(Block.Id);
  }

  RegionId RootId = Tree.addRegion(std::move(Root));
  Tree.setRoot(RootId);
  return Tree;
}

} // namespace notdec::backend::structuring
