#ifndef NOTDEC_BACKENDS_STRUCTURING_GOTOMANAGER_H
#define NOTDEC_BACKENDS_STRUCTURING_GOTOMANAGER_H

#include "notdec-backends/Structuring/StructuredCFG.h"

#include <set>
#include <vector>

namespace notdec::backend::structuring {

// Minimal Angr-style goto summary for shared structuring output. It is kept in
// the backend-independent layer so SAILR deoptimization can reason about gotos
// before either the C or Solidity renderer sees the tree.
struct StructuredGoto {
  BlockId Source = InvalidBlockId;
  BlockId Target = InvalidBlockId;
  NodeId Node = InvalidNodeId;

  bool operator<(const StructuredGoto &Other) const {
    if (Source != Other.Source) {
      return Source < Other.Source;
    }
    if (Target != Other.Target) {
      return Target < Other.Target;
    }
    return Node < Other.Node;
  }
};

class GotoManager {
public:
  static GotoManager collect(const StructuredTree &Tree);

  const std::set<StructuredGoto> &gotos() const { return Gotos; }
  bool empty() const { return Gotos.empty(); }
  std::size_t size() const { return Gotos.size(); }

  std::vector<StructuredGoto> gotosInBlock(BlockId Source) const;
  bool isGotoEdge(BlockId Source, BlockId Target) const;

private:
  std::set<StructuredGoto> Gotos;
};

} // namespace notdec::backend::structuring

#endif
