#ifndef NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H

#include "notdec-backends/Structuring/PhoenixStructurer.h"

namespace notdec::backend::structuring {

// SAILR shares Phoenix reducers and changes the last-resort edge choice. This
// keeps the algorithm boundary the same as Angr: SAILR is a Phoenix variant
// whose first reusable part is virtual edge ordering.
class SAILRStructurer : public PhoenixStructurer {
public:
  SAILRStructurer(bool ImprovePhoenix = true, unsigned PostDomMaxEdges = 10,
                  unsigned PostDomMaxGraphSize = 50)
      : ImprovePhoenix(ImprovePhoenix), PostDomMaxEdges(PostDomMaxEdges),
        PostDomMaxGraphSize(PostDomMaxGraphSize) {}

protected:
  bool useImprovedCyclicSchemas() const override { return ImprovePhoenix; }
  std::vector<VirtualEdge>
  orderVirtualizableEdges(const StructuredCFG &Cfg,
                          const MutableRegionGraph &Graph,
                          const MutableRegionGraphAnalysis &Analysis,
                          std::vector<VirtualEdge> Edges) const override;

private:
  // Match Angr's default SAILR postdominator heuristic limits. Keep them on the
  // algorithm object so future backend options do not need to touch the helper.
  bool ImprovePhoenix;
  unsigned PostDomMaxEdges;
  unsigned PostDomMaxGraphSize;
};

} // namespace notdec::backend::structuring

#endif
