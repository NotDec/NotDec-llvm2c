#ifndef NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H

#include "notdec-backends/Structuring/PhoenixStructurer.h"

namespace notdec::backend::structuring {

// SAILR shares Phoenix reducers and changes the last-resort edge choice. This
// keeps the algorithm boundary the same as Angr: SAILR is a Phoenix variant
// whose first reusable part is virtual edge ordering.
class SAILRStructurer : public PhoenixStructurer {
protected:
  bool useImprovedCyclicSchemas() const override { return true; }
  std::vector<VirtualEdge>
  orderVirtualizableEdges(const StructuredCFG &Cfg,
                          const MutableRegionGraph &Graph,
                          const MutableRegionGraphAnalysis &Analysis,
                          std::vector<VirtualEdge> Edges) const override;
};

} // namespace notdec::backend::structuring

#endif
