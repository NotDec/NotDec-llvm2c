#ifndef NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_SAILRSTRUCTURER_H

#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/SAILRDeoptimization.h"

namespace notdec::backend::structuring {

// SAILR shares Phoenix reducers and changes the last-resort edge choice. This
// keeps the algorithm boundary the same as Angr: SAILR is a Phoenix variant
// whose first reusable part is virtual edge ordering.
class SAILRStructurer : public PhoenixStructurer {
public:
  SAILRStructurer(bool ImprovePhoenix = true, unsigned PostDomMaxEdges = 10,
                  unsigned PostDomMaxGraphSize = 50,
                  SAILRDeoptimizationPipelineOptions DeoptOptions =
                      defaultSAILRDeoptimizationPipelineOptions())
      : ImprovePhoenix(ImprovePhoenix), PostDomMaxEdges(PostDomMaxEdges),
        PostDomMaxGraphSize(PostDomMaxGraphSize),
        DeoptOptions(DeoptOptions) {}

  StructuredTree structure(const StructuredCFG &Cfg) override;

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
  SAILRDeoptimizationPipelineOptions DeoptOptions;
};

} // namespace notdec::backend::structuring

#endif
