#ifndef NOTDEC_BACKENDS_STRUCTURING_PHOENIXSTRUCTURER_H
#define NOTDEC_BACKENDS_STRUCTURING_PHOENIXSTRUCTURER_H

#include "notdec-backends/Structuring/MutableRegionGraph.h"
#include "notdec-backends/Structuring/RegionStructurer.h"
#include "notdec-backends/Structuring/Structurer.h"

#include <map>
#include <vector>

namespace notdec::backend::structuring {

// Phoenix-style reducer on the shared structuring model. This class is the
// backend-independent migration target for the old C Phoenix implementation:
// it mutates MutableRegionGraph, emits StructuredTree nodes, and never touches
// Clang or Solidity-specific ASTs.
class PhoenixStructurer : public Structurer, public RegionStructurer {
public:
  StructuredTree structure(const StructuredCFG &Cfg) override;
  bool supportsChildRegions() const override { return true; }
  NodeId structureRegion(const StructuredCFG &Cfg, const Region &R,
                         StructuredTree &Tree) override;
  NodeId structureRegion(const StructuredCFG &Cfg, const RegionTree &Regions,
                         const Region &R,
                         const std::map<RegionId, NodeId> &StructuredChildren,
                         StructuredTree &Tree) override;

protected:
  // Angr's Phoenix keeps these stages separate: acyclic schemas, cyclic
  // schemas, cyclic refinement, and last-resort edge virtualization. Keep the
  // same boundary here so SAILR and future algorithms can override one stage
  // without replacing the whole reducer loop.
  virtual bool analyzeAcyclic(const StructuredCFG &Cfg,
                              MutableRegionGraph &Graph,
                              StructuredTree &Tree) const;
  virtual bool analyzeCyclic(const StructuredCFG &Cfg,
                             MutableRegionGraph &Graph,
                             StructuredTree &Tree) const;
  virtual bool refineCyclic(const StructuredCFG &Cfg, const RegionTree &Regions,
                            const Region &R, MutableRegionGraph &Graph,
                            StructuredTree &Tree) const;
  virtual bool lastResortRefinement(const StructuredCFG &Cfg, const Region &R,
                                    MutableRegionGraph &Graph,
                                    StructuredTree &Tree) const;
  virtual bool useImprovedCyclicSchemas() const { return false; }
  virtual bool preprocessRegionGraph(const StructuredCFG &Cfg, const Region &R,
                                     MutableRegionGraph &Graph) const {
    (void)Cfg;
    (void)R;
    (void)Graph;
    return false;
  }
  virtual std::vector<VirtualEdge>
  edgeVirtualizationHints(const StructuredCFG &Cfg,
                          const MutableRegionGraph &Graph,
                          const MutableRegionGraphAnalysis &Analysis) const;
  virtual std::vector<VirtualEdge>
  orderVirtualizableEdges(const StructuredCFG &Cfg,
                          const MutableRegionGraph &Graph,
                          const MutableRegionGraphAnalysis &Analysis,
                          std::vector<VirtualEdge> Edges) const;
  bool virtualizeOneEdge(const StructuredCFG &Cfg, const Region &R,
                         MutableRegionGraph &Graph, StructuredTree &Tree) const;
};

} // namespace notdec::backend::structuring

#endif
