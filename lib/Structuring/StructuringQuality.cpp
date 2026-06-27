#include "notdec-backends/Structuring/StructuringQuality.h"

#include <algorithm>

namespace notdec::backend::structuring {
namespace {

void collectQuality(const StructuredTree &Tree, NodeId Id,
                    ControlFlowStructureCounter &Counter) {
  const StructuredNode *Node = Tree.getNode(Id);
  if (Node == nullptr) {
    return;
  }

  switch (Node->Kind) {
  case StructuredNodeKind::While:
    ++Counter.WhileLoops;
    break;
  case StructuredNodeKind::DoWhile:
    ++Counter.DoWhileLoops;
    break;
  case StructuredNodeKind::InfiniteLoop:
    ++Counter.InfiniteLoops;
    break;
  case StructuredNodeKind::Goto:
    if (Node->Target != InvalidBlockId) {
      ++Counter.GotoTargets[Node->Target];
    }
    break;
  case StructuredNodeKind::Label:
    if (Node->Block != InvalidBlockId &&
        std::find(Counter.OrderedLabels.begin(), Counter.OrderedLabels.end(),
                  Node->Block) == Counter.OrderedLabels.end()) {
      Counter.OrderedLabels.push_back(Node->Block);
    }
    break;
  case StructuredNodeKind::Sequence:
  case StructuredNodeKind::BasicBlock:
  case StructuredNodeKind::If:
  case StructuredNodeKind::Switch:
  case StructuredNodeKind::Break:
  case StructuredNodeKind::Continue:
  case StructuredNodeKind::Return:
  case StructuredNodeKind::Unreachable:
    break;
  }

  for (NodeId Child : Node->Children) {
    collectQuality(Tree, Child, Counter);
  }
  for (const StructuredSwitchCase &Case : Node->StructuredCases) {
    collectQuality(Tree, Case.Body, Counter);
  }
  collectQuality(Tree, Node->Then, Counter);
  collectQuality(Tree, Node->Else, Counter);
  collectQuality(Tree, Node->Body, Counter);
  collectQuality(Tree, Node->Default, Counter);
}

unsigned totalLoops(const ControlFlowStructureCounter &Counter) {
  // Angr's relative-quality check only compares LoopNode sorts: for, while,
  // and do-while. Keep NotDec's infinite-loop count out of this trade check so
  // SAILR pass acceptance stays aligned with the upstream heuristic.
  return Counter.WhileLoops + Counter.DoWhileLoops + Counter.ForLoops;
}

unsigned totalGotos(const ControlFlowStructureCounter &Counter) {
  unsigned Total = 0;
  for (const auto &[_, Count] : Counter.GotoTargets) {
    Total += Count;
  }
  return Total;
}

} // namespace

ControlFlowStructureCounter
ControlFlowStructureCounter::collect(const StructuredTree &Tree) {
  ControlFlowStructureCounter Counter;
  collectQuality(Tree, Tree.root(), Counter);
  Counter.normalizeGotoLabels();
  return Counter;
}

void ControlFlowStructureCounter::normalizeGotoLabels() {
  // Match Angr's post-walk cleanup: a goto to a block with no output label does
  // not participate in relative-quality checks, and unused labels do not affect
  // left/right label ordering.
  for (auto It = GotoTargets.begin(); It != GotoTargets.end();) {
    if (std::find(OrderedLabels.begin(), OrderedLabels.end(), It->first) ==
        OrderedLabels.end()) {
      It = GotoTargets.erase(It);
      continue;
    }
    ++It;
  }

  OrderedLabels.erase(
      std::remove_if(OrderedLabels.begin(), OrderedLabels.end(),
                     [&](BlockId Label) {
                       return GotoTargets.count(Label) == 0;
                     }),
      OrderedLabels.end());
}

bool improvesRelativeStructuringQuality(
    const ControlFlowStructureCounter &Initial,
    const ControlFlowStructureCounter &Current) {
  unsigned InitialLoops = totalLoops(Initial);
  unsigned CurrentLoops = totalLoops(Current);

  if (Current.ForLoops < Initial.ForLoops && CurrentLoops == InitialLoops) {
    return false;
  }

  if (totalGotos(Initial) == totalGotos(Current) && totalGotos(Initial) != 0) {
    if (Current.GotoTargets.size() > Initial.GotoTargets.size()) {
      return false;
    }

    for (const auto &[Target, CurrentCount] : Current.GotoTargets) {
      unsigned InitialCount = 0;
      auto InitialIt = Initial.GotoTargets.find(Target);
      if (InitialIt != Initial.GotoTargets.end()) {
        InitialCount = InitialIt->second;
      }

      auto LabelIt = std::find(Current.OrderedLabels.begin(),
                               Current.OrderedLabels.end(), Target);
      if (LabelIt == Current.OrderedLabels.end()) {
        continue;
      }

      if (CurrentCount > InitialCount) {
        for (auto It = std::next(LabelIt); It != Current.OrderedLabels.end();
             ++It) {
          unsigned RightCurrent = Current.GotoTargets.count(*It) == 0
                                      ? 0
                                      : Current.GotoTargets.at(*It);
          unsigned RightInitial = Initial.GotoTargets.count(*It) == 0
                                      ? 0
                                      : Initial.GotoTargets.at(*It);
          if (RightCurrent < RightInitial) {
            return false;
          }
        }
      } else if (CurrentCount < InitialCount) {
        for (auto It = Current.OrderedLabels.begin(); It != LabelIt; ++It) {
          unsigned LeftCurrent = Current.GotoTargets.count(*It) == 0
                                     ? 0
                                     : Current.GotoTargets.at(*It);
          unsigned LeftInitial = Initial.GotoTargets.count(*It) == 0
                                     ? 0
                                     : Initial.GotoTargets.at(*It);
          if (LeftCurrent > LeftInitial) {
            return false;
          }
        }
      }
    }
  }

  return true;
}

} // namespace notdec::backend::structuring
