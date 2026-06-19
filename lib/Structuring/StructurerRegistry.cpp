#include "notdec-backends/Structuring/StructurerRegistry.h"

#include "notdec-backends/Structuring/GotoStructurer.h"

namespace notdec::backend::structuring {

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  if (Name == "goto") {
    return std::make_unique<GotoStructurer>();
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
