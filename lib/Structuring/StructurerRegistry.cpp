#include "notdec-backends/Structuring/StructurerRegistry.h"

#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"

namespace notdec::backend::structuring {

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  if (Name == "goto") {
    return std::make_unique<GotoStructurer>();
  }
  if (Name == "phoenix") {
    return std::make_unique<PhoenixStructurer>();
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
