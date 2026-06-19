#include "notdec-backends/Structuring/StructurerRegistry.h"

#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"

namespace notdec::backend::structuring {

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  if (Name == "goto") {
    return std::make_unique<GotoStructurer>();
  }
  if (Name == "phoenix") {
    return std::make_unique<PhoenixStructurer>();
  }
  if (Name == "sailr") {
    return std::make_unique<SAILRStructurer>();
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
