#include "notdec-backends/Structuring/StructurerRegistry.h"

#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"

namespace notdec::backend::structuring {
namespace {

using StructurerFactory = std::unique_ptr<Structurer> (*)();

std::unique_ptr<Structurer> createGoto() {
  return std::make_unique<GotoStructurer>();
}

std::unique_ptr<Structurer> createPhoenix() {
  return std::make_unique<PhoenixStructurer>();
}

std::unique_ptr<Structurer> createSAILR() {
  return std::make_unique<SAILRStructurer>();
}

struct StructurerRegistration {
  std::string_view Name;
  StructurerFactory Factory;
};

constexpr StructurerRegistration Structurers[] = {
    {"goto", createGoto},
    {"phoenix", createPhoenix},
    {"sailr", createSAILR},
};

constexpr std::string_view StructurerNames[] = {
    "goto",
    "phoenix",
    "sailr",
};

} // namespace

llvm::ArrayRef<std::string_view> registeredStructurerNames() {
  return StructurerNames;
}

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  for (const StructurerRegistration &Registration : Structurers) {
    if (Registration.Name == Name) {
      return Registration.Factory();
    }
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
