#include "notdec-backends/Structuring/StructurerRegistry.h"

#include "notdec-backends/Structuring/GotoStructurer.h"
#include "notdec-backends/Structuring/PhoenixStructurer.h"
#include "notdec-backends/Structuring/SAILRStructurer.h"

#include <llvm/ADT/StringExtras.h>

#include <algorithm>
#include <array>

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

constexpr auto makeStructurerNames() {
  std::array<std::string_view, std::size(Structurers)> Names{};
  for (std::size_t I = 0; I < std::size(Structurers); ++I) {
    Names[I] = Structurers[I].Name;
  }
  return Names;
}

constexpr auto StructurerNames = makeStructurerNames();

bool equalsIgnoreCase(std::string_view Lhs, std::string_view Rhs) {
  return Lhs.size() == Rhs.size() &&
         std::equal(Lhs.begin(), Lhs.end(), Rhs.begin(), [](char A, char B) {
           return llvm::toLower(A) == llvm::toLower(B);
         });
}

} // namespace

llvm::ArrayRef<std::string_view> registeredStructurerNames() {
  return StructurerNames;
}

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  for (const StructurerRegistration &Registration : Structurers) {
    if (equalsIgnoreCase(Registration.Name, Name)) {
      return Registration.Factory();
    }
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
