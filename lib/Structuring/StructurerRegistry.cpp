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

struct StructurerAlias {
  std::string_view ExternalName;
  std::string_view CanonicalName;
};

// Keep planned Angr-compatible algorithm names in one place even before every
// factory is implemented. The executable registry below only exposes working
// algorithms.
constexpr std::string_view KnownStructurers[] = {
    "goto",
    "phoenix",
    "sailr",
    "dream",
};

constexpr StructurerRegistration Structurers[] = {
    {"goto", createGoto},
    {"phoenix", createPhoenix},
    {"sailr", createSAILR},
};

constexpr StructurerAlias CLIStructurerAliases[] = {
    {"goto", "goto"},
    {"structured-goto", "goto"},
    {"structured-phoenix", "phoenix"},
    {"structured-sailr", "sailr"},
    {"structured-dream", "dream"},
};

constexpr StructurerAlias StructurerAliases[] = {
    {"goto", "goto"},
    {"structured-goto", "goto"},
    {"phoenix", "phoenix"},
    {"structured-phoenix", "phoenix"},
    {"sailr", "sailr"},
    {"structured-sailr", "sailr"},
    {"dream", "dream"},
    {"structured-dream", "dream"},
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

llvm::ArrayRef<std::string_view> knownStructurerNames() {
  return KnownStructurers;
}

std::string_view resolveAlias(std::string_view Name,
                              llvm::ArrayRef<StructurerAlias> Aliases) {
  for (const StructurerAlias &Alias : Aliases) {
    if (equalsIgnoreCase(Alias.ExternalName, Name)) {
      return Alias.CanonicalName;
    }
  }
  return {};
}

llvm::ArrayRef<std::string_view> registeredStructurerNames() {
  return StructurerNames;
}

std::string_view resolveStructurerName(std::string_view Name) {
  return resolveAlias(Name, CLIStructurerAliases);
}

std::unique_ptr<Structurer> createStructurer(std::string_view Name) {
  std::string_view CanonicalName = resolveAlias(Name, StructurerAliases);
  if (CanonicalName.empty()) {
    return nullptr;
  }
  for (const StructurerRegistration &Registration : Structurers) {
    if (equalsIgnoreCase(Registration.Name, CanonicalName)) {
      return Registration.Factory();
    }
  }
  return nullptr;
}

} // namespace notdec::backend::structuring
