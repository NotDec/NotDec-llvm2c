#ifndef NOTDEC_BACKENDS_STRUCTURING_STRUCTURERREGISTRY_H
#define NOTDEC_BACKENDS_STRUCTURING_STRUCTURERREGISTRY_H

#include "notdec-backends/Structuring/Structurer.h"

#include <llvm/ADT/ArrayRef.h>

#include <memory>
#include <string_view>

namespace notdec::backend::structuring {

constexpr std::string_view DefaultStructurerName = "sailr";

llvm::ArrayRef<std::string_view> knownStructurerNames();
llvm::ArrayRef<std::string_view> registeredStructurerNames();
std::string_view resolveStructurerName(std::string_view Name);
std::unique_ptr<Structurer> createStructurer(std::string_view Name);

} // namespace notdec::backend::structuring

#endif
