#include "notdec-backends/Solidity/TypePrinter.h"

#include <llvm/Support/Casting.h>

namespace notdec::backend::solidity {

using notdec::ast::HType;

static std::string formatIntegerType(const notdec::ast::IntegerType &Ty) {
  unsigned Bits = Ty.getBitSize();
  if (Bits == 1) {
    return "bool";
  }
  if (Bits == 0 || Bits > 256 || Bits % 8 != 0) {
    Bits = 256;
  }
  return std::string(Ty.isSigned() ? "int" : "uint") + std::to_string(Bits);
}

std::string TypePrinter::formatType(const HType *Ty) {
  if (Ty == nullptr) {
    return "uint256";
  }
  Ty = Ty->getCanonicalType();
  if (const auto *Int = llvm::dyn_cast<notdec::ast::IntegerType>(Ty)) {
    return formatIntegerType(*Int);
  }
  if (const auto *DualPtr = llvm::dyn_cast<notdec::ast::DualPointerType>(Ty)) {
    return formatType(DualPtr->getLoadType());
  }
  if (const auto *Ptr = llvm::dyn_cast<notdec::ast::PointerType>(Ty)) {
    return formatType(Ptr->getPointeeType());
  }
  if (const auto *Array = llvm::dyn_cast<notdec::ast::ArrayType>(Ty)) {
    std::string Element = formatType(Array->getElementType());
    if (auto Count = Array->getNumElements()) {
      return Element + "[" + std::to_string(*Count) + "]";
    }
    return Element + "[]";
  }
  if (const auto *Inter = llvm::dyn_cast<notdec::ast::SetInterType>(Ty)) {
    for (const HType *Member : Inter->getTypes()) {
      std::string Printed = formatType(Member);
      if (Printed != "uint256") {
        return Printed;
      }
    }
  }
  if (const auto *Union = llvm::dyn_cast<notdec::ast::SetUnionType>(Ty)) {
    for (const HType *Member : Union->getTypes()) {
      std::string Printed = formatType(Member);
      if (Printed != "uint256") {
        return Printed;
      }
    }
  }
  return "uint256";
}

} // namespace notdec::backend::solidity
