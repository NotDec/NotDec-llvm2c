#include "notdec-backends/Solidity/TypePrinter.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>

namespace notdec::backend::solidity {

using notdec::ast::HType;

namespace {

const notdec::ast::RecordDecl *getRecordDecl(const HType *Ty) {
  if (Ty == nullptr) {
    return nullptr;
  }
  Ty = Ty->getCanonicalType();
  if (const auto *Record = llvm::dyn_cast<notdec::ast::RecordType>(Ty)) {
    return Record->getDecl();
  }
  return nullptr;
}

const notdec::ast::FieldDecl *findField(const notdec::ast::RecordDecl *Decl,
                                        llvm::StringRef Name) {
  if (Decl == nullptr) {
    return nullptr;
  }
  for (const notdec::ast::FieldDecl &Field : Decl->getFields()) {
    if (Field.Name == Name) {
      return &Field;
    }
  }
  return nullptr;
}

const HType *mappingKeyType(const HType *Ty) {
  const auto *MapField = findField(getRecordDecl(Ty), "map");
  const auto *MapRecord = findField(
      MapField == nullptr ? nullptr : getRecordDecl(MapField->Type), "key");
  return MapRecord == nullptr ? nullptr : MapRecord->Type;
}

const HType *mappingValueType(const HType *Ty) {
  const auto *MapField = findField(getRecordDecl(Ty), "map");
  const auto *ValueField = findField(
      MapField == nullptr ? nullptr : getRecordDecl(MapField->Type), "value");
  return ValueField == nullptr ? nullptr : ValueField->Type;
}

const HType *arrayElementType(const HType *Ty) {
  const notdec::ast::RecordDecl *Decl = getRecordDecl(Ty);
  const auto *ArrayField = findField(Decl, "dynamic_array");
  if (ArrayField == nullptr) {
    ArrayField = findField(Decl, "static_array");
  }
  const auto *ElemField = findField(
      ArrayField == nullptr ? nullptr : getRecordDecl(ArrayField->Type), "elem");
  return ElemField == nullptr ? nullptr : ElemField->Type;
}

} // namespace

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

static bool hasMappingField(const notdec::ast::RecordDecl *Decl) {
  return findField(Decl, "map") != nullptr;
}

static bool hasArrayField(const notdec::ast::RecordDecl *Decl) {
  return findField(Decl, "dynamic_array") != nullptr ||
         findField(Decl, "static_array") != nullptr;
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

bool TypePrinter::isMappingType(const HType *Ty) {
  return hasMappingField(getRecordDecl(Ty)) &&
         mappingKeyType(Ty) != nullptr && mappingValueType(Ty) != nullptr;
}

bool TypePrinter::isArrayType(const HType *Ty) {
  if (const auto *Ptr = llvm::dyn_cast_or_null<notdec::ast::PointerType>(Ty)) {
    return isArrayType(Ptr->getPointeeType());
  }
  if (llvm::isa_and_nonnull<notdec::ast::ArrayType>(Ty)) {
    return true;
  }
  return hasArrayField(getRecordDecl(Ty)) && arrayElementType(Ty) != nullptr;
}

std::string TypePrinter::formatStateVariableType(const HType *Ty) {
  if (isMappingType(Ty)) {
    return "mapping(" + formatType(mappingKeyType(Ty)) + " => " +
           formatType(mappingValueType(Ty)) + ")";
  }
  if (const auto *Array = llvm::dyn_cast_or_null<notdec::ast::ArrayType>(Ty)) {
    return formatType(Array);
  }
  if (const HType *Element = arrayElementType(Ty)) {
    return formatType(Element) + "[]";
  }
  return formatType(Ty);
}

} // namespace notdec::backend::solidity
