#include "notdec-llvm2c/Interface/HType.h"
#include "Interface/StructManager.h"
#include <cassert>
#include <llvm/Support/Casting.h>

namespace notdec::ast {

// HTypeContext HTypeContext::Instance;

SimpleRange getRange(const TypedDecl *Decl) {
  if (auto *RD = llvm::dyn_cast<RecordDecl>(Decl)) {
    return RD->getRange();
  }
  if (auto *UD = llvm::dyn_cast<UnionDecl>(Decl)) {
    return UD->getRange();
  }
  if (auto *TD = llvm::dyn_cast<TypedefDecl>(Decl)) {
    assert(false && "Cannot get range for typedef type");
  }
  assert(false && "Unknown Decl type");
}

HType *ArrayType::withSize(HTypeContext &Ctx,
                           std::optional<unsigned> NumElements) {
  return Ctx.getArrayType(isConst(), ElementType, NumElements);
}

bool HType::isCharArrayType() const {
  if (isArrayType()) {
    if (llvm::cast<ArrayType>(this)->getElementType()->isCharType()) {
      return true;
    }
  }
  return false;
}

bool HType::isCharType() const {
  return getKind() == TK_Integer &&
         llvm::cast<IntegerType>(this)->getBitSize() == 8;
}

TypedefDecl *TypedefDecl::Create(HTypeContext &Ctx, const std::string &Name,
                                 HType *Type) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new TypedefDecl(Name, Type);
  Ctx.addDecl(Name, std::shared_ptr<TypedefDecl>{Decl});
  return Decl;
}

RecordDecl *RecordDecl::Create(HTypeContext &Ctx, const std::string &Name) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new RecordDecl(Name);
  Ctx.addDecl(Name, std::shared_ptr<RecordDecl>{Decl});
  return Decl;
}

UnionDecl *UnionDecl::Create(HTypeContext &Ctx, const std::string &Name) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new UnionDecl(Name);
  Ctx.addDecl(Name, std::shared_ptr<UnionDecl>{Decl});
  return Decl;
}

HType *HType::getPointeeType() const {
  assert(getKind() == TK_Pointer);
  return llvm::cast<PointerType>(this)->getPointeeType();
}

HType *HType::getArrayElementType() const {
  assert(getKind() == TK_Array);
  return llvm::cast<ArrayType>(this)->getElementType();
}

TypedDecl *HType::getAsRecordOrUnionDecl() const {
  if (auto *R = getAsRecordDecl()) {
    return R;
  } else if (auto *U = getAsUnionDecl()) {
    return U;
  }
  return nullptr;
}

RecordDecl *HType::getAsRecordDecl() const {
  return llvm::dyn_cast<RecordType>(this)->getDecl();
}

UnionDecl *HType::getAsUnionDecl() const {
  return llvm::dyn_cast<UnionType>(this)->getDecl();
}

TypedefDecl *HType::getAsTypedefDecl() const {
  return llvm::dyn_cast<TypedefType>(this)->getDecl();
}

std::string HType::getAsString() const {
  switch (getKind()) {
  case TK_Integer:
    return "i" + std::to_string(llvm::cast<IntegerType>(this)->getBitSize());
  case TK_Float:
    return "f" + std::to_string(llvm::cast<FloatingType>(this)->getBitSize());
  case TK_Pointer:
    return (getPointeeType() == nullptr ? "void"
                                        : getPointeeType()->getAsString()) +
           "*";
  case TK_Record:
    return "struct " + llvm::cast<RecordType>(this)->getDecl()->getName();
  case TK_Union:
    return "union " + llvm::cast<UnionType>(this)->getDecl()->getName();
  case TK_Array: {
    auto *AT = llvm::cast<ArrayType>(this);
    bool hasSize = AT->getNumElements().has_value();
    return AT->getElementType()->getAsString() + "[" +
           (hasSize ? "" : std::to_string(*AT->getNumElements())) + "]";
  }
  case TK_Typedef:
    return llvm::cast<TypedefType>(this)->getDecl()->getName();
        default : assert(false && "Unknown HType");
  }
}

void TypedDecl::print(llvm::raw_fd_ostream &OS) const {
  switch (getKind()) {
  case DK_Record:
    llvm::cast<RecordDecl>(this)->print(OS);
    break;
  case DK_Union:
    llvm::cast<UnionDecl>(this)->print(OS);
    break;
  case DK_Typedef:
    llvm::cast<TypedefDecl>(this)->print(OS);
    break;
  }
}

void RecordDecl::print(llvm::raw_fd_ostream &OS) const {
  OS << "struct " << getName() << " {\n";
  for (auto &Field : Fields) {
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* "
       << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void UnionDecl::print(llvm::raw_fd_ostream &OS) const {
  OS << "union " << getName() << " {\n";
  for (auto &Field : Members) {
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* "
       << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void TypedefDecl::print(llvm::raw_fd_ostream &OS) const {
  OS << "typedef " << Type->getAsString() << " " << getName() << "; /* "
     << getComment() << " */\n";
}

const FieldDecl *RecordDecl::getFieldAt(OffsetTy Offset) const {
  const ast::FieldDecl *Target = nullptr;
  for (auto &Field : getFields()) {
    auto End = Field.R.Size + Field.R.Start;
    if (Field.R.Start == Offset) {
      Target = &Field;
      break;
    }
    if (Field.R.Start <= Offset && Offset < End) {
      Target = &Field;
      break;
    }
  }
  return Target;
}

// void RecordDecl::addPaddings() {
// TODO

// for (size_t i = 0; i < Fields.size(); i++) {

//   // add padding, expand array accordingly
//   FieldEntry *Next = nullptr;
//   if (i + 1 < Info.Fields.size()) {
//     Next = &Info.Fields[i + 1];
//   }

//   // expand the array size to fill up the padding
//   auto ArraySize = Ent.R.Size;
//   // expand the array size to fill up the padding
//   // set to Ent.R.Size later if this is really an array.
//   if (Next != nullptr && Ent.R.Start + Ent.R.Size < Next->R.Start) {
//     ArraySize = Next->R.Start - Ent.R.Start;
//   }

//   auto Count = ArraySize / Ent.R.Start.access.at(0).Size;
//   // create array type
//   Ty = Ctx.getConstantArrayType(
//       Ty, llvm::APInt(32, Count), nullptr,
//       Count == 0 ? clang::ArrayType::Star : clang::ArrayType::Normal, 0);

//   // set to Ent.R.Size if this is really an array.
//   if (Ty->isArrayType()) {
//     Ent.R.Size = ArraySize;
//   }

//   // if prev is array
//   if (Prev != nullptr && Prev->Decl != nullptr &&
//       Prev->Decl->getType()->isArrayType()) {
//     auto ElemTy = Prev->Decl->getType()->getArrayElementTypeNoTypeQual();
//     // if current ty is char
//     if (ElemTy == Ty.getTypePtr() && Ty->isCharType()) {
//       if (Prev->R.Start + Prev->R.Size == Ent.R.Start) {
//         // merge to prev
//         Prev->R.Size += Ent.R.Size;
//         // Update array Size
//         Prev->Decl->setType(Ctx.getConstantArrayType(
//             Ty, llvm::APInt(32, Prev->R.Size),
//             clang::IntegerLiteral::Create(Ctx, llvm::APInt(32,
//             Prev->R.Size),
//                                           Ctx.IntTy,
//                                           clang::SourceLocation()),
//             clang::ArrayType::Normal, 0));
//         Info.Fields.erase(Info.Fields.begin() + i);
//         i--;
//         continue;
//       }
//       // merge to prev
//     }
//   }

//   // add padding?
//   auto End = Ent.R.Start + Ent.R.Size;
//   if (Next != nullptr && End < Next->R.Start) {
//     auto PaddingSize = Next->R.Start - End;
//     if (PaddingSize != 0) {
//       Ty = Ctx.getConstantArrayType(
//           Ctx.CharTy, llvm::APInt(32, PaddingSize),
//           clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, PaddingSize),
//                                         Ctx.IntTy,
//                                         clang::SourceLocation()),
//           clang::ArrayType::Normal, 0);

//       auto *FII = &Ctx.Idents.get(ValueNamer::getName("padding_"));
//       clang::FieldDecl *Field = clang::FieldDecl::Create(
//           Ctx, Decl, clang::SourceLocation(), clang::SourceLocation(), FII,
//           Ty, nullptr, nullptr, false, clang::ICIS_CopyInit);

//       Parent.DeclComments[Field] = "at offset: " + std::to_string(End);
//       Decl->addDecl(Field);
//     }
//   }
// }
// }

} // namespace notdec::ast
