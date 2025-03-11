#include "notdec-llvm2c/Interface/HType.h"
#include <cassert>
#include <llvm/Support/Casting.h>

namespace notdec::ast {

// HTypeContext HTypeContext::Instance;

TypedefDecl *TypedefDecl::Create(HTypeContext &Ctx, const std::string &Name,
                                 HType *Type) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new TypedefDecl(Name, Type);
  Ctx.addDecl(Name, std::unique_ptr<TypedDecl>{Decl});
  return Decl;
}

RecordDecl *RecordDecl::Create(HTypeContext &Ctx, const std::string &Name) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new RecordDecl(Name);
  Ctx.addDecl(Name, std::unique_ptr<TypedDecl>{Decl});
  return Decl;
}

UnionDecl *UnionDecl::Create(HTypeContext &Ctx, const std::string &Name) {
  assert(!Ctx.getDecl(Name));
  auto Decl = new UnionDecl(Name);
  Ctx.addDecl(Name, std::unique_ptr<TypedDecl>{Decl});
  return Decl;
}

HType *HType::getPointeeType() const {
  assert(getKind() == TK_Pointer);
  return llvm::cast<PointerType>(this)->getPointeeType();
}

TypedDecl *HType::getAsRecordOrUnionDecl() const {
  if(auto* R = getAsRecordDecl()) {
    return R;
  } else if (auto* U = getAsUnionDecl()) {
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
    assert(false && "TODO");
  default:
    assert(false && "Unknown HType");
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
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* " << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void UnionDecl::print(llvm::raw_fd_ostream &OS) const {
  OS << "union " << getName() << " {\n";
  for (auto &Field : Members) {
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* " << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void TypedefDecl::print(llvm::raw_fd_ostream &OS) const {
  OS << "typedef " << Type->getAsString() << " " << getName() << "; /* " << getComment() <<" */\n";
}

// void RecordDecl::addPaddings() {
// for (size_t i = 0; i < Fields.size(); i++) {
//   auto &Ent = Fields[i];
//   if (i + 1 < Fields.size()) {
//     auto &Next = Fields[i + 1];
//     auto NextOffset = Next.R.Start;
//     auto CurEnd = Ent.R.Start + Ent.R.Size;
//     bool isArray = Ent.Decl == nullptr ? false :
//     Ent.Decl->getType()->isArrayType(); if (CurEnd < NextOffset) {
//       if (Ent.R.Start.access.size() > 0 || isArray) {
//         // expand the array size
//         Ent.R.Size = NextOffset - Ent.R.Start;
//       } else {
//         // add padding
//         Fields.insert(
//             Fields.begin() + i + 1,
//             FieldEntry{.R = Range{.Start = IndexTy{.offset = CurEnd},
//                                   .Size = NextOffset - CurEnd},
//                        .isPadding = true});
//       }
//     }
//   }
// }
// }

void RecordDecl::addPaddings() {
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
}

void RecordDecl::resolveInitialValue() {
  // for (size_t i = 0; i < Fields.size(); i++) {
  //   auto &Ent = Fields[i];
  //   if (Ent.isPadding) {
  //     continue;
  //   }
  //   if (Ent.Decl == nullptr) {
  //     continue;
  //   }
  //   auto QT = Ent.Decl->getType();
  //   if (QT->isArrayType() &&
  //       QT->getAsArrayTypeUnsafe()->getElementType()->isCharType()) {
  //     auto Offset = Ent.R.Start;
  //     auto CStr = Bytes->decodeCStr(Offset);
  //     if (CStr.size() > 0) {
  //       llvm::cast<clang::FieldDecl>(Ent.Decl)->setInClassInitializer(
  //           clang::StringLiteral::Create(Decl->getASTContext(), CStr,
  //                                        clang::StringLiteral::Ascii, false,
  //                                        QT, clang::SourceLocation()));
  //       // shrink the array size if the entry is too large.
  //       // expand if there is enough space.
  //       auto NewSize = CStr.size() + 1;
  //       if (Ent.R.Size > NewSize || (i + 1 == Fields.size()) ||
  //           (i + 1 < Fields.size() &&
  //            Fields[i + 1].R.Start - Ent.R.Start > NewSize)) {
  //         updateFieldSize(i, NewSize);
  //       }
  //     }
  //   }
  // }
}

} // namespace notdec::ast
