#include "notdec-llvm2c/Interface/HType.h"
#include "Interface/StructManager.h"
#include <cassert>
#include <llvm/Support/Casting.h>

namespace notdec::ast {

namespace {

template <typename T> void sortPairForDisplay(T &Lhs, T &Rhs) {
  if (Rhs < Lhs) {
    std::swap(Lhs, Rhs);
  }
}

} // namespace

// HTypeContext HTypeContext::Instance;

SimpleRange getRange(const TypedDecl *Decl) {
  if (auto *RD = llvm::dyn_cast<RecordDecl>(Decl)) {
    return RD->getRange();
  }
  if (auto *UD = llvm::dyn_cast<UnionDecl>(Decl)) {
    return UD->getRange();
  }
  if (llvm::isa<TypedefDecl>(Decl)) {
    assert(false && "Cannot get range for typedef type");
  }
  assert(false && "Unknown Decl type");
}

HType *ArrayType::withSize(HTypeContext &Ctx,
                           std::optional<unsigned> NumElements) {
  return Ctx.getArrayType(isConst(), ElementType, NumElements);
}

bool HType::isVoidPtrType() const {
  if (isPointerType()) {
    if (getPointeeType() == nullptr || getPointeeType()->isVoidPtrType()) {
      return true;
    }
  }
  return false;
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
  return llvm::dyn_cast<RecordPtrType>(this)->getDecl();
}

UnionDecl *HType::getAsUnionDecl() const {
  return llvm::dyn_cast<UnionType>(this)->getDecl();
}

TypedefDecl *HType::getAsTypedefDecl() const {
  return llvm::dyn_cast<TypedefType>(this)->getDecl();
}

std::string HType::getAsString() const {
  switch (getKind()) {
  case TK_Top:
    return "top:" + std::to_string(llvm::cast<TopType>(this)->getBitSize());
  case TK_Bottom:
    return "bottom:" +
           std::to_string(llvm::cast<BottomType>(this)->getBitSize());
  case TK_Integer:
    return "i" + std::to_string(llvm::cast<IntegerType>(this)->getBitSize());
  case TK_Float:
    return "f" + std::to_string(llvm::cast<FloatingType>(this)->getBitSize());
  case TK_Pointer:
    return (getPointeeType() == nullptr ? "void"
                                        : getPointeeType()->getAsString()) +
           "*";
  case TK_Record:
    return "struct " + llvm::cast<RecordPtrType>(this)->getDecl()->getName() +
           "*";
  case TK_Union:
    return "union " + llvm::cast<UnionType>(this)->getDecl()->getName();
  case TK_Array: {
    auto *AT = llvm::cast<ArrayType>(this);
    bool hasSize = AT->getNumElements().has_value();
    return AT->getElementType()->getAsString() + "[" +
           (!hasSize ? "" : std::to_string(*AT->getNumElements())) + "]";
  }
  case TK_Function: {
    auto *FT = llvm::cast<FunctionType>(this);
    std::string Result;
    auto &Ret = FT->getReturnType();
    if (Ret.empty()) {
      Result = "void";
    } else if (Ret.size() == 1) {
      Result = Ret[0]->getAsString();
    } else {
      Result = "(";
      for (size_t i = 0; i < Ret.size(); i++) {
        if (i > 0)
          Result += ", ";
        Result += Ret[i]->getAsString();
      }
      Result += ")";
    }
    Result += " (*)(";
    auto &Params = FT->getParamTypes();
    for (size_t i = 0; i < Params.size(); i++) {
      if (i > 0)
        Result += ", ";
      Result += Params[i]->getAsString();
    }
    Result += ")";
    return Result;
  }
  case TK_DualPointer: {
    auto *DPT = llvm::cast<DualPointerType>(this);
    std::string Result = "ptr<load=";
    Result += (DPT->getLoadType() == nullptr ? "void"
                                             : DPT->getLoadType()->getAsString());
    Result += ", store=";
    Result +=
        (DPT->getStoreType() == nullptr ? "void"
                                        : DPT->getStoreType()->getAsString());
    Result += ", psize=" + std::to_string(DPT->getPointerSize()) + ">";
    return Result;
  }
  case TK_SetUnion: {
    auto *UT = llvm::cast<SetUnionType>(this);
    return "(" + UT->getLhs()->getAsString() + " | " +
           UT->getRhs()->getAsString() + ")";
  }
  case TK_SetInter: {
    auto *IT = llvm::cast<SetInterType>(this);
    return "(" + IT->getLhs()->getAsString() + " & " +
           IT->getRhs()->getAsString() + ")";
  }
  case TK_Typedef:
    return llvm::cast<TypedefType>(this)->getDecl()->getName();
  case TK_TypeVariable: {
    auto *TV = llvm::cast<TypeVariableType>(this);
    auto SizeBits = TV->getSizeBits();
    if (!SizeBits.has_value()) {
      return TV->getName();
    }
    return TV->getName() + ":" + std::to_string(*SizeBits);
  }
  default:
    assert(false && "Unknown HType");
  }
}

void TypedDecl::print(llvm::raw_ostream &OS) const {
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

void RecordDecl::print(llvm::raw_ostream &OS) const {
  OS << "struct " << getName() << " {\n";
  for (auto &Field : Fields) {
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* "
       << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void UnionDecl::print(llvm::raw_ostream &OS) const {
  OS << "union " << getName() << " {\n";
  for (auto &Field : Members) {
    OS << "  " << Field.Type->getAsString() << " " << Field.Name << "; /* "
       << Field.Comment << " */\n";
  }
  OS << "}; /* " << getComment() << " */\n";
}

void TypedefDecl::print(llvm::raw_ostream &OS) const {
  OS << "typedef " << Type->getAsString() << " " << getName() << "; /* "
     << getComment() << " */\n";
}

std::string HTypeSnapshotFormatter::getDeclName(const TypedDecl &Decl) {
  auto It = DeclNames.find(&Decl);
  if (It != DeclNames.end()) {
    return It->second;
  }

  std::string Name;
  switch (Decl.getKind()) {
  case TypedDecl::DK_Record:
    Name = "struct_" + std::to_string(NextStructId++);
    break;
  case TypedDecl::DK_Union:
    Name = "union_" + std::to_string(NextUnionId++);
    break;
  case TypedDecl::DK_Typedef:
    Name = "typedef_" + std::to_string(NextTypedefId++);
    break;
  }

  DeclNames.emplace(&Decl, Name);
  DeclOrder.push_back(&Decl);
  return Name;
}

std::string HTypeSnapshotFormatter::formatFieldName(const FieldDecl &Field,
                                                    unsigned Index,
                                                    unsigned PaddingIndex) const {
  if (Field.isPadding) {
    return "padding_" + std::to_string(PaddingIndex);
  }
  return "field_" + std::to_string(Index);
}

std::string HTypeSnapshotFormatter::formatDeclName(const TypedDecl &Decl) {
  return getDeclName(Decl);
}

void HTypeSnapshotFormatter::collectType(const HType *Type) {
  if (Type == nullptr) {
    return;
  }

  switch (Type->getKind()) {
  case HType::TK_Simple:
    assert(false && "Unexpected abstract simple type");
    return;
  case HType::TK_Top:
  case HType::TK_Bottom:
  case HType::TK_Integer:
  case HType::TK_Float:
  case HType::TK_TypeVariable:
    return;
  case HType::TK_Pointer:
    collectType(Type->getPointeeType());
    return;
  case HType::TK_Record:
    collectDecl(*llvm::cast<RecordPtrType>(Type)->getDecl());
    return;
  case HType::TK_Union:
    collectDecl(*llvm::cast<UnionType>(Type)->getDecl());
    return;
  case HType::TK_Array:
    collectType(Type->getArrayElementType());
    return;
  case HType::TK_Function: {
    auto *FT = llvm::cast<FunctionType>(Type);
    for (auto *Ret : FT->getReturnType()) {
      collectType(Ret);
    }
    for (auto *Param : FT->getParamTypes()) {
      collectType(Param);
    }
    return;
  }
  case HType::TK_DualPointer: {
    auto *DPT = llvm::cast<DualPointerType>(Type);
    collectType(DPT->getLoadType());
    collectType(DPT->getStoreType());
    return;
  }
  case HType::TK_SetUnion: {
    auto *UT = llvm::cast<SetUnionType>(Type);
    collectType(UT->getLhs());
    collectType(UT->getRhs());
    return;
  }
  case HType::TK_SetInter: {
    auto *IT = llvm::cast<SetInterType>(Type);
    collectType(IT->getLhs());
    collectType(IT->getRhs());
    return;
  }
  case HType::TK_Typedef:
    collectDecl(*llvm::cast<TypedefType>(Type)->getDecl());
    return;
  }
}

void HTypeSnapshotFormatter::collectDecl(const TypedDecl &Decl) {
  getDeclName(Decl);
  if (!CollectedDecls.insert(&Decl).second) {
    return;
  }

  if (auto *RD = llvm::dyn_cast<RecordDecl>(&Decl)) {
    for (const auto &Field : RD->getFields()) {
      collectType(Field.Type);
    }
    return;
  }
  if (auto *UD = llvm::dyn_cast<UnionDecl>(&Decl)) {
    for (const auto &Field : UD->getMembers()) {
      collectType(Field.Type);
    }
    return;
  }
  if (auto *TD = llvm::dyn_cast<TypedefDecl>(&Decl)) {
    collectType(TD->getType());
    return;
  }
}

std::string HTypeSnapshotFormatter::formatType(const HType *Type) {
  if (Type == nullptr) {
    return "<null>";
  }

  collectType(Type);

  switch (Type->getKind()) {
  case HType::TK_Simple:
    assert(false && "Unexpected abstract simple type");
  case HType::TK_Top:
    return "top:" + std::to_string(llvm::cast<TopType>(Type)->getBitSize());
  case HType::TK_Bottom:
    return "bottom:" +
           std::to_string(llvm::cast<BottomType>(Type)->getBitSize());
  case HType::TK_Integer:
    return "i" + std::to_string(llvm::cast<IntegerType>(Type)->getBitSize());
  case HType::TK_Float:
    return "f" + std::to_string(llvm::cast<FloatingType>(Type)->getBitSize());
  case HType::TK_Pointer:
    return (Type->getPointeeType() == nullptr ? "void"
                                              : formatType(Type->getPointeeType())) +
           "*";
  case HType::TK_Record:
    return getDeclName(*llvm::cast<RecordPtrType>(Type)->getDecl()) + "*";
  case HType::TK_Union:
    return getDeclName(*llvm::cast<UnionType>(Type)->getDecl());
  case HType::TK_Array: {
    auto *AT = llvm::cast<ArrayType>(Type);
    bool HasSize = AT->getNumElements().has_value();
    return formatType(AT->getElementType()) + "[" +
           (HasSize ? std::to_string(*AT->getNumElements()) : "") + "]";
  }
  case HType::TK_Function: {
    auto *FT = llvm::cast<FunctionType>(Type);
    std::string Result;
    auto &Ret = FT->getReturnType();
    if (Ret.empty()) {
      Result = "void";
    } else if (Ret.size() == 1) {
      Result = formatType(Ret[0]);
    } else {
      Result = "(";
      for (size_t I = 0; I < Ret.size(); I++) {
        if (I > 0) {
          Result += ", ";
        }
        Result += formatType(Ret[I]);
      }
      Result += ")";
    }
    Result += " (*)(";
    auto &Params = FT->getParamTypes();
    for (size_t I = 0; I < Params.size(); I++) {
      if (I > 0) {
        Result += ", ";
      }
      Result += formatType(Params[I]);
    }
    Result += ")";
    return Result;
  }
  case HType::TK_DualPointer: {
    auto *DPT = llvm::cast<DualPointerType>(Type);
    std::string Result = "ptr<load=";
    Result += DPT->getLoadType() == nullptr ? "void"
                                            : formatType(DPT->getLoadType());
    Result += ", store=";
    Result += DPT->getStoreType() == nullptr ? "void"
                                             : formatType(DPT->getStoreType());
    Result += ", psize=" + std::to_string(DPT->getPointerSize()) + ">";
    return Result;
  }
  case HType::TK_SetUnion: {
    auto *UT = llvm::cast<SetUnionType>(Type);
    auto Lhs = formatType(UT->getLhs());
    auto Rhs = formatType(UT->getRhs());
    sortPairForDisplay(Lhs, Rhs);
    return "(" + Lhs + " | " + Rhs + ")";
  }
  case HType::TK_SetInter: {
    auto *IT = llvm::cast<SetInterType>(Type);
    auto Lhs = formatType(IT->getLhs());
    auto Rhs = formatType(IT->getRhs());
    sortPairForDisplay(Lhs, Rhs);
    return "(" + Lhs + " & " + Rhs + ")";
  }
  case HType::TK_Typedef:
    return getDeclName(*llvm::cast<TypedefType>(Type)->getDecl());
  case HType::TK_TypeVariable: {
    auto *TV = llvm::cast<TypeVariableType>(Type);
    auto SizeBits = TV->getSizeBits();
    if (!SizeBits.has_value()) {
      return TV->getName();
    }
    return TV->getName() + ":" + std::to_string(*SizeBits);
  }
  }
  assert(false && "Unknown HType");
}

std::string HTypeSnapshotFormatter::formatDecl(const TypedDecl &Decl) {
  collectDecl(Decl);

  std::string Result;
  llvm::raw_string_ostream OS(Result);

  if (auto *RD = llvm::dyn_cast<RecordDecl>(&Decl)) {
    OS << "struct " << getDeclName(*RD) << " {\n";
    unsigned FieldIndex = 0;
    unsigned PaddingIndex = 0;
    for (const auto &Field : RD->getFields()) {
      auto Name = formatFieldName(Field, FieldIndex, PaddingIndex);
      OS << "  " << formatType(Field.Type) << " " << Name << "; /* "
         << Field.Comment << " */\n";
      if (Field.isPadding) {
        ++PaddingIndex;
      } else {
        ++FieldIndex;
      }
    }
    OS << "}; /* " << RD->getComment() << " */\n";
    OS.flush();
    return Result;
  }

  if (auto *UD = llvm::dyn_cast<UnionDecl>(&Decl)) {
    struct CanonicalUnionMember {
      std::string TypeText;
      std::string Comment;
      bool IsPadding = false;
      const FieldDecl *Field = nullptr;
    };

    std::vector<CanonicalUnionMember> Members;
    Members.reserve(UD->getMembers().size());
    for (const auto &Field : UD->getMembers()) {
      Members.push_back(CanonicalUnionMember{
          .TypeText = formatType(Field.Type),
          .Comment = Field.Comment,
          .IsPadding = Field.isPadding,
          .Field = &Field,
      });
    }
    std::sort(Members.begin(), Members.end(),
              [](const CanonicalUnionMember &LHS,
                 const CanonicalUnionMember &RHS) {
                return std::tie(LHS.IsPadding, LHS.TypeText, LHS.Comment) <
                       std::tie(RHS.IsPadding, RHS.TypeText, RHS.Comment);
              });

    OS << "union " << getDeclName(*UD) << " {\n";
    unsigned FieldIndex = 0;
    unsigned PaddingIndex = 0;
    for (const auto &Member : Members) {
      auto Name =
          formatFieldName(*Member.Field, FieldIndex, PaddingIndex);
      OS << "  " << Member.TypeText << " " << Name << "; /* " << Member.Comment
         << " */\n";
      if (Member.IsPadding) {
        ++PaddingIndex;
      } else {
        ++FieldIndex;
      }
    }
    OS << "}; /* " << UD->getComment() << " */\n";
    OS.flush();
    return Result;
  }

  auto *TD = llvm::cast<TypedefDecl>(&Decl);
  OS << "typedef " << formatType(TD->getType()) << " " << getDeclName(*TD)
     << "; /* " << TD->getComment() << " */\n";
  OS.flush();
  return Result;
}

std::vector<const TypedDecl *> HTypeSnapshotFormatter::getOrderedDecls() {
  if (Ctx != nullptr) {
    for (const auto &Ent : Ctx->getDecls()) {
      collectDecl(*Ent.second);
    }
  }
  return DeclOrder;
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
