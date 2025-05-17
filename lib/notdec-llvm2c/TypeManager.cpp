#include "notdec-llvm2c/TypeManager.h"
#include "Interface/HType.h"
#include "Interface/StructManager.h"
#include "StructuralAnalysis.h"
#include "Utils.h"
#include <cassert>
#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/OperationKinds.h>
#include <clang/AST/Type.h>
#include <clang/Basic/Specifiers.h>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <optional>
#include <variant>
#include <vector>

namespace notdec::llvm2c {

clang::QualType removeArrayType(clang::ASTContext &Ctx, clang::QualType Ty) {
  unsigned Quals = Ty.getQualifiers().getFastQualifiers();
  if (Ty->isArrayType()) {
    return clang::QualType(Ty->getArrayElementTypeNoTypeQual(), Quals);
  }
  if (Ty->isPointerType()) {
    return clang::QualType(
        Ctx.getPointerType(removeArrayType(Ctx, Ty->getPointeeType()))
            .getTypePtr(),
        Quals);
  }
  return Ty;
}

clang::QualType toLValueType(clang::ASTContext &Ctx, clang::QualType Ty) {
  if (Ty->isArrayType()) {
    return Ty;
  } else {
    return removeArrayType(Ctx, Ty);
  }
}

bool ClangTypeResult::hasType(ExtValuePtr Val, bool isUpperBound) {
  if (!isUpperBound) {
    return Result->ValueTypesLowerBound.count(Val) > 0;
  } else {
    return Result->ValueTypes.count(Val) > 0;
  }
}

clang::QualType ClangTypeResult::getType(ExtValuePtr Val, bool isUpperBound) {
  if (!isUpperBound) {
    return convertType(Result->ValueTypesLowerBound.at(Val));
  } else {
    return convertType(Result->ValueTypes.at(Val));
  }
}

clang::RecordDecl *createRecordDecl(clang::ASTContext &Ctx,
                                    TranslationUnitDecl *TUD,
                                    clang::TagDecl::TagKind Kind,
                                    llvm::StringRef Name) {
  auto *II = &Ctx.Idents.get(Name);
  // Create a Struct type for it.
  clang::RecordDecl *decl =
      clang::RecordDecl::Create(Ctx, Kind, TUD, clang::SourceLocation(),
                                clang::SourceLocation(), II, nullptr);
  return decl;
}

clang::QualType ClangTypeResult::convertType(HType *T) {
  // Check for cache
  if (auto It = TypeMap.find(T); It != TypeMap.end()) {
    return It->second;
  }

  // convert logic without cache
  auto doConvert = [&](HType *T) -> clang::QualType {
    if (auto *IT = llvm::dyn_cast<ast::IntegerType>(T)) {
      if (IT->getBitSize() == 1) {
        return Ctx.BoolTy;
      }
      if (IT->getBitSize() == 8) {
        return Ctx.CharTy;
      }
      return Ctx.getIntTypeForBitwidth(IT->getBitSize(), IT->isSigned());
    } else if (auto *FT = llvm::dyn_cast<ast::FloatingType>(T)) {
      if (FT->getBitSize() == 32) {
        return Ctx.FloatTy;
      } else if (FT->getBitSize() == 64) {
        return Ctx.DoubleTy;
      } else {
        assert(false && "Handle other float types");
      }
    } else if (auto *PT = llvm::dyn_cast<ast::PointerType>(T)) {
      if (PT->getPointeeType() == nullptr) {
        return Ctx.VoidPtrTy;
      }
      return Ctx.getPointerType(convertType(PT->getPointeeType()));
    } else if (auto *AT = llvm::dyn_cast<ast::ArrayType>(T)) {
      if (AT->getNumElements().has_value()) {
        return Ctx.getConstantArrayType(
            convertType(AT->getElementType()),
            llvm::APInt(32, *AT->getNumElements()), nullptr,
            clang::ArrayType::ArraySizeModifier::Normal, 0);
      } else {
        return Ctx.getConstantArrayType(convertType(AT->getElementType()),
                                        llvm::APInt(32, 0), nullptr,
                                        clang::ArrayType::Star, 0);
      }
    } else if (auto *RT = llvm::dyn_cast<ast::RecordType>(T)) {
      return Ctx.getRecordType(
          llvm::cast<clang::RecordDecl>(getDecl(RT->getDecl())));
    } else if (auto *UT = llvm::dyn_cast<ast::UnionType>(T)) {
      return Ctx.getRecordType(
          llvm::cast<clang::RecordDecl>(getDecl(UT->getDecl())));
    } else if (auto *TT = llvm::dyn_cast<ast::TypedefType>(T)) {
      return Ctx.getTypedefType(
          llvm::cast<clang::TypedefDecl>(getDecl(TT->getDecl())));
    } else {
      assert(false && "convertDecl: Unhandled type");
    }
  };

  auto Ret = doConvert(T);
  assert(!Ret.isNull() && "convertType: Ret is null?");
  TypeMap[T] = Ret;
  return Ret;
}

void ClangTypeResult::calcUseRelation() {
  assert(DeclUsage.empty() && ValueUsage.empty());
  // Only record direct usage.
  for (auto &Ent : Result->HTCtx->getDecls()) {
    if (auto *RD = llvm::dyn_cast<ast::RecordDecl>(Ent.second.get())) {
      for (auto &Field : RD->getFields()) {
        if (auto Decl = Field.Type->getAsRecordDecl()) {
          DeclUsage[RD].insert(Decl);
        }
        if (auto Decl = Field.Type->getAsUnionDecl()) {
          DeclUsage[RD].insert(Decl);
        }
        if (auto Decl = Field.Type->getAsTypedefDecl()) {
          DeclUsage[RD].insert(Decl);
        }
      }
    }
  }

  for (auto &Ent : Result->ValueTypes) {
    if (auto *RD = Ent.second->getAsRecordDecl()) {
      ValueUsage[RD].insert(Ent.first);
    }
    if (auto *UD = Ent.second->getAsUnionDecl()) {
      ValueUsage[UD].insert(Ent.first);
    }
    if (auto *TD = Ent.second->getAsTypedefDecl()) {
      ValueUsage[TD].insert(Ent.first);
    }
  }
}

clang::RecordDecl *ClangTypeResult::convertUnion(ast::UnionDecl *UD) {
  auto *TUD = AM->getTypeDefinitions();
  auto Ret = createRecordDecl(Ctx, TUD, clang::TTK_Union, UD->getName());
  Ret->startDefinition();
  for (auto &Member : UD->getMembers()) {
    auto QT = convertTypeL(Member.Type);
    auto *II = &Ctx.Idents.get(Member.Name);
    auto *FD = clang::FieldDecl::Create(
        Ctx, Ret, clang::SourceLocation(), clang::SourceLocation(), II, QT,
        nullptr, nullptr, false, clang::ICIS_NoInit);
    Ret->addDecl(FD);
    FieldDeclMap[FD] = &Member;
  }
  Ret->completeDefinition();
  TUD->addDecl(Ret);
  return Ret;
}

clang::RecordDecl *ClangTypeResult::convertStruct(ast::RecordDecl *RD,
                                                  bool isMemory) {
  auto *TUD = AM->getTypeDefinitions();
  if (isMemory) {
    TUD = AM->getGlobalDefinitions();
  }
  auto Ret = createRecordDecl(Ctx, TUD, clang::TTK_Struct, RD->getName());
  Ret->startDefinition();
  for (auto &Field : RD->getFields()) {
    auto QT = convertTypeL(Field.Type);
    auto *II = &Ctx.Idents.get(Field.Name);
    auto *FD = clang::FieldDecl::Create(
        Ctx, Ret, clang::SourceLocation(), clang::SourceLocation(), II, QT,
        nullptr, nullptr, false, clang::ICIS_NoInit);
    Ret->addDecl(FD);
    Field.setASTDecl(FD);
    FieldDeclMap[FD] = &Field;
  }
  Ret->completeDefinition();
  TUD->addDecl(Ret);
  return Ret;
}

void ClangTypeResult::defineDecls() {
  std::set<notdec::ast::TypedDecl *> Visited;

  // Because if one record is embedded, inner record need to be defined first.
  // So, Depth-first search to visit and define type.
  std::function<void(notdec::ast::TypedDecl *)> Visit =
      [&](notdec::ast::TypedDecl *Decl) -> void {
    std::function<void(notdec::ast::HType *)> VisitType =
        [&](notdec::ast::HType *Ty) {
          if (Ty->isArrayType()) {
            VisitType(Ty->getArrayElementType());
          } else if (Ty->isTypedefType()) {
            VisitType(Ty->getAsTypedefDecl()->getType());
          } else if (Ty->isRecordType()) {
            Visit(Ty->getAsRecordDecl());
          } else if (Ty->isUnionType()) {
            Visit(Ty->getAsUnionDecl());
          }
          // Skip simple type, pointer type, function type
        };

    if (Decl == Result->MemoryDecl) {
      return;
    }

    if (Visited.count(Decl)) {
      return;
    }
    Visited.insert(Decl);

    auto OldDecl = Decl->getASTDecl();
    assert(OldDecl != nullptr && "Decl not declared?");
    clang::Decl *ASTDecl = nullptr;
    if (auto *RD = llvm::dyn_cast<ast::RecordDecl>(Decl)) {
      for (auto &Field : RD->getFields()) {
        VisitType(Field.Type);
      }
      auto *CDecl = convertStruct(RD);
      CDecl->setPreviousDecl(llvm::cast<clang::TagDecl>(OldDecl));
      ASTDecl = CDecl;
    } else if (auto *UD = llvm::dyn_cast<ast::UnionDecl>(Decl)) {
      for (auto &Member : UD->getMembers()) {
        VisitType(Member.Type);
      }
      auto *CDecl = convertUnion(UD);
      CDecl->setPreviousDecl(llvm::cast<clang::TagDecl>(OldDecl));
      ASTDecl = CDecl;
    } else if (auto *TD = llvm::dyn_cast<ast::TypedefDecl>(Decl)) {
      // typedef only need declaration, no need to create definition.
      return;
    } else {
      assert(false && "Unknown Decl type");
    }

    // replaced with definition.
    Decl->setASTDecl(ASTDecl);
    DeclMap[ASTDecl] = Decl;
  };

  // Add all decls but memory and maintain DeclMap
  for (auto &Ent : Result->HTCtx->getDecls()) {
    auto *Decl = Ent.second.get();
    Visit(Decl);
  }

  // Create the Memory types
  createMemoryDecls();
}

void ClangTypeResult::declareDecls() {
  assert(!createNonFreeStandingStruct && "TODO");
  // if (createNonFreeStandingStruct) {
  //   calcUseRelation();
  // }

  auto *TUD = AM->getTypeDeclarations();
  // Declare all decls but memory
  for (auto &Ent : Result->HTCtx->getDecls()) {
    auto *Decl = Ent.second.get();
    if (Decl == Result->MemoryDecl) {
      continue;
    }
    clang::Decl *ASTDecl = nullptr;
    if (auto *RD = llvm::dyn_cast<ast::RecordDecl>(Decl)) {
      auto *CDecl =
          createRecordDecl(Ctx, TUD, clang::TTK_Struct, RD->getName());
      ASTDecl = CDecl;
    } else if (auto *UD = llvm::dyn_cast<ast::UnionDecl>(Decl)) {
      auto *CDecl = createRecordDecl(Ctx, TUD, clang::TTK_Union, UD->getName());
      ASTDecl = CDecl;
    } else if (auto *TD = llvm::dyn_cast<ast::TypedefDecl>(Decl)) {
      clang::TypeSourceInfo *TInfo =
          Ctx.getTrivialTypeSourceInfo(convertType(TD->getType()));
      auto *CDecl = clang::TypedefDecl::Create(
          Ctx, TUD, clang::SourceLocation(), clang::SourceLocation(),
          &Ctx.Idents.get(TD->getName()), TInfo);
      ASTDecl = CDecl;
    } else {
      assert(false && "Unknown Decl type");
    }

    // declaration in this field will be replaced with definition later.
    Decl->setASTDecl(ASTDecl);
    TUD->addDecl(ASTDecl);
  }
}

llvm::APInt stringToAPInt(const std::string &bytes,
                          llvm::support::endianness endian) {
  const char *ptr = bytes.data();
  size_t len = bytes.size();
  size_t remaining = len;
  std::vector<uint64_t> Data;

  // 处理完整的8字节块
  while (remaining >= sizeof(uint64_t)) {
    uint64_t word = llvm::support::endian::readNext<uint64_t, 1>(ptr, endian);
    Data.push_back(word);
    remaining -= sizeof(uint64_t);
  }

  // 处理剩余字节（不足8字节的情况）
  if (remaining > 0) {
    std::array<uint8_t, sizeof(uint64_t)> buffer = {0};
    std::memcpy(buffer.data(), ptr, remaining);
    uint64_t word = llvm::support::endian::read<uint64_t, 1>(
        reinterpret_cast<const char *>(buffer.data()), endian);
    Data.push_back(word);
  }

  unsigned numBits = bytes.size() * 8;
  return llvm::APInt(numBits, llvm::makeArrayRef(Data.data(), Data.size()));
}

void ClangTypeResult::increaseArraySize(ValueDecl *Decl, int64_t Size) {
  auto AT = llvm::cast<clang::ConstantArrayType>(Decl->getType());
  auto OldSize = AT->getSize().getZExtValue();
  if (OldSize >= Size) {
    return;
  }
  auto NewAT =
      Ctx.getConstantArrayType(AT->getElementType(), llvm::APInt(32, Size),
                               nullptr, clang::ArrayType::Normal, 0);
  Decl->setType(NewAT);
}

clang::Expr *
ClangTypeResult::decodeInitializer(std::variant<QualType, ValueDecl *> DeclOrTy,
                                   BytesManager &Bytes) {
  ValueDecl *Decl = nullptr;
  clang::QualType Ty;
  if (auto Declp = std::get_if<ValueDecl *>(&DeclOrTy)) {
    Decl = *Declp;
    Ty = Decl->getType();
  } else {
    Ty = std::get<QualType>(DeclOrTy);
  }

  if (Ty->isIntegerType()) {
    auto BitSize = Ctx.getTypeSize(Ty);
    assert(BitSize > 0);
    auto Bs = Bytes.getRange(
        SimpleRange{.Start = 0, .Size = static_cast<OffsetTy>(BitSize / 8)});
    if (!Bs) {
      return nullptr;
    }
    auto Val = stringToAPInt(*Bs, llvm::support::endianness::little);
    return clang::IntegerLiteral::Create(Ctx, Val, Ty, clang::SourceLocation());
  } else if (Ty->isPointerType()) {
    auto BitSize = Ctx.getTypeSize(Ty);
    assert(BitSize > 0);
    auto Bs = Bytes.getRange(
        SimpleRange{.Start = 0, .Size = static_cast<OffsetTy>(BitSize / 8)});
    if (!Bs) {
      return nullptr;
    }
    auto Val = stringToAPInt(*Bs, llvm::support::endianness::little);
    return getGlobal(Val.getSExtValue());
  } else if (Ty->isRecordType()) {
    auto *RD = Ty->getAsRecordDecl();
    auto ASTDecl = DeclMap.at(RD)->getAs<ast::RecordDecl>();
    llvm::SmallVector<clang::Expr *> Inits;
    if (RD->getTagKind() == clang::TTK_Struct) {
      auto &FS = ASTDecl->getFields();
      for (size_t i = 0; i < FS.size(); i++) {
        auto &Field = FS[i];
        auto VD = llvm::cast<clang::ValueDecl>(Field.ASTDecl);
        llvm::SmallVector<DesignatedInitExpr::Designator> Designators;
        llvm::SmallVector<clang::Expr *> IndexExprs;
        Designators.push_back(DesignatedInitExpr::Designator(
            VD->getIdentifier(), clang::SourceLocation(),
            clang::SourceLocation()));

        OffsetTy EndOff = std::numeric_limits<OffsetTy>::max();
        if (i + 1 < FS.size()) {
          EndOff = FS[i + 1].R.Start;
        }

        auto Bs = Bytes.getRange(SimpleRange{.Start = Field.R.Start,
                                             .Size = EndOff - Field.R.Start});
        if (!Bs) {
          continue;
        }
        auto BM1 = BytesManager::fromOneString(*Bs);
        auto Init = decodeInitializer(VD, *BM1);
        auto DIE = clang::DesignatedInitExpr::Create(
            Ctx, Designators, IndexExprs, clang::SourceLocation(), false, Init);
        Inits.push_back(DIE);
      }
    } else if (RD->getTagKind() == clang::TTK_Union) {
      // TODO select which for init expr?
      // Select first member.
      auto *VD = *RD->fields().begin();
      auto Init = decodeInitializer(VD, Bytes);
      if (Init != nullptr) {
        llvm::SmallVector<DesignatedInitExpr::Designator> Designators;
        llvm::SmallVector<clang::Expr *> IndexExprs;

        Designators.push_back(DesignatedInitExpr::Designator(
            VD->getIdentifier(), clang::SourceLocation(),
            clang::SourceLocation()));

        auto DIE = clang::DesignatedInitExpr::Create(
            Ctx, Designators, IndexExprs, clang::SourceLocation(), false, Init);
        Inits.push_back(DIE);
      }
    }

    return new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), Inits,
                                         clang::SourceLocation());
  } else if (Ty->isArrayType()) {
    assert(llvm::isa<clang::ConstantArrayType>(Ty.getTypePtr()) &&
           "Not ConstantArrayType?");
    auto *AT = llvm::cast<clang::ConstantArrayType>(Ty.getTypePtr());
    auto ElemTy = AT->getElementType();
    if (ElemTy->isCharType()) {
      auto Str = Bytes.decodeCStr(0);
      if (!Str.empty()) {
        auto SSize = Str.size();
        if (Decl) {
          increaseArraySize(Decl, SSize);
        }
        if (Str.back() == '\0') {
          SSize -= 1;
          Str.pop_back();
        }
        return clang::StringLiteral::Create(
            Ctx, Str, clang::StringLiteral::Ascii, false,
            Ctx.getStringLiteralArrayType(ElemTy, SSize),
            clang::SourceLocation());
      }
    }
    auto ElemSize = getTypeSizeInChars(ElemTy);
    if (auto Err = ElemSize.takeError()) {
      llvm::errs()
          << "ClangTypeResult::decodeInitializer: Failed to get element size: "
          << Err << "\n";
      std::abort();
    }
    llvm::SmallVector<clang::Expr *> Inits;
    for (unsigned Off = 0; true; Off += *ElemSize) {
      auto Bs =
          Bytes.getRange(SimpleRange{.Start = Off, .Size = Off + *ElemSize});
      if (!Bs) {
        break;
      }
      auto BM1 = BytesManager::fromOneString(*Bs);
      auto Init = decodeInitializer(ElemTy, *BM1);
      Inits.push_back(Init);
    }
    if (Inits.size() == 0) {
      return nullptr;
    }
    if (Decl) {
      increaseArraySize(Decl, Inits.size());
    }
    return new (Ctx) clang::InitListExpr(Ctx, clang::SourceLocation(), Inits,
                                         clang::SourceLocation());
  } else {
    assert(false && "Unsupported type for initializer");
  }
}

void ClangTypeResult::createMemoryDecls() {
  auto *MDecl = Result->MemoryDecl;
  auto *TUD = AM->getGlobalDefinitions();

  if (MDecl) {
    if (expandMemory) {
      // split the memory type into individual var decls.
      auto &FS = MDecl->getFields();
      for (size_t i = 0; i < FS.size(); i++) {
        auto &Field = FS[i];
        auto Name = Field.Name;
        if (startswith(Name, "field_")) {
          Name = Name.substr(strlen("field_"));
          Name = "global_" + Name;
        }
        clang::IdentifierInfo *II = &Ctx.Idents.get(Name);
        auto CType = convertTypeL(Field.Type);
        clang::VarDecl *VD = clang::VarDecl::Create(
            Ctx, TUD, clang::SourceLocation(), clang::SourceLocation(), II,
            CType, nullptr, clang::SC_None);

        // handle initializer
        if (MDecl->getBytesManager()) {
          auto EndRange = std::numeric_limits<int64_t>::max();
          if (i + 1 < FS.size()) {
            EndRange = FS[i + 1].R.Start;
          }
          auto SubBytes =
              MDecl->getBytesManager()->getSubBytes(Field.R.Start, EndRange);
          clang::Expr *Init = decodeInitializer(VD, SubBytes);
          VD->setInit(Init);
        }

        TUD->addDecl(VD);
        Field.setASTDecl(VD);
        FieldDeclMap[VD] = &Field;
      }
    } else {
      // create non-freestanding memory type.
      auto *CDecl = convertStruct(MDecl, true);
      MDecl->setASTDecl(CDecl);
      CDecl->setFreeStanding(false);
      auto ElabTy = Ctx.getElaboratedType(clang::ETK_Struct, nullptr,
                                          Ctx.getRecordType(CDecl), CDecl);

      DeclMap[CDecl] = MDecl;

      auto *II = &Ctx.Idents.get("MEMORY");
      MemoryVar = clang::VarDecl::Create(Ctx, TUD, clang::SourceLocation(),
                                         clang::SourceLocation(), II, ElabTy,
                                         nullptr, clang::SC_None);
      if (MDecl->getBytesManager()) {
        clang::Expr *Init =
            decodeInitializer(MemoryVar, *MDecl->getBytesManager());
        MemoryVar->setInit(Init);
      }
      TUD->addDecl(MemoryVar);
      DeclMap[MemoryVar] = MDecl;
    }
  }
}

bool ClangTypeResult::isTypeCompatible(clang::ASTContext &Ctx,
                                       clang::QualType From,
                                       clang::QualType To) {
  // char[0] vs char (*)[32]
  // if (From->isPointerType() && From->getPointeeType()->isArrayType()) {
  //   return isTypeCompatible(From->getPointeeType(), To);
  // }
  // if (To->isPointerType() && To->getPointeeType()->isArrayType()) {
  //   return isTypeCompatible(From, To->getPointeeType());
  // }

  if (From->isArrayType()) {
    From = Ctx.getDecayedType(From);
  }
  if (To->isArrayType()) {
    To = Ctx.getDecayedType(To);
  }

  if (From == To || From.getCanonicalType() == To.getCanonicalType()) {
    return true;
  }
  if (From->isReferenceType() || To->isReferenceType()) {
    assert(false &&
           "TODO isTypeCompatible: reference type is not supposed to exist.");
  }

  if (From->isPointerType() && To->isPointerType()) {
    return isTypeCompatible(Ctx, From->getPointeeType(), To->getPointeeType());
  } else if (From->isPointerType() != To->isPointerType()) {
    // pointer and non-pointer
    return false;
  }
  if (From->isBooleanType() && To->isIntegerType()) {
    return false;
  } else if (From->isIntegerType() && To->isBooleanType()) {
    return false;
  }
  // uncompatible if different size
  if (Ctx.getTypeSize(From) != Ctx.getTypeSize(To)) {
    return false;
  }
  if (From->isIntegerType() && To->isIntegerType()) {
    return false;
  }
  if (From->isStructureOrClassType() || To->isStructureOrClassType()) {
    return false;
  }

  if (From->isVoidType()) {
    return false;
  }
  if (To->isVoidType()) {
    return false;
  }

  if (From->isFloatingType()) {
    return false;
  }
  if (To->isFloatingType()) {
    return false;
  }

  if (From->isAggregateType()) {
    return false;
  }
  if (To->isAggregateType()) {
    return false;
  }

  if (From->isFunctionType() && To->isFunctionType()) {
    return false;
  }
  From->dump();
  To->dump();
  assert(false && "TODO isTypeCompatible: implement the rest.");
}

clang::Expr *ClangTypeResult::gepCast(clang::Expr *Val, clang::QualType To) {
  // try to cast using gep
  if (Val->getType()->isPointerType()) {
    clang::Expr *R = Val;
    auto Exprs = tryAddZero(R);
    for (auto R : Exprs) {
      if (isTypeCompatible(Ctx, R->getType(), To)) {
        return R;
      }
    }
  }
  return nullptr;
}

std::vector<clang::Expr *> ClangTypeResult::tryAddZero(clang::Expr *Val) {
  std::vector<clang::Expr *> Result;
  assert(Val->getType()->isPointerType());

  std::function<void(clang::Expr *)> addZero = [&](clang::Expr *Val) {
    auto PointeeTy = Val->getType()->getPointeeOrArrayElementType();
    if (llvm::isa<clang::RecordType>(PointeeTy)) {
      auto *Decl = llvm::cast<clang::RecordDecl>(PointeeTy->getAsTagDecl());
      if (Decl->getTagKind() == clang::TTK_Struct) {
        auto ASTDecl = DeclMap.at(Decl)->getAs<ast::RecordDecl>();
        const ast::FieldDecl *Target = ASTDecl->getFieldAt(0);
        if (Target != nullptr) {
          auto FD = llvm::cast<clang::FieldDecl>(Target->ASTDecl);
          assert(FD != nullptr && "Field not declared?");
          auto *ME = createMemberExpr(Ctx, Val, FD);
          auto R = addrOf(Ctx, ME);
          Result.push_back(R);
          return addZero(R);
        }
      } else if (Decl->getTagKind() == clang::TTK_Union) {
        for (auto *F : Decl->fields()) {
          auto *ME = createMemberExpr(Ctx, Val, F);
          auto R = addrOf(Ctx, ME);
          Result.push_back(R);
          addZero(R);
        }
      } else {
        Decl->dump();
        llvm::errs()
            << "Error: Unsupported Record DeclKind for pointer arithmetic: "
            << Decl->getKindName() << "\n";
      }
    } else if (auto ArrayTy = PointeeTy->getAsArrayTypeUnsafe()) {
      auto ElemTy = ArrayTy->getElementType();
      auto Index = clang::IntegerLiteral::Create(
          Ctx, llvm::APInt(32, 0, false), Ctx.IntTy, clang::SourceLocation());
      auto AS = new (Ctx) clang::ArraySubscriptExpr(
          deref(Ctx, Val), Index, ElemTy, clang::VK_LValue, clang::OK_Ordinary,
          clang::SourceLocation());
      auto R = addrOf(Ctx, AS);
      Result.push_back(R);
      return addZero(R);
    }
  };

  addZero(Val);
  return Result;
}

std::optional<int64_t> getIntValue(clang::Expr *Index) {
  std::optional<int64_t> OffsetNum;
  while (auto Cast = llvm::dyn_cast<clang::CastExpr>(Index)) {
    Index = Cast->getSubExpr();
  }
  if (auto IntegerLiteral = llvm::dyn_cast<clang::IntegerLiteral>(Index)) {
    OffsetNum =
        llvm::cast<clang::IntegerLiteral>(Index)->getValue().getSExtValue();
  }
  return OffsetNum;
}

std::optional<int> isPowerOfTwo(int n) {
  if (n <= 0 || (n & (n - 1)) != 0) {
    return std::nullopt;
  }
  int count = 0;
  while (n > 1) {
    n >>= 1;
    ++count;
  }
  return count;
}

int myPow(int x, unsigned int p) {
  if (p == 0)
    return 1;
  if (p == 1)
    return x;

  int tmp = myPow(x, p / 2);
  if (p % 2 == 0)
    return tmp * tmp;
  else
    return x * tmp * tmp;
}

clang::Expr *tryDivideBySize(clang::ASTContext &Ctx, clang::Expr *Index,
                             int Size) {
  if (auto IntVal = getIntValue(Index)) {
    if (*IntVal % Size == 0) {
      return clang::IntegerLiteral::Create(
          Ctx, llvm::APInt(32, *IntVal / Size, false), Ctx.IntTy,
          clang::SourceLocation());
    }
  }
  // try to match a multiply of constant.
  if (auto *BO = llvm::dyn_cast<clang::BinaryOperator>(Index)) {
    if (BO->getOpcode() == clang::BO_Mul) {
      if (auto IntVal = getIntValue(BO->getRHS())) {
        if (*IntVal == Size) {
          return BO->getLHS();
        } else if (*IntVal % Size == 0) {
          assert(false && "todo");
        }
      }
    }
    if (BO->getOpcode() == clang::BO_Shl) {
      if (auto IntVal = getIntValue(BO->getRHS())) {
        auto Mul = myPow(2, *IntVal);
        if (Mul == Size) {
          return BO->getLHS();
        } else if (*IntVal % Size == 0) {
          assert(false && "todo");
        }
      }
    }
  }
  return nullptr;
}

llvm::Expected<int64_t>
ClangTypeResult::getTypeSizeInChars(const clang::Type *Ty) {
  if (llvm::isa<clang::RecordType>(Ty)) {
    auto ASTDecl = DeclMap.at(Ty->getAsRecordDecl());
    auto Range = ast::getRange(ASTDecl);
    if (Range.Start < 0) {
      return llvm::createStringError(
          llvm::inconvertibleErrorCode(),
          "Invalid start offset for a valid size: %d", Range.Start);
    }
    return Range.Start + Range.Size;
  }
  return Ctx.getTypeSizeInChars(Ty).getQuantity();
}

// TODO: How to handle Union with multiple possible solutions?
clang::Expr *ClangTypeResult::tryHandlePtrAdd(clang::Expr *Base,
                                              clang::Expr *Index) {
  assert((Base->getType()->isPointerType() || Base->getType()->canDecayToPointerType()) &&
         "tryHandlePtrAdd: Base is not a pointer type");
  clang::QualType ValQTy = Base->getType();
  const clang::Type *ValTy = Base->getType().getTypePtr();

  // 1. pointer arithmetic is only allowed in the beginning, and only once.
  std::optional<int64_t> OffsetNum = getIntValue(Index);
  if (ValTy->isPointerType()) {
    auto ElemTy = Ctx.getCanonicalType(ValTy->getPointeeType().getTypePtr());
    auto ElemSize = llvm::expectedToOptional(getTypeSizeInChars(ElemTy));
    // TODO if it is multiple of OffsetNum, try divide Index by ElemSize
    if (ElemSize && OffsetNum && *OffsetNum > *ElemSize) {
      // try to divide the index by ElemSize.
      auto NewIndex = *OffsetNum / *ElemSize;
      auto RemainIndex = *OffsetNum % *ElemSize;
      auto NewIndexExpr = clang::IntegerLiteral::Create(
          Ctx, llvm::APInt(32, NewIndex, false), Index->getType(),
          clang::SourceLocation());
      Base = createBinaryOperator(Ctx, Base, NewIndexExpr, clang::BO_Add,
                                  ValQTy, clang::VK_LValue);
      if (RemainIndex == 0) {
        return Base;
      } else {
        // set the remainder index.
        Index = clang::IntegerLiteral::Create(
            Ctx, llvm::APInt(32, RemainIndex, false), Index->getType(),
            clang::SourceLocation());
      }
    }
  }

  while (Index != nullptr) {
    ValQTy = Base->getType();
    const clang::Type *ValTy = ValQTy.getTypePtr();
    auto PointeeTy =
        Ctx.getCanonicalType(ValTy->getPointeeOrArrayElementType());
    auto DerefBase = deref(Ctx, Base);
    // try to get the constant offset.
    OffsetNum = getIntValue(Index);

    if (OffsetNum && OffsetNum.value() == 0) {
      // zero offset means finished
      return Base;
    }

    // Struct or Union type, but index is not constant: fail?
    if (llvm::isa<clang::RecordType>(PointeeTy)) {
      if (!OffsetNum) {
        // TODO try to resolve this case using zero offset conversion?
        llvm::errs() << "Error: Struct/Union type with non-constant index\n";
        return nullptr;
      }
      auto *Decl = llvm::cast<clang::RecordDecl>(PointeeTy->getAsTagDecl());
      if (Decl->getTagKind() == clang::TTK_Struct) {
        auto ASTDecl = DeclMap.at(Decl)->getAs<ast::RecordDecl>();
        const ast::FieldDecl *Target = ASTDecl->getFieldAt(*OffsetNum);
        if (Target == nullptr) {
          Decl->dump();
          llvm::errs() << "Error: failed to get field at offset: " << *OffsetNum
                       << "\n";
          return nullptr;
        }
        assert(Target->ASTDecl != nullptr && "Field not declared?");
        auto FD = llvm::cast<clang::FieldDecl>(Target->ASTDecl);

        auto *ME = createMemberExpr(Ctx, DerefBase, FD);

        Base = addrOf(Ctx, ME);
        auto RemainingOffset = *OffsetNum - Target->R.Start;
        if (RemainingOffset == 0) {
          return Base;
        } else {
          Index = clang::IntegerLiteral::Create(
              Ctx, llvm::APInt(32, RemainingOffset, false), Index->getType(),
              clang::SourceLocation());
        }
      } else if (Decl->getTagKind() == clang::TTK_Union) {
        // assert(false && "todo");
        llvm::errs() << "Warning: TODO how to handle ptradd for union?\n";
        auto *ME = createMemberExpr(Ctx, DerefBase, *Decl->field_begin());
        return handlePtrAdd(addrOf(Ctx, ME), Index);
      } else {
        Decl->dump();
        llvm::errs()
            << "Error: Unsupported Record DeclKind for pointer arithmetic: "
            << Decl->getKindName() << "\n";
        return nullptr;
      }
    } else if (auto ArrayTy = PointeeTy->getAsArrayTypeUnsafe()) {
      // Array type: try to divide by size and convert to array indexing?
      auto ElemTy = ArrayTy->getElementType();
      // divide the offset by the size of the type.
      auto ElemSize = getTypeSizeInChars(ElemTy);
      if (auto Err = ElemSize.takeError()) {
        llvm::errs() << "Failed to get ArrayElem Size "
                     << toString(std::move(Err)) << "\n";
        std::abort();
      }

      clang::Expr *CurrentIndex = nullptr;
      // TODO Remaining Offset?
      // clang::Expr *RestIndex = nullptr;
      if (*ElemSize > 1) {
        // try to divide the index by ElemSize.
        CurrentIndex = tryDivideBySize(Ctx, Index, *ElemSize);
        if (!CurrentIndex) {
          // failed to create array subscript
          ValTy->dump();
          llvm::errs()
              << "Error: failed to create array subscript, cannnot divide by"
              << *ElemSize << ":\n";
          Index->dump();
          return nullptr;
        }
      } else {
        // Array size is 1, no need to divide by 1.
        CurrentIndex = Index;
      }

      // Create array indexing?
      auto AS = new (Ctx) clang::ArraySubscriptExpr(
          deref(Ctx, Base), CurrentIndex, ElemTy, clang::VK_LValue,
          clang::OK_Ordinary, clang::SourceLocation());

      Base = addrOf(Ctx, AS);
      // TODO Remaining Offset?
      // auto RemainingOffset = *OffsetNum % ElemSize;
      // if (RemainingOffset == 0) {
      return Base;
      // } else {
      //   Index = clang::IntegerLiteral::Create(
      //       Ctx, llvm::APInt(32, RemainingOffset, false), Index->getType(),
      //       clang::SourceLocation());
      // }
    } else {
      ValTy->dump();
      llvm::errs() << "Error: Unsupported type for pointer arithmetic\n";
      return nullptr;
    }
  }
  // when done, should directly return. only break when failed.
  return nullptr;
}

clang::Expr *ClangTypeResult::handlePtrAdd(clang::Expr *Val,
                                           clang::Expr *Index) {
  assert((Val->getType()->isPointerType() || Val->getType()->canDecayToPointerType()) &&
         "handlePtrAdd: Val is not a pointer type");
  auto Result = tryHandlePtrAdd(Val, Index);
  if (Result != nullptr) {
    return Result;
  }
  // create cast to void* and add.
  auto CharPtrTy = Ctx.getPointerType(Ctx.CharTy);
  auto Casted = createCStyleCastExpr(Ctx, CharPtrTy, clang::VK_LValue,
                                     clang::CK_BitCast, Val);
  auto Add = createBinaryOperator(Ctx, Casted, Index, clang::BO_Add, CharPtrTy,
                                  clang::VK_LValue);
  return Add;
}

clang::Expr *ClangTypeResult::getGlobal(int64_t Offset) {
  if (!expandMemory) {
    return handlePtrAdd(
        addrOf(Ctx, makeDeclRefExpr(MemoryVar)),
        clang::IntegerLiteral::Create(Ctx, llvm::APInt(32, Offset, false),
                                      Ctx.IntTy, clang::SourceLocation()));
  } else {
    // unroll handlePtrAdd for memory.
    auto MemASTDecl = Result->MemoryDecl;
    const ast::FieldDecl *Target = MemASTDecl->getFieldAt(Offset);
    if (Target != nullptr) {
      auto RemainingOffset = Offset - Target->R.Start;
      auto Var = llvm::cast<clang::VarDecl>(Target->ASTDecl);
      if (RemainingOffset == 0) {
        return addrOf(Ctx, makeDeclRefExpr(Var));
      } else {
        return handlePtrAdd(addrOf(Ctx, makeDeclRefExpr(Var)),
                            clang::IntegerLiteral::Create(
                                Ctx, llvm::APInt(32, RemainingOffset, false),
                                Ctx.IntTy, clang::SourceLocation()));
      }
    } else {
      // failed to get the global.
      llvm::errs() << "Error: failed to get global at offset: " << Offset
                   << "\n";
      return nullptr;
    }
  }
}
} // namespace notdec::llvm2c
