#include "notdec-llvm2c/TypeManager.h"
#include "Interface/HType.h"
#include "StructuralAnalysis.h"
#include "Utils.h"
#include <cassert>
#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/Type.h>
#include <clang/Basic/Specifiers.h>
#include <llvm/Support/Error.h>
#include <optional>

namespace notdec::llvm2c {

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
    } else {
      assert(false && "convertDecl: Unhandled type");
    }
  };

  auto Ret = doConvert(T);
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
    auto QT = convertType(Member.Type);
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

clang::RecordDecl *ClangTypeResult::convertStruct(ast::RecordDecl *RD, bool isMemory) {
  auto *TUD = AM->getTypeDefinitions();
  if (isMemory) {
    TUD = AM->getGlobalDefinitions();
  }
  auto Ret = createRecordDecl(Ctx, TUD, clang::TTK_Struct, RD->getName());
  Ret->startDefinition();
  for (auto &Field : RD->getFields()) {
    auto QT = convertType(Field.Type);
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

  // Add all decls but memory and maintain DeclMap
  for (auto &Ent : Result->HTCtx->getDecls()) {
    auto *Decl = Ent.second.get();
    if (Decl == Result->MemoryDecl) {
      continue;
    }
    auto OldDecl = Decl->getASTDecl();
    assert(OldDecl != nullptr && "Decl not declared?");
    clang::Decl *ASTDecl = nullptr;
    if (auto *RD = llvm::dyn_cast<ast::RecordDecl>(Decl)) {
      auto *CDecl = convertStruct(RD);
      CDecl->setPreviousDecl(llvm::cast<clang::TagDecl>(OldDecl));
      ASTDecl = CDecl;
    } else if (auto *UD = llvm::dyn_cast<ast::UnionDecl>(Decl)) {
      auto *CDecl = convertUnion(UD);
      CDecl->setPreviousDecl(llvm::cast<clang::TagDecl>(OldDecl));
      ASTDecl = CDecl;
    } else if (auto *TD = llvm::dyn_cast<ast::TypedefDecl>(Decl)) {
      // no need to create typedef.
      continue;
    } else {
      assert(false && "Unknown Decl type");
    }

    // replaced with definition.
    Decl->setASTDecl(ASTDecl);
    DeclMap[ASTDecl] = Decl;
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
      auto *CDecl = createRecordDecl(Ctx, TUD, clang::TTK_Union, RD->getName());
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

void ClangTypeResult::createMemoryDecls() {
  auto *MDecl = Result->MemoryDecl;
  auto *TUD = AM->getGlobalDefinitions();

  if (expandMemory) {
    // split the memory type into individual var decls.
    for (auto &Field : MDecl->getFields()) {
      auto Name = Field.Name;
      if (startswith(Name, "field_")) {
        Name = Name.substr(strlen("field_"));
        Name = "global_" + Name;
      }
      clang::IdentifierInfo *II = &Ctx.Idents.get(Name);
      clang::VarDecl *VD = clang::VarDecl::Create(
          Ctx, TUD, clang::SourceLocation(), clang::SourceLocation(), II,
          convertType(Field.Type), nullptr, clang::SC_None);

      // handle initializer
      // VD->setInit();

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
    TUD->addDecl(MemoryVar);
    DeclMap[MemoryVar] = MDecl;
  }
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
        assert(false && "todo");
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
  }
  return nullptr;
}

// TODO: How to handle Union with multiple possible solutions?
clang::Expr *ClangTypeResult::tryHandlePtrAdd(clang::Expr *Base,
                                              clang::Expr *Index) {
  clang::QualType ValQTy = Base->getType();
  const clang::Type *ValTy = Base->getType().getTypePtr();

  // 1. pointer arithmetic is only allowed in the beginning, and only once.
  std::optional<int64_t> OffsetNum = getIntValue(Index);
  if (ValTy->isPointerType()) {
    auto ElemTy = ValTy->getPointeeType();
    auto ElemSize = Ctx.getTypeSizeInChars(ElemTy).getQuantity();
    if (OffsetNum && *OffsetNum > ElemSize) {
      // try to divide the index by ElemSize.
      auto NewIndex = *OffsetNum / ElemSize;
      auto RemainIndex = *OffsetNum % ElemSize;
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
    auto PointeeTy = ValTy->getPointeeOrArrayElementType();
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
        assert(false && "todo");
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
      auto ElemSize = Ctx.getTypeSizeInChars(ElemTy).getQuantity();

      clang::Expr *CurrentIndex = nullptr;
      // TODO Remaining Offset?
      // clang::Expr *RestIndex = nullptr;
      if (ElemSize > 1) {
        // try to divide the index by ElemSize.
        CurrentIndex = tryDivideBySize(Ctx, Index, ElemSize);
        if (!CurrentIndex) {
          // failed to create array subscript
          ValTy->dump();
          llvm::errs()
              << "Error: failed to create array subscript, cannnot divide by"
              << ElemSize << ":\n";
          Index->dump();
          return nullptr;
        }
      } else {
        // Array size is 1, no need to divide by 1.
        CurrentIndex = Index;
      }

      // Create array indexing?
      auto AS = new (Ctx) clang::ArraySubscriptExpr(
          deref(Ctx, Base), Index, ElemTy, clang::VK_LValue, clang::OK_Ordinary,
          clang::SourceLocation());

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
  auto Result = tryHandlePtrAdd(Val, Index);
  if (Result != nullptr) {
    return Result;
  }
  // create cast to void* and add.
  auto CharPtrTy = Ctx.getPointerType(Ctx.Char8Ty);
  auto Casted = createCStyleCastExpr(Ctx, CharPtrTy, clang::VK_LValue,
                                     clang::CK_BitCast, Val);
  auto Add = createBinaryOperator(Ctx, Casted, Index, clang::BO_Add, CharPtrTy,
                                  clang::VK_LValue);
  return Add;
}

clang::Expr *ClangTypeResult::getGlobal(int64_t Offset) {
  if (!expandMemory) {
    return handlePtrAdd(
        makeDeclRefExpr(MemoryVar),
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
        return handlePtrAdd(makeDeclRefExpr(Var),
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
