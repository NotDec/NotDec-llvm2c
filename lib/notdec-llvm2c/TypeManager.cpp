#include "notdec-llvm2c/TypeManager.h"
#include "Interface/HType.h"
#include <cassert>
#include <clang/AST/Decl.h>

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

clang::RecordDecl *createStruct(clang::ASTContext &Ctx, llvm::StringRef Name) {
  auto *II = &Ctx.Idents.get(Name);
  // Create a Struct type for it.
  clang::RecordDecl *decl = clang::RecordDecl::Create(
      Ctx, clang::TagDecl::TagKind::TTK_Struct, Ctx.getTranslationUnitDecl(),
      clang::SourceLocation(), clang::SourceLocation(), II, nullptr);
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

void ClangTypeResult::addDecls() {
  if (createNonFreeStandingStruct) {
    calcUseRelation();
  }

  assert(false && "TODO");
  // TODO
  //  if the memory type need to be expanded, then expand
  //  if the stack need to be expanded.
  //  Add all decls.
  // if (HT != nullptr) {
  //   // Skip memory type for flattening..
  //   RecordDecl *MDecl;
  //   if (!HT->MemoryType.isNull()) {
  //     MDecl = HT->MemoryType->getAs<clang::RecordType>()->getDecl();
  //   }
  //   for (auto *Decl : HT->AllDecls) {
  //     if (Decl == MDecl) {
  //       continue;
  //     }
  //     HT->ASTUnit->getASTContext().getTranslationUnitDecl()->addDecl(Decl);
  //   }
  // }
}

void ClangTypeResult::createMemoryDecls() {
  assert(false && "TODO");
  // auto *MDecl = HT->MemoryType->getAs<clang::RecordType>()->getDecl();

  // // getASTContext().getTranslationUnitDecl()->addDecl(MDecl);
  // // clang::IdentifierInfo *II = getIdentifierInfo("MEMORY");
  // // Memory = clang::VarDecl::Create(
  // //     getASTContext(), getASTContext().getTranslationUnitDecl(),
  // //     clang::SourceLocation(), clang::SourceLocation(), II,
  // HT->MemoryType,
  // //     nullptr, clang::SC_None);
  // // getASTContext().getTranslationUnitDecl()->addDecl(Memory);

  // auto &SI = HT->StructInfos.at(MDecl);
  // // convert struct to vardecl.
  // for (auto &Field : SI.Fields) {
  //   FieldDecl *FD = llvm::cast<FieldDecl>(Field.Decl);
  //   auto Name = FD->getName();
  //   Name.consume_front("field_");
  //   clang::IdentifierInfo *II = getIdentifierInfo(("global_" + Name).str());
  //   clang::VarDecl *VD = clang::VarDecl::Create(
  //       getASTContext(), getASTContext().getTranslationUnitDecl(),
  //       clang::SourceLocation(), clang::SourceLocation(), II, FD->getType(),
  //       nullptr, clang::SC_None);
  //   if (FD->hasInClassInitializer()) {
  //     VD->setInit(FD->getInClassInitializer());
  //   }
  //   getASTContext().getTranslationUnitDecl()->addDecl(VD);
  //   Field.Decl = VD;
  // }
}

} // namespace notdec::llvm2c
