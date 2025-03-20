#ifndef _NOTDEC_BACKEND_TYPEMANAGER_H_
#define _NOTDEC_BACKEND_TYPEMANAGER_H_

#include <clang/AST/ASTContext.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclBase.h>
#include <clang/AST/Type.h>
#include <clang/Frontend/ASTUnit.h>
#include <cstdint>

#include "ASTManager.h"
#include "Interface.h"
#include "Interface/HType.h"

namespace notdec::llvm2c {

// remove all array types within.
clang::QualType removeArrayType(clang::ASTContext &Ctx, clang::QualType Ty);
clang::QualType toLValueType(clang::ASTContext &Ctx, clang::QualType Ty);

// 1) Covert HType to Clang type. 2) maintain additional infos for Clang Decls.
class ClangTypeResult {
  std::shared_ptr<HTypeResult> Result;
  std::shared_ptr<ASTManager> AM;
  clang::ASTContext &Ctx;
  bool expandStack = false;
  bool createNonFreeStandingStruct = false;

  // if not expandMemory, MemoryVar stores memory var with ElaboratedType;
  // if expandMemory, find globals in ast::FieldDecl's ASTDecl.
  bool expandMemory = true;
  clang::VarDecl *MemoryVar = nullptr;

  std::map<HType *, clang::QualType> TypeMap;
  // map from clang struct/union/typedef decl to ast decl.
  std::map<clang::Decl *, ast::TypedDecl *> DeclMap;
  // map from field decl to ast field decl. For globals, may map VarDecl to ast
  // FieldDecl
  std::map<clang::Decl *, const ast::FieldDecl *> FieldDeclMap;

  clang::Decl *getDecl(ast::TypedDecl *Decl) { return Decl->getASTDecl(); }
  clang::QualType convertType(HType *T);
  clang::QualType convertTypeL(HType *T) {
    return toLValueType(Ctx, convertType(T));
  }

  // map from struct/union decl to its users
  std::map<ast::TypedDecl *, std::set<ast::TypedDecl *>> DeclUsage;
  std::map<ast::TypedDecl *, std::set<ExtValuePtr>> ValueUsage;

public:
  void calcUseRelation();

public:
  ClangTypeResult(std::shared_ptr<HTypeResult> Result,
                  std::shared_ptr<ASTManager> AM)
      : Result(Result), AM(AM), Ctx(AM->getASTContext()) {}

  std::shared_ptr<ASTManager> getASTManager() { return AM; }
  bool hasType(ExtValuePtr Val, bool isUpperBound = false);
  clang::QualType getType(ExtValuePtr Val, bool isUpperBound = false);

  clang::RecordDecl *convertUnion(ast::UnionDecl *UD);
  clang::RecordDecl *convertStruct(ast::RecordDecl *RD, bool isMemory = false);

  void declareDecls();
  void defineDecls();
  // returns true if this type can be embedded.
  bool isNonFreeStanding(ast::TypedDecl *Decl) {
    if (Decl == Result->MemoryDecl) {
      return false;
    }
    unsigned UsageCount = 0;
    if (DeclUsage.count(Decl)) {
      UsageCount += DeclUsage.at(Decl).size();
    }
    if (ValueUsage.count(Decl)) {
      UsageCount += ValueUsage.at(Decl).size();
    }
    return UsageCount <= 1;
  }
  void createMemoryDecls();

  llvm::Expected<int64_t> getTypeSizeInChars(const clang::Type *Ty);
  llvm::Expected<int64_t> getTypeSizeInChars(QualType Ty) {
    return getTypeSizeInChars(Ty.getTypePtr());
  }
  clang::Expr *handlePtrAdd(clang::Expr *Val, clang::Expr *Index);
  // If the add cannot be handled properly, return nullptr.
  clang::Expr *tryHandlePtrAdd(clang::Expr *Val, clang::Expr *Index);
  std::vector<clang::Expr *> tryAddZero(clang::Expr *Val);
  clang::Expr *getGlobal(int64_t Offset);

  // For array type, we may modify decl's type
  clang::Expr *decodeInitializer(std::variant<QualType, ValueDecl *> DeclOrTy,
                                 BytesManager &Bytes);
  void increaseArraySize(ValueDecl *Decl, int64_t Size);

  std::string getComment(clang::Decl *Decl) {
    auto It = DeclMap.find(Decl);
    if (It != DeclMap.end()) {
      return It->second->getComment();
    }
    auto It2 = FieldDeclMap.find(Decl);
    if (It2 != FieldDeclMap.end()) {
      return It2->second->Comment;
    }
    return "";
  }
};

std::optional<int64_t> getIntValue(clang::Expr *Index);

} // namespace notdec::llvm2c

#endif
