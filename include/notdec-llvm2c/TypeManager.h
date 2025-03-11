#ifndef _NOTDEC_BACKEND_TYPEMANAGER_H_
#define _NOTDEC_BACKEND_TYPEMANAGER_H_

#include <clang/AST/ASTContext.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclBase.h>
#include <clang/AST/Type.h>
#include <clang/Frontend/ASTUnit.h>

#include "Interface.h"
#include "Interface/HType.h"

namespace notdec::llvm2c {

// 1) Covert HType to Clang type. 2) maintain additional infos for Clang Decls.
class ClangTypeResult {
  std::shared_ptr<HTypeResult> Result;
  std::shared_ptr<clang::ASTUnit> AST;
  clang::ASTContext &Ctx;
  bool expandMemory = true;
  bool expandStack = false;
  bool createNonFreeStandingStruct = false;

  std::map<HType *, clang::QualType> TypeMap;
  std::map<clang::Decl *, ast::TypedDecl *> DeclMap;
  std::map<clang::FieldDecl *, ast::FieldDecl *> FieldDeclMap;

  clang::Decl *getDecl(ast::TypedDecl *Decl) { return Decl->getASTDecl(); }
  clang::QualType convertType(HType *T);


  // map from struct/union decl to its users
  std::map<ast::TypedDecl *, std::set<ast::TypedDecl *>> DeclUsage;
  std::map<ast::TypedDecl *, std::set<ExtValuePtr>> ValueUsage;
public:
  void calcUseRelation();


public:
  ClangTypeResult(std::shared_ptr<HTypeResult> Result,
                  std::shared_ptr<clang::ASTUnit> AST)
      : Result(Result), AST(AST), Ctx(AST->getASTContext()) {}

  bool hasType(ExtValuePtr Val, bool isUpperBound = false);
  clang::QualType getType(ExtValuePtr Val, bool isUpperBound = false);

  void addDecls();
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

  std::string getComment(clang::Decl *Decl) {
    auto It = DeclMap.find(Decl);
    if (It != DeclMap.end()) {
      return It->second->getComment();
    }
    return "";
  }
};

} // namespace notdec::llvm2c

#endif
