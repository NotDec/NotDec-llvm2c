
#ifndef _NOTDEC_BACKEND_ASTMANAGER_H_
#define _NOTDEC_BACKEND_ASTMANAGER_H_

#include "ASTPrinter/DeclPrinter.h"
#include <clang/AST/DeclBase.h>
#include <clang/Frontend/ASTUnit.h>
#include <memory>
#include <vector>
namespace notdec::llvm2c {

// Replace the Compilation Unit Decls for better printing.
class ASTManager {
  llvm::raw_fd_ostream &OS;
  std::shared_ptr<clang::ASTUnit> AST;
  TranslationUnitDecl *TypeDeclarations;
  TranslationUnitDecl *FunctionDeclarations;
  TranslationUnitDecl *GlobalDefinitions;
  TranslationUnitDecl *TypeDefinitions;
  TranslationUnitDecl *FunctionDefinitions;

public:
  ASTManager(llvm::raw_fd_ostream &OS, std::shared_ptr<clang::ASTUnit> AST)
      : OS(OS), AST(AST) {
    TypeDeclarations = TranslationUnitDecl::Create(getASTContext());
    FunctionDeclarations = TranslationUnitDecl::Create(getASTContext());
    GlobalDefinitions = TranslationUnitDecl::Create(getASTContext());
    TypeDefinitions = TranslationUnitDecl::Create(getASTContext());
    FunctionDefinitions = TranslationUnitDecl::Create(getASTContext());
  }

  clang::ASTContext &getASTContext() { return AST->getASTContext(); }

  TranslationUnitDecl *getTypeDeclarations() { return TypeDeclarations; }
  TranslationUnitDecl *getFunctionDeclarations() {
    return FunctionDeclarations;
  }
  TranslationUnitDecl *getGlobalDefinitions() { return GlobalDefinitions; }
  TranslationUnitDecl *getTypeDefinitions() { return TypeDefinitions; }
  TranslationUnitDecl *getFunctionDefinitions() { return FunctionDefinitions; }

  void addTypeDeclaration(clang::Decl *D) { TypeDeclarations->addDecl(D); }
  void addFunctionDeclaration(clang::Decl *D) {
    FunctionDeclarations->addDecl(D);
  }
  void addGlobalDefinition(clang::Decl *D) { GlobalDefinitions->addDecl(D); }
  void addTypeDefinition(clang::Decl *D) { TypeDefinitions->addDecl(D); }
  void addFunctionDefinition(clang::Decl *D) {
    FunctionDefinitions->addDecl(D);
  }

  clang::FunctionDecl* getFuncDeclaration(const char* Name);

  void print(DeclPrinter &Printer);
};

} // namespace notdec::llvm2c

#endif
