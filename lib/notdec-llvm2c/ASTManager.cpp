#include "notdec-llvm2c/ASTManager.h"
#include "ASTPrinter/DeclPrinter.h"

namespace notdec::llvm2c {

void ASTManager::print(DeclPrinter &Printer) {
  OS << "// ====== Type Declarations ======\n";
  Printer.VisitTranslationUnitDecl(TypeDeclarations);
  OS << "// ====== End of Type Declarations ======\n\n";

  OS << "// ====== Function Declarations ======\n";
  Printer.VisitTranslationUnitDecl(FunctionDeclarations);
  OS << "// ====== End of Function Declarations ======\n\n";

  OS << "// ====== Type Definitions ======\n";
  Printer.VisitTranslationUnitDecl(TypeDefinitions);
  OS << "// ====== End of Type Definitions ======\n\n";

  OS << "// ====== Global Definitions ======\n";
  Printer.VisitTranslationUnitDecl(GlobalDefinitions);
  OS << "// ====== End of Global Definitions ======\n\n";

  OS << "// ====== Function Definitions ======\n";
  Printer.VisitTranslationUnitDecl(FunctionDefinitions);
  OS << "// ====== End of Function Definitions ======\n\n";
}

clang::FunctionDecl* ASTManager::getFuncDeclaration(const char* Name) {
  for (auto* Decl : FunctionDeclarations->decls()) {
    if (auto *FuncDecl = Decl->getAsFunction()) {
      if (FuncDecl->getName().equals(Name)) {
        return FuncDecl;
      }
    }
  }
  return nullptr;
}
} // namespace notdec::llvm2c
