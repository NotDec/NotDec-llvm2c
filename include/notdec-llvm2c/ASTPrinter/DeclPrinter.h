
#ifndef _NOTDEC_DECLPRINTER_H_
#define _NOTDEC_DECLPRINTER_H_

#include "notdec-llvm2c/Interface/StructManager.h"
#include <clang/AST/DeclVisitor.h>
#include <llvm/Support/raw_ostream.h>
#include <optional>

namespace notdec::llvm2c {

using namespace clang;
class ClangTypeResult;

struct MyPrintingPolicy {
  bool AllowGroupedDecl = true; // TODO
  bool MSVCIntSuffix = false;
};

class DeclPrinter : public DeclVisitor<DeclPrinter> {
  raw_ostream &Out;
  const ASTContext &Context;
  const clang::PrintingPolicy &Policy;
  unsigned Indentation;
  MyPrintingPolicy MyPolicy;
  std::shared_ptr<ClangTypeResult> CT;

  raw_ostream &Indent() { return Indent(Indentation); }
  raw_ostream &Indent(unsigned Indentation);
  void ProcessDeclGroup(SmallVectorImpl<Decl *> &Decls);

public:
  DeclPrinter(raw_ostream &Out, const PrintingPolicy &Policy,
              const ASTContext &Context, unsigned Indentation = 2,
              MyPrintingPolicy P2 = MyPrintingPolicy(),
              std::shared_ptr<ClangTypeResult> CT = nullptr)
      : Out(Out), Context(Context), Policy(Policy), Indentation(Indentation),
        MyPolicy(P2), CT(CT) {}

  void printDeclComments(Decl *D);
  void VisitDeclContext(DeclContext *DC, bool Indent = true);

  void VisitTranslationUnitDecl(TranslationUnitDecl *D);
  void VisitTypedefDecl(TypedefDecl *D);
  // void VisitTypeAliasDecl(TypeAliasDecl *D);
  void VisitEnumDecl(EnumDecl *D);
  void VisitRecordDecl(RecordDecl *D);
  void VisitEnumConstantDecl(EnumConstantDecl *D);
  void VisitEmptyDecl(EmptyDecl *D);
  void VisitFunctionDecl(FunctionDecl *D);
  // void VisitFriendDecl(FriendDecl *D);
  void VisitFieldDecl(FieldDecl *D);
  void VisitVarDecl(VarDecl *D);
  void VisitLabelDecl(LabelDecl *D);
  void VisitParmVarDecl(ParmVarDecl *D);

  void prettyPrintAttributes(Decl *D);
  void prettyPrintPragmas(Decl *D);
  void printDeclType(QualType T, StringRef DeclName, bool Pack = false);
};

} // namespace notdec::llvm2c

#endif
