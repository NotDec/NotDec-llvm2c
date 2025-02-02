#ifndef _NOTDEC_STMTPRINTER_H_
#define _NOTDEC_STMTPRINTER_H_

#include <clang/AST/StmtVisitor.h>

#include "ASTPrinter/DeclPrinter.h"

namespace notdec::llvm2c {
using namespace clang;

class StmtPrinter : public clang::StmtVisitor<StmtPrinter> {
  raw_ostream &OS;
  unsigned IndentLevel;
  PrinterHelper *Helper;
  PrintingPolicy Policy;
  std::string NL;
  const ASTContext *Context;
  MyPrintingPolicy MyPolicy;

public:
  StmtPrinter(raw_ostream &os, PrinterHelper *helper,
              const PrintingPolicy &Policy, unsigned Indentation = 0,
              StringRef NL = "\n", const ASTContext *Context = nullptr, MyPrintingPolicy P2 = MyPrintingPolicy())
      : OS(os), IndentLevel(Indentation), Helper(helper), Policy(Policy),
        NL(NL), Context(Context), MyPolicy(P2) {}

  void PrintStmt(Stmt *S) { PrintStmt(S, Policy.Indentation); }

  void PrintStmt(Stmt *S, int SubIndent) {
    IndentLevel += SubIndent;
    if (S && isa<Expr>(S)) {
      // If this is an expr used in a stmt context, indent and newline it.
      Indent();
      Visit(S);
      OS << ";" << NL;
    } else if (S) {
      Visit(S);
    } else {
      Indent() << "<<<NULL STATEMENT>>>" << NL;
    }
    IndentLevel -= SubIndent;
  }

  void PrintInitStmt(Stmt *S, unsigned PrefixWidth) {
    // FIXME: Cope better with odd prefix widths.
    IndentLevel += (PrefixWidth + 1) / 2;
    if (auto *DS = dyn_cast<DeclStmt>(S))
      PrintRawDeclStmt(DS);
    else
      PrintExpr(cast<Expr>(S));
    OS << "; ";
    IndentLevel -= (PrefixWidth + 1) / 2;
  }

  void PrintControlledStmt(Stmt *S) {
    if (auto *CS = dyn_cast<CompoundStmt>(S)) {
      OS << " ";
      PrintRawCompoundStmt(CS);
      OS << NL;
    } else {
      OS << NL;
      PrintStmt(S);
    }
  }

  void PrintRawCompoundStmt(CompoundStmt *S);
  void PrintRawDecl(Decl *D);
  void PrintRawDeclStmt(const DeclStmt *S);
  void PrintRawIfStmt(IfStmt *If);
  void PrintRawCXXCatchStmt(CXXCatchStmt *Catch);
  void PrintCallArgs(CallExpr *E);
  void PrintRawSEHExceptHandler(SEHExceptStmt *S);
  void PrintRawSEHFinallyStmt(SEHFinallyStmt *S);
  void PrintOMPExecutableDirective(OMPExecutableDirective *S,
                                   bool ForceNoStmt = false);

  void PrintExpr(Expr *E) {
    if (E)
      Visit(E);
    else
      OS << "<null expr>";
  }

  raw_ostream &Indent(int Delta = 0) {
    for (int i = 0, e = IndentLevel + Delta; i < e; ++i)
      OS << "  ";
    return OS;
  }

  void Visit(Stmt *S) {
    if (Helper && Helper->handledStmt(S, OS))
      return;
    else
      StmtVisitor<StmtPrinter>::Visit(S);
  }

  void VisitStmt(Stmt *Node) LLVM_ATTRIBUTE_UNUSED {
    Indent() << "<<unknown stmt type>>" << NL;
  }

  void VisitExpr(Expr *Node) LLVM_ATTRIBUTE_UNUSED {
    OS << "<<unknown expr type>>";
  }

  void VisitCXXNamedCastExpr(CXXNamedCastExpr *Node);

#define ABSTRACT_STMT(CLASS)
#define STMT(CLASS, PARENT) void Visit##CLASS(CLASS *Node);
#include "clang/AST/StmtNodes.inc"
};

} // namespace notdec::llvm2c

#endif
