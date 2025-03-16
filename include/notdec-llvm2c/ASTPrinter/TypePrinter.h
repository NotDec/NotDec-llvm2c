#ifndef _NOTDEC_TYPEPRINTER_H_
#define _NOTDEC_TYPEPRINTER_H_

#include <clang/AST/DeclarationName.h>
#include <clang/AST/PrettyPrinter.h>
#include <clang/AST/Type.h>
namespace notdec::llvm2c {
using namespace clang;

/// RAII object that enables printing of the ARC __strong lifetime
/// qualifier.
class IncludeStrongLifetimeRAII {
  PrintingPolicy &Policy;
  bool Old;

public:
  explicit IncludeStrongLifetimeRAII(PrintingPolicy &Policy)
      : Policy(Policy), Old(Policy.SuppressStrongLifetime) {
    if (!Policy.SuppressLifetimeQualifiers)
      Policy.SuppressStrongLifetime = false;
  }

  ~IncludeStrongLifetimeRAII() { Policy.SuppressStrongLifetime = Old; }
};

class ParamPolicyRAII {
  PrintingPolicy &Policy;
  bool Old;

public:
  explicit ParamPolicyRAII(PrintingPolicy &Policy)
      : Policy(Policy), Old(Policy.SuppressSpecifiers) {
    Policy.SuppressSpecifiers = false;
  }

  ~ParamPolicyRAII() { Policy.SuppressSpecifiers = Old; }
};

class ElaboratedTypePolicyRAII {
  PrintingPolicy &Policy;
  bool SuppressTagKeyword;
  bool SuppressScope;

public:
  explicit ElaboratedTypePolicyRAII(PrintingPolicy &Policy) : Policy(Policy) {
    SuppressTagKeyword = Policy.SuppressTagKeyword;
    SuppressScope = Policy.SuppressScope;
    Policy.SuppressTagKeyword = true;
    Policy.SuppressScope = true;
  }

  ~ElaboratedTypePolicyRAII() {
    Policy.SuppressTagKeyword = SuppressTagKeyword;
    Policy.SuppressScope = SuppressScope;
  }
};

class TypePrinter {
  PrintingPolicy Policy;
  unsigned Indentation;
  bool HasEmptyPlaceHolder = false;
  bool InsideCCAttribute = false;

public:
  explicit TypePrinter(const PrintingPolicy &Policy, unsigned Indentation = 0)
      : Policy(Policy), Indentation(Indentation) {}

  void print(const Type *ty, Qualifiers qs, raw_ostream &OS,
             StringRef PlaceHolder);
  void print(QualType T, raw_ostream &OS, StringRef PlaceHolder);

  static bool canPrefixQualifiers(const Type *T, bool &NeedARCStrongQualifier);
  void spaceBeforePlaceHolder(raw_ostream &OS);
  void printTypeSpec(NamedDecl *D, raw_ostream &OS);
  void printTemplateId(const TemplateSpecializationType *T, raw_ostream &OS,
                       bool FullyQualify);

  void printBefore(QualType T, raw_ostream &OS);
  void printAfter(QualType T, raw_ostream &OS);
  void AppendScope(DeclContext *DC, raw_ostream &OS,
                   DeclarationName NameInScope);
  void printTag(TagDecl *T, raw_ostream &OS);
  void printFunctionAfter(const FunctionType::ExtInfo &Info, raw_ostream &OS);
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT)                                                    \
  void print##CLASS##Before(const CLASS##Type *T, raw_ostream &OS);            \
  void print##CLASS##After(const CLASS##Type *T, raw_ostream &OS);
#include "clang/AST/TypeNodes.inc"

private:
  void printBefore(const Type *ty, Qualifiers qs, raw_ostream &OS);
  void printAfter(const Type *ty, Qualifiers qs, raw_ostream &OS);
};

} // namespace notdec::llvm2c

#endif
