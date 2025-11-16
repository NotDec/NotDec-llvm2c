#include "notdec-llvm2c/ASTManager.h"
#include "ASTPrinter/DeclPrinter.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include <llvm/Support/Casting.h>
#include <string>

namespace notdec::llvm2c {

class TypeRefCollector : public RecursiveASTVisitor<TypeRefCollector> {
public:
  explicit TypeRefCollector() {}

  // 对外接口：传入 TranslationUnitDecl*
  void collect(TranslationUnitDecl *TU) { TraverseDecl(TU); }

  // 最终结果：所有被引用过的 struct/union/typedef 名字
  const std::set<std::string> &getReferencedNames() const { return Referenced; }
  size_t size() { return Referenced.size(); }
  auto count(const std::string &Str) { return Referenced.count(Str); }

  // —— 一类：所有带 QualType 的节点 —— //

  bool VisitValueDecl(ValueDecl *VD) {
    processType(VD->getType());
    return true;
  }

  bool VisitCastExpr(CastExpr *CE) {
    processType(CE->getType());
    return true;
  }

  bool VisitCompoundLiteralExpr(CompoundLiteralExpr *CLE) {
    processType(CLE->getType());
    return true;
  }

  bool VisitOffsetOfExpr(OffsetOfExpr *OOE) {
    // offsetof(struct S, field) 里，getTypeSourceInfo() 一定 NON-NULL
    if (auto *TSI = OOE->getTypeSourceInfo())
      processType(TSI->getType());
    return true;
  }

  // —— 另一类：定义新类型的节点 —— //

  bool VisitTypedefNameDecl(TypedefNameDecl *TD) {
    // typedef Foo Bar;
    processType(TD->getUnderlyingType());
    // 自身也是一个被引用（别名）！
    Referenced.insert(TD->getNameAsString());
    return true;
  }

  bool VisitRecordDecl(RecordDecl *RD) {
    // struct/union 定义里，成员也可能引用新类型
    for (auto *FD : RD->fields())
      processType(FD->getType());
    return true;
  }

private:
  // ASTContext &Context;
  std::set<std::string> Referenced;

  // 把各种复合类型都拆开
  void processType(QualType QT) {
    if (QT.isNull())
      return;

    if (auto *TT = QT->getAs<TypedefType>()) {
      auto *TD = TT->getDecl();
      auto Ent = Referenced.insert(TD->getNameAsString());
      if (Ent.second) {
        processType(TD->getUnderlyingType());
      }
      return;
    } else if (auto *FT = QT->getAs<FunctionType>()) {
      // 返回值
      processType(FT->getReturnType());
      // 如果是带参数原型的，就一并处理每个参数
      if (auto *FPT = dyn_cast<FunctionProtoType>(FT)) {
        for (auto ParamTy : FPT->param_types())
          processType(ParamTy);
      }
      return;
    } else if (auto *PT = QT->getAs<PointerType>()) {
      processType(PT->getPointeeType());
    } else if (auto *RT = QT->getAs<ReferenceType>()) {
      processType(RT->getPointeeType());
    } else if (auto *AT = QT->getAsArrayTypeUnsafe()) {
      processType(AT->getElementType());
    } else if (auto *RT = QT->getAs<RecordType>()) {
      auto *RD = RT->getDecl();
      auto Ent = Referenced.insert(RD->getNameAsString());
      if (Ent.second) {
        VisitRecordDecl(RD);
      }
    } else if (QT->isBuiltinType() || QT->isVoidType()) {
      // do nothing
    } else {
      assert(false && "Unhandled Type!");
    }
  }
};

void ASTManager::print(DeclPrinter &Printer) {
  // auto &Ctx = FunctionDefinitions->getASTContext();
  // collect all usages
  TypeRefCollector Collector;

  Collector.collect(GlobalDefinitions);
  Collector.collect(FunctionDefinitions);
  Collector.collect(FunctionDeclarations);

  // // 处理类型内引用其他类型的情况，递归访问类型
  // auto Size = Collector.size();
  // std::set<std::string> Visited;
  // do {
  //   Size = Collector.size();
  //   for (auto &Str : Collector.getReferencedNames()) {
  //     if (Visited.count(Str)) {
  //       continue;
  //     }
  //     Visited.insert(Str);
  //     if (auto Decl = getNamedDecl(Str.c_str())) {
  //       Collector.VisitDecl(Decl);
  //     }
  //   }
  // } while (Collector.size() > Size);

  // 根据引用情况打印

  OS << "#include <stdbool.h>\n\n";

  OS << "// ====== Type Declarations ======\n";
  // Printer.VisitTranslationUnitDecl(TypeDeclarations);
  Printer.VisitDeclContext(TypeDeclarations, false, [&](Decl *D) -> bool {
    if (auto ND = llvm::dyn_cast<NamedDecl>(D)) {
      auto N = ND->getNameAsString();
      auto C = Collector.count(N);
      return C != 0;
    }
    return false;
  });
  OS << "// ====== End of Type Declarations ======\n\n";

  OS << "// ====== Function Declarations ======\n";
  Printer.VisitTranslationUnitDecl(FunctionDeclarations);
  OS << "// ====== End of Function Declarations ======\n\n";

  OS << "// ====== Type Definitions ======\n";
  // Printer.VisitTranslationUnitDecl(TypeDefinitions);
  Printer.VisitDeclContext(TypeDefinitions, false, [&](Decl *D) -> bool {
    if (auto ND = llvm::dyn_cast<NamedDecl>(D)) {
      auto N = ND->getNameAsString();
      auto C = Collector.count(N);
      return C != 0;
    }
    return false;
  });
  OS << "// ====== End of Type Definitions ======\n\n";

  OS << "// ====== Global Declarations ======\n";
  Printer.VisitTranslationUnitDecl(GlobalDeclarations);
  OS << "// ====== End of Global Declarations ======\n\n";

  OS << "// ====== Global Definitions ======\n";
  Printer.VisitTranslationUnitDecl(GlobalDefinitions);
  OS << "// ====== End of Global Definitions ======\n\n";

  OS << "// ====== Function Definitions ======\n";
  Printer.VisitTranslationUnitDecl(FunctionDefinitions);
  OS << "// ====== End of Function Definitions ======\n\n";
}

clang::NamedDecl *ASTManager::getNamedDecl(const char *Name) {
  for (auto *Decl : TypeDefinitions->decls()) {
    if (auto ND = llvm::dyn_cast<NamedDecl>(Decl)) {
      if (ND->getDeclName().getAsString() == Name) {
        return ND;
      }
    }
  }
  return nullptr;
}

clang::FunctionDecl *ASTManager::getFuncDeclaration(const char *Name) {
  for (auto *Decl : FunctionDeclarations->decls()) {
    if (auto *FuncDecl = Decl->getAsFunction()) {
      if (FuncDecl->getName().equals(Name)) {
        return FuncDecl;
      }
    }
  }
  return nullptr;
}
} // namespace notdec::llvm2c
