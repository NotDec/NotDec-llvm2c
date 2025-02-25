#include "notdec-llvm2c/StructManager.h"

#include <cassert>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <cstddef>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/JSON.h>
#include <memory>
#include <string>
#include <vector>

namespace notdec {

void StructInfo::updateFieldSize(size_t Index, OffsetTy Size) {
  auto OldSize = Fields[Index].R.Size;
  Fields[Index].R.Size = Size;
  auto *Decl = Fields[Index].Decl;
  if (Decl != nullptr) {
    auto QT = Decl->getType();
    if (QT->isArrayType()) {
      auto OldArrSize = llvm::cast<clang::ConstantArrayType>(QT)->getSize().getZExtValue();
      auto NewArrSize = llvm::APInt(32, Size / (OldSize / OldArrSize));
      auto AT = QT->getAsArrayTypeUnsafe();
      Decl->setType(Decl->getASTContext().getConstantArrayType(
        AT->getElementType(), llvm::APInt(32, Size),
          clang::IntegerLiteral::Create(
              Decl->getASTContext(), llvm::APInt(32, Size),
              Decl->getASTContext().IntTy, clang::SourceLocation()),
          clang::ArrayType::Normal, 0));
    }
  }
}

void StructInfo::resolveInitialValue() {
  for (size_t i = 0; i < Fields.size(); i++) {
    auto &Ent = Fields[i];
    if (Ent.isPadding) {
      continue;
    }
    if (Ent.Decl == nullptr) {
      continue;
    }
    auto QT = Ent.Decl->getType();
    if (QT->isArrayType() &&
        QT->getAsArrayTypeUnsafe()->getElementType()->isCharType()) {
      auto Offset = Ent.R.Start.offset;
      auto CStr = Bytes->decodeCStr(Offset);
      if (CStr.size() > 0) {
        llvm::cast<clang::FieldDecl>(Ent.Decl)->setInClassInitializer(
            clang::StringLiteral::Create(Decl->getASTContext(), CStr,
                                         clang::StringLiteral::Ascii, false, QT,
                                         clang::SourceLocation()));
        // shrink the array size if the entry is too large.
        // expand if there is enough space.
        auto NewSize = CStr.size() + 1;
        if (Ent.R.Size > NewSize || (i + 1 == Fields.size()) ||
            (i + 1 < Fields.size() &&
             Fields[i + 1].R.Start.offset - Ent.R.Start.offset > NewSize)) {
          updateFieldSize(i, NewSize);
        }
      }
    }
  }
}

FieldEntry &StructInfo::derefAt(OffsetTy Offset) {
  for (auto &Ent : Fields) {
    if (Ent.R.containsOffset(Offset)) {
      if (Ent.R.Start.offset == Offset) {
        return Ent;
      }
      // return Ent;
    }
  }
  assert(false && "Offset not found");
}

void StructInfo::addPaddings() {
  // for (size_t i = 0; i < Fields.size(); i++) {
  //   auto &Ent = Fields[i];
  //   if (i + 1 < Fields.size()) {
  //     auto &Next = Fields[i + 1];
  //     auto NextOffset = Next.R.Start.offset;
  //     auto CurEnd = Ent.R.Start.offset + Ent.R.Size;
  //     bool isArray = Ent.Decl == nullptr ? false :
  //     Ent.Decl->getType()->isArrayType(); if (CurEnd < NextOffset) {
  //       if (Ent.R.Start.access.size() > 0 || isArray) {
  //         // expand the array size
  //         Ent.R.Size = NextOffset - Ent.R.Start.offset;
  //       } else {
  //         // add padding
  //         Fields.insert(
  //             Fields.begin() + i + 1,
  //             FieldEntry{.R = Range{.Start = IndexTy{.offset = CurEnd},
  //                                   .Size = NextOffset - CurEnd},
  //                        .isPadding = true});
  //       }
  //     }
  //   }
  // }
}

std::shared_ptr<BytesManager> BytesManager::create(llvm::Module &M) {
  std::shared_ptr<BytesManager> BM = std::make_shared<BytesManager>();
  for (auto &G : M.getGlobalList()) {
    auto Name = G.getName();
    if (!Name.consume_front("__notdec_mem0_0x")) {
      continue;
    }
    auto Offset = std::stol(Name.str(), nullptr, 16);
    assert(Offset >= 0);
    if (G.hasInitializer()) {
      auto *Init = G.getInitializer();
      if (auto *C = llvm::dyn_cast<llvm::ConstantDataArray>(Init)) {
        auto Data = C->getRawDataValues();
        BM->Bytes.push_back({Range{.Start = IndexTy{.offset = Offset},
                                   .Size = static_cast<OffsetTy>(Data.size())},
                             Data});
      }
    }
  }
  return BM;
}

llvm::StringRef decodeCStr1(llvm::StringRef Bytes, int64_t Offset) {
  auto Start = Bytes.data() + Offset;
  auto End = Bytes.data() + Bytes.size();
  auto *Ptr = Start;
  while (Ptr < End && *Ptr != '\0') {
    Ptr++;
  }
  return llvm::StringRef(Start, Ptr - Start);
}

llvm::StringRef BytesManager::decodeCStr(int64_t Offset) {
  for (auto &Ent : Bytes) {
    if (Ent.first.containsOffset(Offset)) {
      auto innerOffset = Offset - Ent.first.Start.offset;
      return decodeCStr1(Ent.second, innerOffset);
    }
  }
  return "";
}

/*
  "printf": {
    "constraints": [
      "printf.in_0 <= cstr",
      "cstr.load8 <= #char",
      "printf.out <= #sint"
    ],
    "pni_map": {
      "printf": "func p #1",
      "printf.in_0": "ptr p #2",
      "cstr": "ptr p #2",
      "cstr.load8": "int 8 #3",
      "#char": "int 8 #3",
      "printf.out": "int 32 #4",
      "#sint": "int 32 #4",
      "#double": "float 64 #5"
    }
  },
*/

} // namespace notdec
