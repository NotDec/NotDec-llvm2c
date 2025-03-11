

#ifndef _NOTDEC_STRUCTMANAGER_H_
#define _NOTDEC_STRUCTMANAGER_H_

#include <clang/AST/Decl.h>
#include <cstddef>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Module.h>

#include "notdec-llvm2c/Interface/Range.h"

namespace notdec::retypd {
struct CGNode;
struct CGEdge;
struct ConstraintGraph;
struct ConstraintSummary;
struct TRContext;
} // namespace notdec::retypd

namespace notdec {

using IndexTy = OffsetRange;
using OffsetTy = decltype(IndexTy::offset);

struct SimpleRange {
  OffsetTy Start;
  OffsetTy Size;

  bool containsOffset(OffsetTy Offset) const {
    if (Start == Offset) {
      return true;
    }
    return Start <= Offset && Offset < Start + Size;
  }
};

struct FieldEntry {
  SimpleRange R;
  const retypd::CGEdge *Edge = nullptr;
  retypd::CGNode *Target = nullptr;
  // clang::DeclaratorDecl *Decl = nullptr;
};

struct BytesManager {
  std::vector<std::pair<SimpleRange, llvm::StringRef>> Bytes;
  static std::shared_ptr<BytesManager> create(llvm::Module &M);
  llvm::StringRef decodeCStr(int64_t Offset);
};

struct UnionInfo {
  SimpleRange R;
  std::vector<const retypd::CGEdge *> Members;
};

struct StructInfo {
  std::vector<FieldEntry> Fields;
};

struct SimpleTypeInfo {
  const retypd::CGEdge *Edge;
};

struct ArrayInfo {
  const retypd::CGEdge *Edge;
};

struct TypeInfo {
  OffsetTy Size = -1;
  std::variant<SimpleTypeInfo, StructInfo, UnionInfo, ArrayInfo> Info;
};

} // namespace notdec

#endif
