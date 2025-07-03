

#ifndef _NOTDEC_STRUCTMANAGER_H_
#define _NOTDEC_STRUCTMANAGER_H_

#include <algorithm>
#include <clang/AST/Decl.h>
#include <cstddef>
#include <memory>
#include <optional>
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
  OffsetTy Start = 0;
  OffsetTy Size = 0;

  bool containsOffset(OffsetTy Offset) const {
    if (Start == Offset) {
      return true;
    }
    return Start <= Offset && Offset < Start + Size;
  }
  OffsetTy end() const { return Start + Size; }
  SimpleRange intersect(const SimpleRange &R) const {
    auto Start1 = std::max(this->Start, R.Start);
    auto End1 = std::min(R.end(), end());
    return {.Start = Start1, .Size = std::max(0l, End1 - Start1)};
  }
};

struct FieldEntry {
  SimpleRange R;
  const retypd::CGEdge *Edge = nullptr;
  // clang::DeclaratorDecl *Decl = nullptr;
};

struct BytesManager {
  std::vector<std::pair<SimpleRange, std::string>> Bytes;
  static std::shared_ptr<BytesManager> create(llvm::Module &M);
  static std::shared_ptr<BytesManager> fromOneString(std::string Data) {
    auto BM = std::make_shared<BytesManager>();
    BM->Bytes.push_back(
        {SimpleRange{.Start = 0, .Size = static_cast<OffsetTy>(Data.size())},
         Data});
    return BM;
  }
  std::string decodeCStr(int64_t Offset);
  std::optional<std::string> getRange(SimpleRange R);
  BytesManager getSubBytes(int64_t Start, int64_t End);
};

struct UnionInfo {
  std::vector<const retypd::CGEdge *> Members;
};

struct StructInfo {
  std::vector<FieldEntry> Fields;
};

struct SimpleTypeInfo {
  const retypd::CGEdge *Edge = nullptr;
};

struct ArrayInfo {
  const retypd::CGEdge *Edge = nullptr;
  // force the element size
  std::optional<OffsetTy> ElemSize = std::nullopt;
};

struct TypeInfo {
  std::optional<OffsetTy> Size = std::nullopt;
  std::variant<SimpleTypeInfo, StructInfo, UnionInfo, ArrayInfo> Info;

  bool isSimple() const { return std::holds_alternative<SimpleTypeInfo>(Info); }
  bool isStruct() const { return std::holds_alternative<StructInfo>(Info); }
  bool isUnion() const { return std::holds_alternative<UnionInfo>(Info); }
  bool isArray() const { return std::holds_alternative<ArrayInfo>(Info); }
  const char *getTypeStr() const;

  template <typename T> T *getAs() { return std::get_if<T>(&Info); }
  template <typename T> const T *getAs() const { return std::get_if<T>(&Info); }

  void fixEdge(
      const std::map<const retypd::CGEdge *, const retypd::CGEdge *> &EdgeMap);
};

} // namespace notdec

#endif
