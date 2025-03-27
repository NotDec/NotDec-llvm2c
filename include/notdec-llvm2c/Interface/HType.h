#ifndef _NOTDEC_INTERFACE_HTCONTEXT_H_
#define _NOTDEC_INTERFACE_HTCONTEXT_H_

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include "Range.h"
#include "notdec-llvm2c/Interface/StructManager.h"

namespace notdec::ast {

class HTypeContext;
class TypedefType;
class RecordType;
class HType;

/*
TypedDecl Architecture:
  - TypedDecl
    - RecordDecl
    - UnionDecl
    - TypeDefDecl
*/
class TypedDecl {
public:
  enum TypedDeclKind { DK_Record, DK_Union, DK_Typedef };

protected:
  std::unique_ptr<HType> TypeForDecl;
  std::unique_ptr<HType> TypeForDeclConst;
  TypedDecl(TypedDeclKind K, std::string Name) : Kind(K), Name(Name) {}

public:
  TypedDeclKind getKind() const { return Kind; }
  HType *getTypeForDecl() const { return TypeForDecl.get(); }
  HType *getTypeForDeclConst() const { return TypeForDeclConst.get(); }
  const std::string &getName() const { return Name; }
  const std::string &getComment() const { return Comment; }
  void setComment(const std::string &Comment) { this->Comment = Comment; }
  clang::Decl *getASTDecl() const { return ASTDecl; }
  void setASTDecl(clang::Decl *D) {
    assert(D != nullptr && "setASTDecl: nullptr?");
    ASTDecl = D;
  }

  template <typename T> T *getAs() const {
    if (auto *T1 = llvm::dyn_cast<T>(this)) {
      return const_cast<T *>(T1);
    }
    return nullptr;
  }
  void print(llvm::raw_fd_ostream &OS) const;

private:
  TypedDeclKind Kind;
  std::string Name;
  std::string Comment;
  clang::Decl *ASTDecl = nullptr;
};

class FieldDecl {
public:
  SimpleRange R;
  HType *Type;
  std::string Name;
  std::string Comment;
  bool isPadding = false;
  clang::Decl *ASTDecl = nullptr;

  void setASTDecl(clang::Decl *D) const {
    assert(D != nullptr && "setASTDecl: nullptr?");
    const_cast<FieldDecl *>(this)->ASTDecl = D;
  }
};

class RecordDecl : public TypedDecl {
protected:
  // TODO With range info generated previously.
  std::vector<FieldDecl> Fields;
  std::shared_ptr<BytesManager> Bytes;

  RecordDecl(std::string Name) : TypedDecl(DK_Record, Name) {}
  friend class HTypeContext;

public:
  std::vector<FieldDecl> &getFields() { return Fields; }
  const std::vector<FieldDecl> &getFields() const { return Fields; }
  const ast::FieldDecl *getFieldAt(OffsetTy Off) const;
  void print(llvm::raw_fd_ostream &OS) const;
  void setBytesManager(std::shared_ptr<BytesManager> Bytes) {
    assert(this->Bytes == nullptr && "BytesManager already exists");
    this->Bytes = Bytes;
  }
  std::shared_ptr<BytesManager> getBytesManager() { return Bytes; }

  SimpleRange getRange() const {
    assert(!Fields.empty() && "getRange: Empty fields?");
    int64_t Max = std::numeric_limits<decltype(Max)>::min();
    int64_t Min = std::numeric_limits<decltype(Min)>::max();
    for (auto &Ent : Fields) {
      Max = std::max(Max, Ent.R.Start + Ent.R.Size);
      Min = std::min(Min, Ent.R.Start);
    }
    return SimpleRange{.Start = Min, .Size = Max - Min};
  }

  static bool classof(const TypedDecl *T) { return T->getKind() == DK_Record; }
  // void addField(FieldDecl Field) { Fields.push_back(Field); }
  void addField(FieldDecl Ent) {
    if (Ent.R.Size == 0) {
      assert(false && "TODO support zero sized field?");
    }
    if (Fields.empty()) {
      Fields.push_back(Ent);
      return;
    }
    auto EntStart = Ent.R.Start;
    auto EntEnd = EntStart + Ent.R.Size;
    // check insert begin
    if (EntEnd <= Fields.at(0).R.Start) {
      Fields.insert(Fields.begin(), Ent);
      return;
    }
    for (size_t i = 0; i < Fields.size(); i++) {
      auto &F = Fields[i];
      auto CurStart = F.R.Start;
      auto CurEnd = CurStart + F.R.Size;
      auto NextStart = std::numeric_limits<decltype(CurEnd)>::max();
      if (i + 1 < Fields.size()) {
        NextStart = Fields[i + 1].R.Start;
      }
      if (CurEnd <= EntStart && EntEnd <= NextStart) {
        Fields.insert(Fields.begin() + i + 1, Ent);
        return;
      }
      // check for overlap
      if (CurStart <= EntStart && EntStart < CurEnd) {
        assert(false && "Field cannot overlap!");
      }
    }
    assert(false && "Field not inserted?");
  }
  std::optional<size_t> getLastNonPaddingInd() {
    std::optional<size_t> Ret = std::nullopt;
    for (size_t I = Fields.size(); I > 0; I--) {
      auto Ind = I - 1;
      if (!Fields.at(Ind).isPadding) {
        Ret = Ind;
        break;
      }
    }
    return Ret;
  }

  static RecordDecl *Create(HTypeContext &Ctx, const std::string &Name);
};

class UnionDecl : public TypedDecl {
protected:
  std::vector<FieldDecl> Members;

  UnionDecl(std::string Name) : TypedDecl(DK_Union, Name) {}
  friend class HTypeContext;

public:
  static bool classof(const TypedDecl *T) { return T->getKind() == DK_Union; }
  const std::vector<FieldDecl> &getMembers() const { return Members; }
  void addMember(FieldDecl Field) { Members.push_back(Field); }
  void print(llvm::raw_fd_ostream &OS) const;

  static UnionDecl *Create(HTypeContext &Ctx, const std::string &Name);

  SimpleRange getRange() const {
    assert(!Members.empty() && "getRange: Empty Members?");
    int64_t Max = std::numeric_limits<decltype(Max)>::min();
    int64_t Min = std::numeric_limits<decltype(Min)>::max();
    for (auto &Ent : Members) {
      Max = std::max(Max, Ent.R.Start + Ent.R.Size);
      Min = std::min(Min, Ent.R.Start);
    }
    return SimpleRange{.Start = Min, .Size = Max - Min};
  }
};

class TypedefDecl : public TypedDecl {
protected:
  HType *Type;

  friend class HTypeContext;

public:
  TypedefDecl(std::string Name, HType *Type)
      : TypedDecl(DK_Typedef, Name), Type(Type) {}

  static bool classof(const TypedDecl *T) { return T->getKind() == DK_Typedef; }
  HType *getType() const { return Type; }
  void print(llvm::raw_fd_ostream &OS) const;

  static TypedefDecl *Create(HTypeContext &Ctx, const std::string &Name,
                             HType *Type);
};

SimpleRange getRange(const TypedDecl *Decl);

/*
Type Architecture:
  - SimpleType
    - IntegerType
    - FloatingType
    - PointerType
  - FunctionType
  - RecordType
  - UnionType
  - TypedefType
  - ArrayType
*/
class HType {
public:
  enum HTypeKind {
    // Begin of SimpleType
    TK_Simple,
    TK_Integer,
    TK_Float,
    TK_Pointer,
    // End of SimpleType
    TK_Function,

    // Begin of CompoundType
    TK_Record,
    TK_Union,
    TK_Array,
    // End of CompoundType
    TK_Typedef
  };

protected:
  HType(HTypeKind K, HType *Canon, bool IsConst)
      : Kind(K), CanonicalType(Canon == nullptr ? this : Canon),
        IsConst(IsConst) {}

public:
  HTypeKind getKind() const { return Kind; }
  bool isConst() const { return IsConst; }
  HType *getCanonicalType() const { return CanonicalType; }

  bool isPointerType() const { return Kind == TK_Pointer; }
  bool isRecordType() const { return Kind == TK_Record; }
  bool isUnionType() const { return Kind == TK_Union; }
  bool isArrayType() const { return Kind == TK_Array; }
  bool isFunctionType() const { return Kind == TK_Function; }
  bool isTypedefType() const { return Kind == TK_Typedef; }
  bool isCharType() const;
  bool isCharArrayType() const;

  HType *getPointeeType() const;
  HType *getArrayElementType() const;
  TypedDecl *getAsRecordOrUnionDecl() const;
  RecordDecl *getAsRecordDecl() const;
  UnionDecl *getAsUnionDecl() const;
  TypedefDecl *getAsTypedefDecl() const;
  template <typename T> T *getAs() const {
    if (auto *T1 = llvm::dyn_cast<T>(this)) {
      return const_cast<T *>(T1);
    }
    return nullptr;
  }

  std::string getAsString() const;

private:
  HTypeKind Kind;
  // points to the type without qualifier and typedef
  HType *CanonicalType = nullptr;
  bool IsConst;
};

class SimpleType : public HType {
protected:
  SimpleType(HTypeKind K, HType *Canon, bool IsConst, unsigned BitSize)
      : HType(K, Canon, IsConst), BitSize(BitSize) {}

public:
  static bool classof(const HType *T) {
    return T->getKind() >= TK_Simple && T->getKind() <= TK_Pointer;
  }
  unsigned getBitSize() const { return BitSize; }

private:
  unsigned BitSize;
};

// 整数类型
class IntegerType : public SimpleType {
protected:
  bool IsUnsigned = false;

  IntegerType(bool IsConst, HType *Canon, unsigned BitSize, bool IsUnsigned)
      : SimpleType(TK_Integer, Canon, IsConst, BitSize),
        IsUnsigned(IsUnsigned) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Integer; }

  bool isSigned() const { return !IsUnsigned; }
  bool isUnsigned() const { return IsUnsigned; }
};

// 浮点类型
class FloatingType : public SimpleType {
  FloatingType(bool IsConst, HType *Canon, unsigned BitSize)
      : SimpleType(TK_Float, Canon, IsConst, BitSize) {}

  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Float; }
};

// 指针类型
class PointerType : public SimpleType {
  HType *PointeeType;

  PointerType(bool IsConst, HType *Canon, unsigned PointerSize, HType *Pointee)
      : SimpleType(TK_Pointer, Canon, PointerSize, IsConst),
        PointeeType(Pointee) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Pointer; }

  HType *getPointeeType() const { return PointeeType; }
};

// 函数类型
class FunctionType : public HType {
  std::vector<HType *> ReturnType;
  std::vector<HType *> ParamTypes;
  FunctionType(bool IsConst, HType *Canon, const std::vector<HType *> &Return,
               const std::vector<HType *> &Params)
      : HType(TK_Function, Canon, IsConst), ReturnType(Return),
        ParamTypes(Params) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Function; }
  const std::vector<HType *> &getReturnType() const { return ReturnType; }
  const std::vector<HType *> &getParamTypes() const { return ParamTypes; }
};

class RecordType : public HType {
  RecordDecl *Decl;
  RecordType(bool IsConst, HType *Canon, RecordDecl *Decl)
      : HType(TK_Record, Canon, IsConst), Decl(Decl) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Record; }
  RecordDecl *getDecl() const { return Decl; }
};

class UnionType : public HType {
  UnionDecl *Decl;
  UnionType(bool IsConst, HType *Canon, UnionDecl *Decl)
      : HType(TK_Union, Canon, IsConst), Decl(Decl) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Union; }
  UnionDecl *getDecl() const { return Decl; }
};

class ArrayType : public HType {
  HType *ElementType;
  std::optional<unsigned> NumElements;
  ArrayType(bool IsConst, HType *Canon, HType *Element,
            std::optional<unsigned> NumElements)
      : HType(TK_Array, Canon, IsConst), ElementType(Element),
        NumElements(NumElements) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Array; }
  HType *getElementType() const { return ElementType; }
  std::optional<unsigned> getNumElements() const { return NumElements; }

  HType* withSize(HTypeContext& Ctx, std::optional<unsigned> NumElements);
};

class TypedefType : public HType {
  TypedefDecl *Decl;
  TypedefType(bool IsConst, TypedefDecl *Decl)
      : HType(TK_Typedef, Decl->getType()->getCanonicalType(), IsConst),
        Decl(Decl) {}
  friend class HTypeContext;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Typedef; }
  TypedefDecl *getDecl() const { return Decl; }
};

/* Factory class implementation */
class HTypeContext {

  // For struct/typedef decls.
  std::map<std::string, std::unique_ptr<TypedDecl>> Decls;
  // For struct types.
  std::vector<std::unique_ptr<HType>> Types;
  // factory methods
  using IntegerKey = std::tuple<bool, unsigned, bool>;
  std::map<IntegerKey, std::unique_ptr<IntegerType>> IntegerTypes;

  using FloatKey = std::tuple<bool, unsigned>;
  std::map<FloatKey, std::unique_ptr<FloatingType>> FloatTypes;

  using PointerKey = std::tuple<bool, unsigned, HType *>;
  std::map<PointerKey, std::unique_ptr<PointerType>> PointerTypes;

  using ArrayKey = std::tuple<bool, HType *, std::optional<unsigned>>;
  std::map<ArrayKey, std::unique_ptr<ArrayType>> ArrayTypes;

  // using FunctionKey = std::tuple<bool, std::vector<HType*>,
  // std::vector<HType*>>; static std::map<FunctionKey,
  // std::unique_ptr<FunctionType>> functionTypes;

protected:
  friend class RecordDecl;
  friend class UnionDecl;
  friend class TypedefDecl;
  void addDecl(const std::string &Name, std::unique_ptr<TypedDecl> Decl) {
    assert(!Decls.count(Name));
    Decls[Name] = std::move(Decl);
  }

public:
  const std::map<std::string, std::unique_ptr<TypedDecl>> &getDecls() const {
    return Decls;
  }
  TypedDecl *getDecl(const std::string &Name) {
    auto It = Decls.find(Name);
    if (It != Decls.end()) {
      return It->second.get();
    }
    return nullptr;
  }

  // Type factory methods
  HType *getIntegerType(bool IsConst, unsigned BitSize, bool IsUnsigned) {
    auto key = std::make_tuple(IsConst, BitSize, IsUnsigned);
    auto &entry = IntegerTypes[key];
    if (!entry) {
      HType *Canon = nullptr;
      if (IsConst) {
        Canon = getIntegerType(false, BitSize, IsUnsigned);
      }
      entry.reset(new IntegerType(IsConst, Canon, BitSize, IsUnsigned));
    }
    return entry.get();
  }
  HType *getBool() { return getIntegerType(false, 1, true); }
  HType *getChar() { return getIntegerType(false, 8, false); }

  HType *getFloatType(bool IsConst, unsigned BitSize) {
    auto key = std::make_tuple(IsConst, BitSize);
    auto &entry = FloatTypes[key];
    if (!entry) {
      HType *Canon = nullptr;
      if (IsConst) {
        Canon = getFloatType(false, BitSize);
      }
      entry.reset(new FloatingType(IsConst, Canon, BitSize));
    }
    return entry.get();
  }

  HType *getPointerType(bool IsConst, unsigned PtrSize, HType *Pointee) {
    auto key = std::make_tuple(IsConst, PtrSize, Pointee);
    auto &entry = PointerTypes[key];
    if (!entry) {
      HType *Canon = nullptr;
      if (key != std::make_tuple(false, PtrSize,
                                 Pointee == nullptr
                                     ? nullptr
                                     : Pointee->getCanonicalType())) {
        Canon = getPointerType(false, PtrSize, Pointee->getCanonicalType());
      }
      entry.reset(new PointerType(IsConst, Canon, PtrSize, Pointee));
    }
    return entry.get();
  }

  HType *getArrayType(bool IsConst, HType *Element,
                      std::optional<unsigned> NumElements) {
    auto key = std::make_tuple(IsConst, Element, NumElements);
    auto &entry = ArrayTypes[key];
    if (!entry) {
      HType *Canon = nullptr;
      if (key !=
          std::make_tuple(false, Element->getCanonicalType(), NumElements)) {
        Canon = getArrayType(false, Element->getCanonicalType(), NumElements);
      }
      entry.reset(new ArrayType(IsConst, Canon, Element, NumElements));
    }
    return entry.get();
  }

  //  HType* getFunctionType(bool IsConst, const std::vector<HType*>& Ret,
  // const std::vector<HType*>& Params) {
  //   auto key = std::make_tuple(IsConst, Ret, Params);
  //   auto& entry = functionTypes[key];
  //   if (!entry) {
  //     entry.reset(new FunctionType(IsConst, Ret, Params));
  //   }
  //   return entry.get();
  // }

  HType *getRecordType(bool IsConst, RecordDecl *Decl) {
    if (!Decl->TypeForDecl) {
      Decl->TypeForDecl.reset(new RecordType(false, nullptr, Decl));
      Decl->TypeForDeclConst.reset(
          new RecordType(true, Decl->TypeForDecl.get(), Decl));
    }

    if (!IsConst) {
      return Decl->TypeForDecl.get();
    } else {
      return Decl->TypeForDeclConst.get();
    }
  }

  HType *getUnionType(bool IsConst, UnionDecl *Decl) {
    if (!Decl->TypeForDecl) {
      Decl->TypeForDecl.reset(new UnionType(false, nullptr, Decl));
      Decl->TypeForDeclConst.reset(
          new UnionType(true, Decl->TypeForDecl.get(), Decl));
    }

    if (!IsConst) {
      return Decl->TypeForDecl.get();
    } else {
      return Decl->TypeForDeclConst.get();
    }
  }

  HType *getTypedefType(bool IsConst, TypedefDecl *Decl) {
    if (!Decl->TypeForDecl) {
      Decl->TypeForDecl.reset(new TypedefType(false, Decl));
      Decl->TypeForDeclConst.reset(new TypedefType(true, Decl));
    }

    if (!IsConst) {
      return Decl->TypeForDecl.get();
    } else {
      return Decl->TypeForDeclConst.get();
    }
  }

  void printDecls(llvm::raw_fd_ostream &OS) {
    for (auto &Ent : Decls) {
      Ent.second->print(OS);
      OS << "\n";
    }
  }

  // static HTypeContext Instance;
  // static HTypeContext &getInstance() { return Instance; }
};

} // namespace notdec::ast

#endif
