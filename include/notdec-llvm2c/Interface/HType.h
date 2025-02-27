#ifndef _NOTDEC_INTERFACE_HTCONTEXT_H_
#define _NOTDEC_INTERFACE_HTCONTEXT_H_

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

namespace notdec {

class HTypeFactory;

/*
Type Architecture:
  - SimpleType
    - IntegerType
    - FloatingType
    - PointerType
  - FunctionType
  - CompoundType
    - StructType
    - ArrayType
*/
class HType {
public:
  enum HTypeKind {
    TK_Simple,
    TK_Integer,
    TK_Float,
    TK_Pointer,
    // end of SimpleType
    TK_Function,

    TK_Compound,
    TK_Struct,
    TK_Array
    // end of CompoundType
  };

protected:
  HType(HTypeKind K, bool IsConst) : Kind(K), IsConst(IsConst) {}

public:
  HTypeKind getKind() const { return Kind; }
  bool isConst() const { return IsConst; }

private:
  HTypeKind Kind;
  bool IsConst;
};

class SimpleType : public HType {
protected:
  SimpleType(HTypeKind K, bool IsConst, unsigned BitSize)
      : HType(K, IsConst), BitSize(BitSize) {}

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
  bool IsUnsigned;

  IntegerType(bool IsConst, unsigned BitSize, bool IsUnsigned)
      : SimpleType(TK_Integer, IsConst, BitSize), IsUnsigned(IsUnsigned) {}
  friend class HTypeFactory;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Integer; }

  bool isUnsigned() const { return IsUnsigned; }
};

// 浮点类型
class FloatingType : public SimpleType {
  FloatingType(bool IsConst, unsigned BitSize)
      : SimpleType(TK_Float, IsConst, BitSize) {}

  friend class HTypeFactory;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Float; }
};

// 指针类型
class PointerType : public SimpleType {
  HType *PointeeType;

  PointerType(bool IsConst, unsigned PointerSize, HType *Pointee)
      : SimpleType(TK_Pointer, PointerSize, IsConst), PointeeType(Pointee) {}
  friend class HTypeFactory;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Pointer; }

  HType *getPointeeType() const { return PointeeType; }
};

// 函数类型
class FunctionType : public HType {
  HType *ReturnType;
  std::vector<HType *> ParamTypes;
  FunctionType(bool IsConst, HType *Return, const std::vector<HType *> &Params)
      : HType(TK_Function, IsConst), ReturnType(Return), ParamTypes(Params) {}
  friend class HTypeFactory;

public:
  static bool classof(const HType *T) { return T->getKind() == TK_Function; }
  HType *getReturnType() const { return ReturnType; }
  const std::vector<HType *> &getParamTypes() const { return ParamTypes; }
};

/* Factory class implementation */
class HTypeFactory {
  using IntegerKey = std::tuple<bool, unsigned, bool>;
  static std::map<IntegerKey, std::unique_ptr<IntegerType>> integerTypes;

  using FloatKey = std::tuple<bool, unsigned>;
  static std::map<FloatKey, std::unique_ptr<FloatingType>> floatTypes;

  using PointerKey = std::tuple<bool, unsigned, HType*>;
  static std::map<PointerKey, std::unique_ptr<PointerType>> pointerTypes;

  using FunctionKey = std::tuple<bool, HType*, std::vector<HType*>>;
  static std::map<FunctionKey, std::unique_ptr<FunctionType>> functionTypes;

public:
  static HType* getIntegerType(bool IsConst, unsigned BitSize, bool IsUnsigned) {
    auto key = std::make_tuple(IsConst, BitSize, IsUnsigned);
    auto& entry = integerTypes[key];
    if (!entry) {
      entry.reset(new IntegerType(IsConst, BitSize, IsUnsigned));
    }
    return entry.get();
  }

  static HType* getFloatType(bool IsConst, unsigned BitSize) {
    auto key = std::make_tuple(IsConst, BitSize);
    auto& entry = floatTypes[key];
    if (!entry) {
      entry.reset(new FloatingType(IsConst, BitSize));
    }
    return entry.get();
  }

  static HType* getPointerType(bool IsConst, unsigned PtrSize, HType* Pointee) {
    auto key = std::make_tuple(IsConst, PtrSize, Pointee);
    auto& entry = pointerTypes[key];
    if (!entry) {
      entry.reset(new PointerType(IsConst, PtrSize, Pointee));
    }
    return entry.get();
  }

  static HType* getFunctionType(bool IsConst, HType* Ret, const std::vector<HType*>& Params) {
    auto key = std::make_tuple(IsConst, Ret, Params);
    auto& entry = functionTypes[key];
    if (!entry) {
      entry.reset(new FunctionType(IsConst, Ret, Params));
    }
    return entry.get();
  }
};

} // namespace notdec

#endif
