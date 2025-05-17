#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Type.h>

#include "notdec-llvm2c/Interface/Utils.h"

#ifndef _NOTDEC_EXTVALUEPTR_H_
#define _NOTDEC_EXTVALUEPTR_H_

namespace notdec {

struct ReturnValue {
  llvm::Function *Func;
  int32_t Index = 0;
  bool operator<(const ReturnValue &rhs) const {
    return std::tie(Func, Index) < std::tie(rhs.Func, rhs.Index);
  }
  bool operator==(const ReturnValue &rhs) const {
    return !(*this < rhs) && !(rhs < *this);
  }
};
/// Differentiate pointer-sized int constant. It can be pointer or int under
/// different context.
struct UConstant {
  llvm::Constant *Val;
  llvm::User *User;
  long OpInd = -1;
  bool operator<(const UConstant &rhs) const {
    return std::tie(Val, User, OpInd) < std::tie(rhs.Val, rhs.User, rhs.OpInd);
  }
  bool operator==(const UConstant &rhs) const {
    return !(*this < rhs) && !(rhs < *this);
  }
};

// Global variable address / constant address pointer
// To merge different constant expr like (inttoptr i32 1064)
struct ConstantAddr {
  llvm::ConstantInt *Val;
  bool operator<(const ConstantAddr &rhs) const {
    return std::tie(Val) < std::tie(rhs.Val);
  }
  bool operator==(const ConstantAddr &rhs) const {
    return !(*this < rhs) && !(rhs < *this);
  }
};

// We cannot directly map llvm::Value* to Node, because we need to
// differentiate/merge different constants. Extend Value* with some special
// values.
// 1. Differentiate Constants by Users.
// 2. For some constant expr like (inttoptr i32 1064), the address of the expr
// is different, but we need to merge them.
// 3. For Index into global variables like env.table@4, the case is the same.
// So, in general, for constant expr, we probably should always convert to
// NodeKey instead of maintain the mapping
using ExtValuePtr =
    std::variant<llvm::Value *, ReturnValue, UConstant, ConstantAddr>;

ExtValuePtr getExtValuePtr(llvm::Value *Val, llvm::User *User, long OpInd = -1);
std::string getName(const ExtValuePtr &Val);
std::string toString(const ExtValuePtr &Val, bool Verbose);
std::string toString(const ExtValuePtr &Val);
void dump(const ExtValuePtr &Val);
llvm::Type *getType(const ExtValuePtr &Val);
unsigned int getSize(llvm::Type *Ty, unsigned int pointer_size);
unsigned int getSize(const ExtValuePtr &Val, unsigned int pointer_size);
inline void llvmValue2ExtVal(ExtValuePtr &Val, llvm::User *User, long OpInd) {
  using namespace llvm;
  // Differentiate int32/int64 by User.
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (isa<GlobalValue>(*V)) {
      return;
    }
    if (auto CI = dyn_cast<Constant>(*V)) {
      // Convert inttoptr constant int to ConstantAddr
      if (auto CExpr = dyn_cast<ConstantExpr>(CI)) {
        if (CExpr->isCast() && CExpr->getOpcode() == Instruction::IntToPtr) {
          if (auto CI1 = dyn_cast<ConstantInt>(CExpr->getOperand(0))) {
            V = nullptr;
            Val = ConstantAddr{.Val = CI1};
            return;
          }
        }
      }
      assert(User != nullptr && "RetypdGenerator::getTypeVar: User is Null!");
      assert(hasUser(*V, User) &&
             "convertTypeVarVal: constant not used by user");
      Val = UConstant{.Val = cast<Constant>(*V), .User = User, .OpInd = OpInd};
    }
  }
}

// Check if differentiated constants (especially int32/int64 constants) by User.
inline bool checkWrapped(ExtValuePtr &Val) {
  using namespace llvm;
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (!isa<GlobalValue>(*V)) {
      if (auto CI = dyn_cast<Constant>(*V)) {
        return false;
        // assert(false && "Should already be converted to UConstant");
      }
    }
  }
  return true;
}
} // namespace notdec


#endif
