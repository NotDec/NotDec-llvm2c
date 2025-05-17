#include "notdec-llvm2c/Interface/ExtValuePtr.h"
#include "notdec-llvm2c/Interface/ValueNamer.h"

namespace notdec {

std::string toString(const ExtValuePtr &Val) { return toString(Val, false); }

std::string toString(const ExtValuePtr &Val, bool Verbose) {
  std::string Ret;
  llvm::raw_string_ostream OS(Ret);
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    OS << "Value: " << **V;
    if (Verbose) {
      llvm::Function *F = nullptr;
      if (auto I = llvm::dyn_cast<llvm::Instruction>(*V)) {
        F = I->getFunction();
      } else if (auto Arg = llvm::dyn_cast<llvm::Argument>(*V)) {
        F = Arg->getParent();
      }
      if (F) {
        OS << " (In Func: " << F->getName() << ")";
      }
    }
  } else if (auto F = std::get_if<ReturnValue>(&Val)) {
    OS << "ReturnValue: " + ValueNamer::getName(*F->Func, "func_");
  } else if (auto IC = std::get_if<UConstant>(&Val)) {
    OS << "IntConstant: " << *IC->Val << ", User: " << *IC->User;
  } else if (auto CA = std::get_if<ConstantAddr>(&Val)) {
    OS << "ConstantAddr: " << *CA->Val;
  } else {
    llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
                 << "ERROR: getName: unhandled type of ExtValPtr\n";
    std::abort();
  }
  return Ret;
}

void dump(const ExtValuePtr &Val) { llvm::errs() << toString(Val) << "\n"; }

llvm::Type *getType(const ExtValuePtr &Val) {
  using namespace llvm;
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (auto F = dyn_cast<Function>(*V)) {
      return F->getFunctionType();
    }
    return (*V)->getType();
  } else if (auto F = std::get_if<ReturnValue>(&Val)) {
    return F->Func->getReturnType();
  } else if (auto IC = std::get_if<UConstant>(&Val)) {
    return IC->Val->getType();
  } else if (auto CA = std::get_if<ConstantAddr>(&Val)) {
    return CA->Val->getType();
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: getType: unhandled type of ExtValPtr\n";
  std::abort();
}

unsigned int getSize(llvm::Type *Ty, unsigned int pointer_size) {
  assert(pointer_size != 0);
  if (Ty->isPointerTy()) {
    return pointer_size;
  }
  if (!Ty->isAggregateType() && !Ty->isVectorTy()) {
    return Ty->getScalarSizeInBits();
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: getSize: unhandled llvm type: " << *Ty << "\n";
  std::abort();
}

unsigned int getSize(const ExtValuePtr &Val, unsigned int pointer_size) {
  return getSize(getType(Val), pointer_size);
}

// TODO refactor: increase code reusability
ExtValuePtr getExtValuePtr(llvm::Value *V, llvm::User *User, long OpInd) {
  using namespace llvm;
  ExtValuePtr Val = V;
  if (!isa<GlobalValue>(*V)) {
    if (auto CI = dyn_cast<Constant>(V)) {
      assert(User != nullptr && "RetypdGenerator::getTypeVar: User is Null!");
      assert(hasUser(V, User) &&
             "convertTypeVarVal: constant not used by user");
      Val = UConstant{.Val = cast<Constant>(V), .User = User, .OpInd = OpInd};
    }
  }
  return Val;
}

std::string getName(const ExtValuePtr &Val) {
  using namespace llvm;
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    return ValueNamer::getName(**V);
  } else if (auto F = std::get_if<ReturnValue>(&Val)) {
    return "ReturnValue_" + ValueNamer::getName(*F->Func, "func_");
  } else if (auto IC = std::get_if<UConstant>(&Val)) {
    if (auto CI = dyn_cast<ConstantInt>(IC->Val)) {
      return "IntConstant_" + int_to_hex(CI->getSExtValue());
    } else {
      return "Constant_" + ValueNamer::getName(*IC->Val, "constant_");
    }
  } else if (auto CA = std::get_if<ConstantAddr>(&Val)) {
    return "ConstantAddr_" + int_to_hex(CA->Val->getSExtValue());
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: getName: unhandled type of ExtValPtr\n";
  std::abort();
}
} // namespace notdec
