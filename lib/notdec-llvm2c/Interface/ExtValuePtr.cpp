#include "notdec-llvm2c/Interface/ExtValuePtr.h"
#include "notdec-llvm2c/Interface/ValueNamer.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <cctype>
#include <sstream>
#include <llvm/Support/Casting.h>

namespace notdec {

namespace {

std::string squashWhitespace(std::string S) {
  std::string Ret;
  Ret.reserve(S.size());

  bool InSpace = false;
  for (char C : S) {
    if (std::isspace(static_cast<unsigned char>(C))) {
      if (!Ret.empty()) {
        InSpace = true;
      }
      continue;
    }
    if (InSpace && !Ret.empty()) {
      Ret.push_back(' ');
    }
    Ret.push_back(C);
    InSpace = false;
  }
  return Ret;
}

std::string formatPrintedValue(const llvm::Value &V) {
  std::string Ret;
  llvm::raw_string_ostream OS(Ret);
  V.print(OS);
  OS.flush();
  return squashWhitespace(std::move(Ret));
}

std::string formatFunctionId(const llvm::Function &F) {
  if (F.hasName()) {
    return F.getName().str();
  }

  const llvm::Module *M = F.getParent();
  unsigned Index = 0;
  if (M != nullptr) {
    for (const llvm::Function &Candidate : *M) {
      if (&Candidate == &F) {
        break;
      }
      ++Index;
    }
  }
  return "func" + std::to_string(Index);
}

std::string formatGlobalId(const llvm::GlobalValue &GV) {
  if (GV.hasName()) {
    return "@" + GV.getName().str();
  }

  const llvm::Module *M = GV.getParent();
  unsigned Index = 0;
  if (M != nullptr) {
    for (const llvm::GlobalVariable &Candidate : M->globals()) {
      if (&Candidate == &GV) {
        break;
      }
      ++Index;
    }
  }
  return "@global" + std::to_string(Index);
}

std::string formatBlockId(const llvm::BasicBlock &BB) {
  if (BB.hasName()) {
    return "bb." + BB.getName().str();
  }

  const llvm::Function *F = BB.getParent();
  unsigned Index = 0;
  if (F != nullptr) {
    for (const llvm::BasicBlock &Candidate : *F) {
      if (&Candidate == &BB) {
        break;
      }
      ++Index;
    }
  }
  return "bb" + std::to_string(Index);
}

std::string formatInstructionId(const llvm::Instruction &I) {
  const llvm::Function *F = I.getFunction();
  std::string Prefix = F == nullptr ? "<nofunc>" : formatFunctionId(*F);
  Prefix += "::";

  if (I.hasName()) {
    return Prefix + "%" + I.getName().str();
  }

  const llvm::BasicBlock *BB = I.getParent();
  unsigned InstIndex = 0;
  if (BB != nullptr) {
    for (const llvm::Instruction &Candidate : *BB) {
      if (&Candidate == &I) {
        break;
      }
      ++InstIndex;
    }
    return Prefix + "%" + formatBlockId(*BB) + ".i" + std::to_string(InstIndex);
  }
  return Prefix + "%inst" + std::to_string(InstIndex);
}

std::string formatValueRef(const llvm::Value &V) {
  if (const auto *F = llvm::dyn_cast<llvm::Function>(&V)) {
    return formatGlobalId(*F);
  }
  if (const auto *GV = llvm::dyn_cast<llvm::GlobalValue>(&V)) {
    return formatGlobalId(*GV);
  }
  if (const auto *Arg = llvm::dyn_cast<llvm::Argument>(&V)) {
    return formatFunctionId(*Arg->getParent()) + "::arg" +
           std::to_string(Arg->getArgNo());
  }
  if (const auto *BB = llvm::dyn_cast<llvm::BasicBlock>(&V)) {
    const llvm::Function *F = BB->getParent();
    std::string Prefix = F == nullptr ? "<nofunc>" : formatFunctionId(*F);
    return Prefix + "::" + formatBlockId(*BB);
  }
  if (const auto *I = llvm::dyn_cast<llvm::Instruction>(&V)) {
    return formatInstructionId(*I);
  }
  if (llvm::isa<llvm::Constant>(&V)) {
    return "const(" + formatPrintedValue(V) + ")";
  }
  if (V.hasName()) {
    return "%" + V.getName().str();
  }
  return "value(" + formatPrintedValue(V) + ")";
}

std::string formatUseSite(const llvm::User &U, long OpInd) {
  std::string Ret = formatValueRef(U);
  if (OpInd >= 0) {
    Ret += ":" + std::to_string(OpInd);
  }
  return Ret;
}

} // namespace

std::string toString(const ExtValuePtr &Val) { return toString(Val, false); }

std::string toString(const ExtValuePtr &Val, bool Verbose) {
  std::string Ret;
  llvm::raw_string_ostream OS(Ret);
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (*V == nullptr) {
      return "Value: nullptr";
    }
    if (auto F = llvm::dyn_cast<llvm::Function>(*V)) {
      OS << "Func: " << F->getName();
    } else {
      OS << "Value: " << **V;
    }
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
  } else if (auto SO = std::get_if<StackObject>(&Val)) {
    return "StackObject: " + ValueNamer::getName(*SO->Allocator);
  } else if (auto HO = std::get_if<HeapObject>(&Val)) {
    return "HeapObject: " + ValueNamer::getName(*HO->Allocator);
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
  } else if (auto SO = std::get_if<StackObject>(&Val)) {
    return SO->Allocator->getAllocatedType();
  }
  // else if (auto HO = std::get_if<HeapObject>(&Val)) {
  //   return HO->Allocator->getType()->getPointerElementType();
  // }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: getType: unhandled type of ExtValPtr\n";
  std::abort();
}

unsigned int getSize(llvm::Type *Ty, unsigned int pointer_size) {
  assert(pointer_size != 0);
  if (Ty->isPointerTy() || Ty->isFunctionTy()) {
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

ExtValuePtr canonicalizeExtValue(ExtValuePtr Val, llvm::User *User,
                                 long OpInd) {
  using namespace llvm;
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (isa<GlobalValue>(*V)) {
      return Val;
    }
    if (auto *C = dyn_cast<Constant>(*V)) {
      if (auto *CExpr = dyn_cast<ConstantExpr>(C)) {
        if (CExpr->isCast() && CExpr->getOpcode() == Instruction::IntToPtr) {
          if (auto *CI = dyn_cast<ConstantInt>(CExpr->getOperand(0))) {
            return ConstantAddr{.Val = CI};
          }
        }
      }
      assert(User != nullptr && "llvmValue2ExtVal: Constant User is Null!");
      assert(hasUser(*V, User) &&
             "llvmValue2ExtVal: constant not used by user");
      return UConstant{.Val = cast<Constant>(*V), .User = User, .OpInd = OpInd};
    }
  }
  return Val;
}

ExtValuePtr getExtValuePtr(llvm::Value *V, llvm::User *User, long OpInd) {
  return canonicalizeExtValue(ExtValuePtr{V}, User, OpInd);
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
  } else if (auto SO = std::get_if<StackObject>(&Val)) {
    return "StackObj_" + ValueNamer::getName(*SO->Allocator);
  } else if (auto HO = std::get_if<HeapObject>(&Val)) {
    return "HeapObj_" + ValueNamer::getName(*HO->Allocator);
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: getName: unhandled type of ExtValPtr\n";
  std::abort();
}

std::string toStableString(const ExtValuePtr &Val) {
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    if (*V == nullptr) {
      return "<null-value>";
    }
    return formatValueRef(**V);
  }
  if (auto F = std::get_if<ReturnValue>(&Val)) {
    std::string Ret = formatFunctionId(*F->Func) + "::<ret>";
    if (F->Index != 0) {
      Ret = formatFunctionId(*F->Func) + "::<ret:" +
            std::to_string(F->Index) + ">";
    }
    return Ret;
  }
  if (auto IC = std::get_if<UConstant>(&Val)) {
    std::string Ret = "const(" + formatPrintedValue(*IC->Val) + ")";
    if (IC->User != nullptr) {
      Ret += "@" + formatUseSite(*IC->User, IC->OpInd);
    }
    return Ret;
  }
  if (auto CA = std::get_if<ConstantAddr>(&Val)) {
    return "addr(" + int_to_hex(CA->Val->getSExtValue()) + ")";
  }
  if (auto SO = std::get_if<StackObject>(&Val)) {
    return formatFunctionId(*SO->Allocator->getFunction()) + "::stack(" +
           formatInstructionId(*SO->Allocator) + ")";
  }
  if (auto HO = std::get_if<HeapObject>(&Val)) {
    return formatFunctionId(*HO->Allocator->getFunction()) + "::heap(" +
           formatInstructionId(*HO->Allocator) + ")";
  }
  llvm::errs() << __FILE__ << ":" << __LINE__ << ": "
               << "ERROR: toStableString: unhandled type of ExtValPtr\n";
  std::abort();
}

} // namespace notdec
