/// The main interface headers
/// Do not include any clang headers, so that users do not need to install
/// clang.

#ifndef _NOTDEC_BACKEND_INTERFACE_H_
#define _NOTDEC_BACKEND_INTERFACE_H_

#include <clang/AST/Type.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <memory>
#include <variant>

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  StructuralAlgorithms algo;
};

struct RetVal {
  llvm::Function *Func;
  int32_t Index = 0;
  bool operator<(const RetVal &rhs) const {
    return std::tie(Func, Index) < std::tie(rhs.Func, rhs.Index);
  }
  bool operator==(const RetVal &rhs) const {
    return !(*this < rhs) && !(rhs < *this);
  }
};

struct UsedConstant {
  llvm::Constant *Val;
  llvm::User *User;
  UsedConstant(llvm::Constant *Val, llvm::User *User) : Val(Val), User(User) {
    assert(Val != nullptr && User != nullptr);
  }
  bool operator<(const UsedConstant &rhs) const {
    return std::tie(Val, User) < std::tie(rhs.Val, rhs.User);
  }
  bool operator==(const UsedConstant &rhs) const {
    return !(*this < rhs) && !(rhs < *this);
  }
};

// wrapped value pointer
using WValuePtr = std::variant<llvm::Value *, RetVal, UsedConstant>;

inline llvm::Type *getTy(WValuePtr Val) {
  if (auto V = std::get_if<llvm::Value *>(&Val)) {
    return (*V)->getType();
  } else if (auto Ret = std::get_if<RetVal>(&Val)) {
    return Ret->Func->getReturnType();
  } else if (auto IC = std::get_if<UsedConstant>(&Val)) {
    return IC->Val->getType();
  }
  llvm_unreachable("unknown WValuePtr");
}

struct HighTypes {
  // TODO refactor to std::map<WValuePtr, std::pair<clang::QualType,
  // clang::QualType>>
  std::map<WValuePtr, clang::QualType> ValueTypes;
  std::map<WValuePtr, clang::QualType> ValueTypesUpperBound;
  std::unique_ptr<clang::ASTUnit> ASTUnit;

  HighTypes() = default;
  HighTypes(HighTypes &&Other) = default;
  HighTypes &operator=(HighTypes &&Other) = default;
  void dump() const {
    auto PrintMap = [](const auto &Map) -> void {
      for (auto &VT : Map) {
        llvm::errs() << "  ";
        if (auto Val = std::get_if<llvm::Value *>(&VT.first)) {
          if (auto Arg = llvm::dyn_cast<llvm::Argument>(*Val)) {
            llvm::errs() << Arg->getParent()->getName() << ": ";
          } else if (auto Inst = llvm::dyn_cast<llvm::Instruction>(*Val)) {
            llvm::errs() << Inst->getParent()->getParent()->getName() << ": ";
          }
          llvm::errs() << **Val << ": ";
        } else if (auto Ret = std::get_if<RetVal>(&VT.first)) {
          llvm::errs() << Ret->Func->getName() << " RetVal: ";
        } else if (auto IC = std::get_if<UsedConstant>(&VT.first)) {
          llvm::errs() << *IC->Val << " -> ";
        }
        llvm::errs() << VT.second.getAsString() << "\n";
      }
    };

    llvm::errs() << "HighTypes.ValueTypes:\n";
    PrintMap(ValueTypes);
    llvm::errs() << "HighTypes.ValueTypesUpperBound:\n";
    PrintMap(ValueTypesUpperBound);
  }
};

// main interface
void decompileModule(llvm::Module &M, llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HighTypes> HT = nullptr);

void demoteSSA(llvm::Module &M);

} // namespace notdec::llvm2c

#endif
