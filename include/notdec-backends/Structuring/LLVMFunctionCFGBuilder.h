#ifndef NOTDEC_BACKENDS_STRUCTURING_LLVMFUNCTIONCFGBUILDER_H
#define NOTDEC_BACKENDS_STRUCTURING_LLVMFUNCTIONCFGBUILDER_H

#include <vector>

#include <llvm/ADT/StringRef.h>

#include "notdec-backends/Structuring/StructuredCFG.h"

namespace llvm {
class BasicBlock;
class ConstantInt;
class Function;
class Value;
} // namespace llvm

namespace notdec::backend::structuring {

// Converts LLVM control-flow shape into the language-neutral StructuredCFG.
// Backends keep ownership of expression/statement payloads through the
// PayloadProvider callbacks, so this adapter can be reused by C, Solidity, and
// future backends without depending on any target AST.
class LLVMFunctionCFGBuilder {
public:
  class PayloadProvider {
  public:
    virtual ~PayloadProvider() = default;

    virtual void collectStatements(const llvm::BasicBlock &BB,
                                   std::vector<PayloadRef> &Out) = 0;
    virtual PayloadRef getCondition(const llvm::Value &V,
                                    llvm::StringRef FallbackName) = 0;
    virtual PayloadRef getSwitchCase(const llvm::ConstantInt &V) = 0;
  };

  static StructuredCFG build(const llvm::Function &F,
                             PayloadProvider &Provider);
};

} // namespace notdec::backend::structuring

#endif
