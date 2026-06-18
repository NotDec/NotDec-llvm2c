#ifndef NOTDEC_BACKENDS_SOLIDITY_READER_H
#define NOTDEC_BACKENDS_SOLIDITY_READER_H

#include <llvm/IR/Module.h>

#include "notdec-backends/Solidity/Ast.h"

namespace notdec::backend::solidity {

class Reader {
public:
  SourceUnit read(const llvm::Module &M);

private:
  Contract readContract(const llvm::Module &M);
  static bool isPublicEntryFunction(const llvm::Function &F);
  static Function readFunction(const llvm::Function &F);
  static std::vector<Parameter> readReturns(const llvm::Function &F);
  static void applyFunctionNameAndParams(llvm::StringRef IRName,
                                         Function &Result);
  static std::vector<Parameter> parseAbiParameters(llvm::StringRef Encoded);
  static bool isKnownAbiType(llvm::StringRef Type);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
