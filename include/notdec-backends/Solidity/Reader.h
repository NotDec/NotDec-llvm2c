#ifndef NOTDEC_BACKENDS_SOLIDITY_READER_H
#define NOTDEC_BACKENDS_SOLIDITY_READER_H

#include <llvm/IR/Module.h>

#include "notdec-backends/Core/HTypeResult.h"
#include "notdec-backends/Solidity/Ast.h"
#include "notdec-backends/Solidity/StorageInfo.h"

namespace notdec::backend::solidity {

class Reader {
public:
  SourceUnit read(const llvm::Module &M,
                  const ::notdec::llvm2c::HTypeResult *HT = nullptr);

private:
  Contract readContract(const llvm::Module &M,
                        const ::notdec::llvm2c::HTypeResult *HT);
  static void readEvents(const llvm::Module &M, Contract &Result);
  static void readStateVariables(const ::notdec::llvm2c::HTypeResult &HT,
                                 Contract &Result,
                                 StorageSlotMap &StorageSlots);
  static bool isPublicEntryFunction(const llvm::Function &F);
  static Function readFunction(const llvm::Function &F,
                               const StorageSlotMap *StorageSlots,
                               const EventParamTypeMap *EventParamTypes);
  static Block readBody(const llvm::Function &F,
                        const StorageSlotMap *StorageSlots,
                        const std::vector<std::string> *ArgumentNames,
                        const EventParamTypeMap *EventParamTypes);
  static std::vector<Parameter> readReturns(const llvm::Function &F);
  static void applyFunctionNameAndParams(llvm::StringRef IRName,
                                         Function &Result);
  static std::vector<Parameter> parseAbiParameters(llvm::StringRef Encoded);
  static bool isKnownAbiType(llvm::StringRef Type);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
