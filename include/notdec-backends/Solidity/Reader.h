#ifndef NOTDEC_BACKENDS_SOLIDITY_READER_H
#define NOTDEC_BACKENDS_SOLIDITY_READER_H

#include <set>

#include <llvm/IR/Module.h>

#include "notdec-backends/Core/HTypeResult.h"
#include "notdec-backends/Solidity/Ast.h"
#include "notdec-backends/Solidity/BodyBuilder.h"
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
  // Route B: evm2llvm outlines shared code into private__* helpers.  A helper
  // that can be rendered completely becomes a real Solidity function; call
  // sites then reference it instead of losing the call.
  static bool isHelperRenderCandidate(const llvm::Function &F);
  static std::set<const llvm::Function *>
  collectHelperCandidates(const llvm::Module &M,
                          const std::vector<const llvm::Function *> &Roots);
  static Function readFunction(const llvm::Function &F,
                               const StorageSlotMap *StorageSlots,
                               const EventParamTypeMap *EventParamTypes,
                               const HelperRenderMap *Helpers = nullptr);
  static Function readHelperFunction(const llvm::Function &F,
                                     const StorageSlotMap *StorageSlots,
                                     const EventParamTypeMap *EventParamTypes,
                                     const HelperRenderMap *Helpers);
  static std::vector<Parameter> readHelperReturns(const llvm::Function &F);
  static Block readBody(const llvm::Function &F,
                        const StorageSlotMap *StorageSlots,
                        const std::vector<std::string> *ArgumentNames,
                        const ParameterTypeMap *ParameterTypes,
                        const std::vector<std::string> *ReturnTypes,
                        const EventParamTypeMap *EventParamTypes,
                        const HelperRenderMap *Helpers = nullptr);
  static std::vector<Parameter> readReturns(const llvm::Function &F);
  static void applyFunctionNameAndParams(llvm::StringRef IRName,
                                         Function &Result);
  static std::vector<Parameter> parseAbiParameters(llvm::StringRef Encoded);
  static bool isKnownAbiType(llvm::StringRef Type);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
