#ifndef NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H
#define NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H

#include <map>
#include <optional>
#include <string>
#include <variant>
#include <utility>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Instruction.h>

#include "notdec-backends/Solidity/Ast.h"
#include "notdec-backends/Solidity/StorageInfo.h"

namespace llvm {
class Function;
}

namespace notdec::backend::structuring {
class StructuredTree;
}

namespace notdec::backend::solidity {

// A private helper function the backend renders as a real Solidity function.
// evm2llvm always passes the four runtime pointers (mem, calldata, returndata,
// env) first; Solidity has those as implicit globals, so the generated function
// does not declare them and call sites drop the matching arguments.
struct HelperRenderInfo {
  std::string Name;
  unsigned RuntimeArgCount = 4;
};

using HelperRenderMap = std::map<const llvm::Function *, HelperRenderInfo>;

// Builds the current Solidity function-body fallback.  This is intentionally
// separate from Reader so the LLVM CFG -> StructuredCFG adapter can evolve
// without mixing contract-level ABI/storage discovery with control-flow output.
class BodyBuilder {
public:
  using Payload = std::variant<Statement, Expression>;

  static Block readBody(const llvm::Function &F,
                        const StorageSlotMap *StorageSlots = nullptr,
                        const std::vector<std::string> *ArgumentNames = nullptr,
                        const ParameterTypeMap *ParameterTypes = nullptr,
                        const std::vector<std::string> *ReturnTypes = nullptr,
                        const EventParamTypeMap *EventParamTypes = nullptr,
                        const HelperRenderMap *Helpers = nullptr);
  // A helper body is only emitted when every statement in it was recovered:
  // no unresolved value, no condition TODO, and no control-flow gap comment
  // (goto / switch / body TODO).  Otherwise the call site keeps its explicit
  // TODO instead of moving the same gap into a generated function.
  static bool blockIsFullyRecovered(const Block &Body);
  static Block
  renderStructuredBody(const structuring::StructuredTree &Tree,
                       const std::vector<Payload> &Payloads);
  static Payload rewriteCopiedDephicationVVars(
      const Payload &Payload,
      const std::vector<std::pair<std::string, std::string>> &Copies);
  static std::optional<std::string> getStringMetadata(const llvm::Instruction &I,
                                                      llvm::StringRef Kind);
  static std::optional<std::string>
  getEventName(const llvm::Instruction &I, llvm::StringRef Kind);
  static std::vector<ExprPtr> getEventTopicArguments(const llvm::Instruction &I);

private:
  static Statement formatRevertStatement(const llvm::Instruction &I,
                                         llvm::StringRef Kind);
  static Statement formatEventStatement(const llvm::Instruction &I,
                                        llvm::StringRef Kind);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
