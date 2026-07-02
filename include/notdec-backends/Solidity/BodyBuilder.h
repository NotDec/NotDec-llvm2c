#ifndef NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H
#define NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H

#include <optional>
#include <string>
#include <variant>
#include <utility>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Instruction.h>

#include "notdec-backends/Solidity/Ast.h"

namespace llvm {
class Function;
}

namespace notdec::backend::structuring {
class StructuredTree;
}

namespace notdec::backend::solidity {

// Builds the current Solidity function-body fallback.  This is intentionally
// separate from Reader so the LLVM CFG -> StructuredCFG adapter can evolve
// without mixing contract-level ABI/storage discovery with control-flow output.
class BodyBuilder {
public:
  using Payload = std::variant<Statement, Expression>;

  static Block readBody(const llvm::Function &F);
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
