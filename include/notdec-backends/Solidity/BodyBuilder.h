#ifndef NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H
#define NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Instruction.h>

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
  static std::vector<std::string> readBody(const llvm::Function &F);
  static std::vector<std::string>
  renderStructuredBody(const structuring::StructuredTree &Tree,
                       const std::vector<std::string> &Payloads);
  static std::string rewriteCopiedDephicationVVars(
      std::string Text,
      const std::vector<std::pair<std::string, std::string>> &Copies);
  static std::optional<std::string> getStringMetadata(const llvm::Instruction &I,
                                                      llvm::StringRef Kind);
  static std::optional<std::string>
  getEventName(const llvm::Instruction &I, llvm::StringRef Kind);
  static std::vector<std::string>
  getEventTopicArguments(const llvm::Instruction &I);

private:
  static std::string formatRevertStatement(const llvm::Instruction &I,
                                           llvm::StringRef Kind);
  static std::string formatEventStatement(const llvm::Instruction &I,
                                          llvm::StringRef Kind);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
