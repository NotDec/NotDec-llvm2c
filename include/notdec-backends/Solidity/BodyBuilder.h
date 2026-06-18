#ifndef NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H
#define NOTDEC_BACKENDS_SOLIDITY_BODYBUILDER_H

#include <optional>
#include <string>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Instruction.h>

namespace llvm {
class Function;
}

namespace notdec::backend::solidity {

// Builds the current Solidity function-body fallback.  This is intentionally
// separate from Reader so the LLVM CFG -> StructuredCFG adapter can evolve
// without mixing contract-level ABI/storage discovery with control-flow output.
class BodyBuilder {
public:
  static std::vector<std::string> readBody(const llvm::Function &F);
  static std::optional<std::string> getStringMetadata(const llvm::Instruction &I,
                                                      llvm::StringRef Kind);
  static std::optional<std::string>
  getEventName(const llvm::Instruction &I, llvm::StringRef Kind);

private:
  static std::string formatRevertStatement(const llvm::Instruction &I,
                                           llvm::StringRef Kind);
  static std::string formatEventStatement(const llvm::Instruction &I,
                                          llvm::StringRef Kind);
  static std::string sanitizeIdentifier(llvm::StringRef Name);
};

} // namespace notdec::backend::solidity

#endif
