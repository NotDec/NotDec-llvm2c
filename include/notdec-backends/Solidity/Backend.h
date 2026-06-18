#ifndef NOTDEC_BACKENDS_SOLIDITY_BACKEND_H
#define NOTDEC_BACKENDS_SOLIDITY_BACKEND_H

#include <memory>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/raw_ostream.h>

#include "notdec-backends/Core/HTypeResult.h"

namespace notdec::backend::solidity {

struct Options {
  std::string workDir;
  bool emitCommentsForUnknown = true;
};

void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_ostream &OS, const Options &Opts,
                     std::unique_ptr<::notdec::llvm2c::HTypeResult> HT =
                         nullptr);

} // namespace notdec::backend::solidity

#endif
