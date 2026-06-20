#ifndef NOTDEC_BACKENDS_C_BACKEND_H
#define NOTDEC_BACKENDS_C_BACKEND_H

#include <utility>

#include "notdec-llvm2c/Interface.h"

namespace notdec::backend::c {

using Options = ::notdec::llvm2c::Options;
using HTypeResult = ::notdec::llvm2c::HTypeResult;

inline void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                            llvm::raw_fd_ostream &OS, Options Opts,
                            std::unique_ptr<HTypeResult> HT = nullptr) {
  ::notdec::llvm2c::decompileModule(M, MAM, OS, Opts, std::move(HT));
}

inline void demoteSSA(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  ::notdec::llvm2c::demoteSSA(M, MAM);
}

} // namespace notdec::backend::c

#endif
