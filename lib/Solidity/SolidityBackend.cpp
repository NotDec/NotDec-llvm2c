#include "notdec-backends/Solidity/Backend.h"

namespace notdec::backend::solidity {

void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_ostream &OS, const Options &Opts,
                     std::unique_ptr<::notdec::llvm2c::HTypeResult> HT) {
  (void)M;
  (void)MAM;
  (void)Opts;
  (void)HT;

  OS << "contract Decompiled {\n";
  OS << "}\n";
}

} // namespace notdec::backend::solidity
