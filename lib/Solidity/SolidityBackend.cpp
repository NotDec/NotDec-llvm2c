#include "notdec-backends/Solidity/Backend.h"
#include "notdec-backends/Solidity/Printer.h"
#include "notdec-backends/Solidity/Reader.h"

namespace notdec::backend::solidity {

void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_ostream &OS, const Options &Opts,
                     std::unique_ptr<::notdec::llvm2c::HTypeResult> HT) {
  (void)M;
  (void)MAM;
  (void)Opts;

  SourceUnit Unit = Reader().read(M, HT.get());
  Printer(OS).print(Unit);
}

} // namespace notdec::backend::solidity
