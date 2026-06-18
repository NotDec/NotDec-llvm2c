/// The main interface headers

#ifndef _NOTDEC_BACKEND_INTERFACE_H_
#define _NOTDEC_BACKEND_INTERFACE_H_

#include <memory>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/raw_ostream.h>

#include "notdec-backends/Core/HTypeResult.h"

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  bool filterUnusedDefinitions = false;
  StructuralAlgorithms algo;
  std::string workDir;
};

// main interface
void decompileModule(llvm::Module &M, llvm::ModuleAnalysisManager &MAM,
                     llvm::raw_fd_ostream &os, Options opts,
                     std::unique_ptr<HTypeResult> HT = nullptr);

void demoteSSA(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

} // namespace notdec::llvm2c

#endif
