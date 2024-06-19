/// The main interface headers
/// Do not include any clang headers, so that users do not need to install
/// clang.

#ifndef _NOTDEC_BACKEND_INTERFACE_H_
#define _NOTDEC_BACKEND_INTERFACE_H_

#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

namespace notdec::llvm2c {

enum StructuralAlgorithms { SA_Goto, SA_Phoenix };

struct Options {
  bool noDemoteSSA = false;
  bool enableColor = false;
  StructuralAlgorithms algo;
};

// main interface
void decompileModule(llvm::Module &M, llvm::raw_fd_ostream &os, Options opts);

void demoteSSA(llvm::Module &M);

} // namespace notdec::llvm2c

#endif
