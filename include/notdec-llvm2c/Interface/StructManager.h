#ifndef _NOTDEC_STRUCTMANAGER_H_
#define _NOTDEC_STRUCTMANAGER_H_

#include "binarysub/StructManager.h"

namespace llvm {
class Module;
}

namespace notdec {
std::shared_ptr<BytesManager> createBytesManager(llvm::Module &M);
}

#endif
