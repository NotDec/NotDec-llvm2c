#include "notdec-llvm2c/Interface/StructManager.h"

#include <cassert>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <memory>

namespace notdec {

std::shared_ptr<BytesManager> createBytesManager(llvm::Module &M) {
  std::shared_ptr<BytesManager> BM = std::make_shared<BytesManager>();
  for (auto &G : M.globals()) {
    auto Name = G.getName();
    if (!Name.consume_front("__notdec_mem0_0x")) {
      continue;
    }
    auto Offset = std::stol(Name.str(), nullptr, 16);
    assert(Offset >= 0);
    if (G.hasInitializer()) {
      auto *Init = G.getInitializer();
      if (auto *C = llvm::dyn_cast<llvm::ConstantDataArray>(Init)) {
        auto Data = C->getRawDataValues();
        BM->Bytes.emplace_back(
            SimpleRange{.Start = Offset,
                        .Size = static_cast<OffsetTy>(Data.size())},
            Data.str());
      }
    }
  }
  return BM;
}

} // namespace notdec
