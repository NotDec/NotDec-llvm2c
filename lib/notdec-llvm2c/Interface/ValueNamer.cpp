#include "notdec-llvm2c/Interface/ValueNamer.h"
#include <iostream>
#include <llvm/ADT/StringExtras.h>

namespace notdec {

std::set<size_t> TraceIds;

void loadTraceStr(const char *Traces) {
  std::string TraceStr(Traces);
  std::cerr << "Tracing: ";
  // split by ','
  for (auto &Id : llvm::split(TraceStr, ',')) {
    std::cerr << Id.str() << ", ";
    TraceIds.insert(std::stoul(Id.str()));
  }
  std::cerr << "\n";
}

ValueNamer ValueNamer::Instance = ValueNamer();
const char *ValueNamer::DefaultPrefix = "v_";
const char *ValueNamer::FuncPrefix = "func_";
const char *ValueNamer::PhiPrefix = "phi_";
const char *ValueNamer::SelectPrefix = "select_";
const char *ValueNamer::NewPrefix = "new_";
const char *ValueNamer::AddPrefix = "add_";
const char *ValueNamer::SubPrefix = "sub_";
const char *ValueNamer::StackPrefix = "stack_";
const char *ValueNamer::AllocaPrefix = "alloca_";
const char *ValueNamer::LoadPrefix = "load_";
const char *ValueNamer::StorePrefix = "store_";

} // namespace notdec
