#include <iostream>
#include <string>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>

#include "notdec-llvm2c/structural-analysis.h"
#include "notdec-llvm2c/utils.h"

using namespace llvm;

static cl::opt<std::string>
    inputFilename(cl::Positional, cl::desc("<input file>"),
                  cl::value_desc("input LLVM IR file, either .ll or .bc path."),
                  cl::Required);

static cl::OptionCategory mainOpt("! NotDec Options");
static cl::opt<std::string> outputFilename("o", cl::desc("Specify output path"),
                                           cl::value_desc("output.c"),
                                           cl::Optional, cl::cat(mainOpt));
static cl::opt<bool>
    disablePass("disable-pass",
                cl::desc("Disable IR passes. Eliminate phi node by yourself "
                         "before enabling this."),
                cl::init(false), cl::cat(mainOpt));
static cl::opt<bool>
    enableColor("notdec-color",
                cl::desc("Enable color output for things like CFG dump."),
                cl::init(false), cl::cat(mainOpt));

std::string getSuffix(std::string fname) {
  std::size_t ind = fname.find_last_of('.');
  if (ind != std::string::npos) {
    return fname.substr(ind);
  }
  return std::string();
}

// https://llvm.org/docs/ProgrammersManual.html#the-llvm-debug-macro-and-debug-option
// initialize function for the fine-grained debug info with DEBUG_TYPE and the
// -debug-only option
namespace llvm {
void initDebugOptions();
}

int main(int argc, char *argv[]) {
  // parse cmdline
  cl::ParseCommandLineOptions(argc, argv,
                              "NotDec llvm to C decompiler backend: Translates "
                              "LLVM IR or bytecode file into C file.\n");
  // initDebugOptions();

  if (enableColor) {
    notdec::llvm2c::debug_print_color = true;
  }

  std::string inSuffix = getSuffix(inputFilename);
  llvm::LLVMContext Ctx;
  std::unique_ptr<Module> module;
  if (inSuffix.size() == 0) {
    std::cout << "no extension for input file. exiting." << std::endl;
    return 0;
  } else if (inSuffix == ".ll" || inSuffix == ".bc") {
    std::cout << "Loading LLVM IR: " << inputFilename << std::endl;
    SMDiagnostic Err;
    module = parseIRFile(inputFilename, Err, Ctx);
    // TODO: enable optimization?
    if (module.get() == nullptr) {
      Err.print("IR parsing failed: ", errs());
      return 0;
    }
  } else {
    std::cout << "unknown extension " << inSuffix << " for input file. exiting."
              << std::endl;
    return 0;
  }

  if (!disablePass) {
    // demote SSA using reg2mem
    notdec::llvm2c::demoteSSA(*module);
  }

  std::string outSuffix = getSuffix(outputFilename);
  if (outSuffix == ".ll") {
    std::error_code EC;
    llvm::raw_fd_ostream os(outputFilename, EC);
    if (EC) {
      std::cerr << "Cannot open output file." << std::endl;
      std::cerr << EC.message() << std::endl;
      std::abort();
    }
    module->print(os, nullptr);
    std::cout << "IR dumped to " << outputFilename << std::endl;
  } else if (outSuffix == ".c") {
    std::error_code EC;
    llvm::raw_fd_ostream os(outputFilename, EC);
    if (EC) {
      std::cerr << "Cannot open output file." << std::endl;
      std::cerr << EC.message() << std::endl;
      std::abort();
    }
    notdec::llvm2c::decompileModule(*module, os);
    std::cout << "Decompilation result: " << outputFilename << std::endl;
  }

  return 0;
}
