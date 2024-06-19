#include <iostream>
#include <string>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>

#include "notdec-llvm2c/Interface.h"
#include "notdec-llvm2c/StructuralAnalysis.h"
#include "notdec-llvm2c/Utils.h"

using namespace llvm;
using namespace notdec::llvm2c;

#include "Commandlines.def"

static cl::opt<std::string>
    inputFilename(cl::Positional, cl::desc("<input file>"),
                  cl::value_desc("input LLVM IR file, either .ll or .bc path."),
                  cl::Required, cl::cat(NotdecLLVM2CCat));

static cl::opt<std::string> outputFilename("o", cl::desc("Specify output path"),
                                           cl::value_desc("output.c"),
                                           cl::Optional,
                                           cl::cat(NotdecLLVM2CCat));

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
    notdec::llvm2c::decompileModule(*module, os, getLLVM2COptions());
    std::cout << "Decompilation result: " << outputFilename << std::endl;
  }

  return 0;
}
