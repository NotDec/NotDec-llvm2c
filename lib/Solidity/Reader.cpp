#include "notdec-backends/Solidity/Reader.h"

#include <algorithm>
#include <cctype>
#include <string>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Function.h>

namespace notdec::backend::solidity {

SourceUnit Reader::read(const llvm::Module &M) {
  SourceUnit Unit;
  Unit.Contracts.push_back(readContract(M));
  return Unit;
}

Contract Reader::readContract(const llvm::Module &M) {
  Contract Result;

  std::vector<const llvm::Function *> PublicFunctions;
  for (const llvm::Function &F : M.functions()) {
    if (isPublicEntryFunction(F)) {
      PublicFunctions.push_back(&F);
    }
  }
  std::sort(PublicFunctions.begin(), PublicFunctions.end(),
            [](const llvm::Function *LHS, const llvm::Function *RHS) {
              return LHS->getName() < RHS->getName();
            });

  for (const llvm::Function *F : PublicFunctions) {
    Result.Functions.push_back(readFunction(*F));
  }

  return Result;
}

bool Reader::isPublicEntryFunction(const llvm::Function &F) {
  llvm::StringRef Name = F.getName();
  return !F.isDeclaration() && Name.starts_with("public_") &&
         !Name.contains("function_selector");
}

Function Reader::readFunction(const llvm::Function &F) {
  Function Result;
  Result.Name = getFunctionName(F.getName());
  Result.Visibility = "public";
  Result.Body.push_back("// TODO: recover body");
  return Result;
}

std::string Reader::getFunctionName(llvm::StringRef IRName) {
  llvm::StringRef Name = IRName;
  Name.consume_front("public_");

  std::size_t SuffixPos = Name.rfind("__0x");
  if (SuffixPos != llvm::StringRef::npos) {
    Name = Name.take_front(SuffixPos);
  }
  while (Name.ends_with("_")) {
    Name = Name.drop_back();
  }

  while (Name.starts_with("_")) {
    Name = Name.drop_front();
  }
  if (Name.starts_with("0x")) {
    llvm::StringRef Selector = Name.split('_').first;
    return ("public_" + Selector).str();
  }
  if (Name.empty()) {
    return "public_unknown";
  }
  return sanitizeIdentifier(Name);
}

std::string Reader::sanitizeIdentifier(llvm::StringRef Name) {
  std::string Result;
  Result.reserve(Name.size());
  for (char C : Name) {
    unsigned char UC = static_cast<unsigned char>(C);
    if (std::isalnum(UC) || C == '_') {
      Result.push_back(C);
    } else {
      Result.push_back('_');
    }
  }
  if (Result.empty()) {
    return "public_unknown";
  }
  if (std::isdigit(static_cast<unsigned char>(Result.front()))) {
    Result.insert(Result.begin(), '_');
  }
  return Result;
}

} // namespace notdec::backend::solidity
