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
  applyFunctionNameAndParams(F.getName(), Result);
  Result.Visibility = "public";
  Result.Body.push_back("// TODO: recover body");
  return Result;
}

void Reader::applyFunctionNameAndParams(llvm::StringRef IRName,
                                        Function &Result) {
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
    Result.Name = ("public_" + Selector).str();
    return;
  }
  if (Name.empty()) {
    Result.Name = "public_unknown";
    return;
  }

  llvm::SmallVector<llvm::StringRef, 4> Parts;
  Name.split(Parts, '_', -1, false);
  std::size_t ParamStart = Parts.size();
  while (ParamStart > 0 && isKnownAbiType(Parts[ParamStart - 1])) {
    --ParamStart;
  }

  std::string BaseName;
  for (std::size_t I = 0; I < ParamStart; ++I) {
    if (I != 0) {
      BaseName += "_";
    }
    BaseName += Parts[I].str();
  }
  Result.Name = sanitizeIdentifier(BaseName);

  std::string EncodedParams;
  for (std::size_t I = ParamStart; I < Parts.size(); ++I) {
    if (I != ParamStart) {
      EncodedParams += "_";
    }
    EncodedParams += Parts[I].str();
  }
  Result.Parameters = parseAbiParameters(EncodedParams);
}

std::vector<Parameter> Reader::parseAbiParameters(llvm::StringRef Encoded) {
  std::vector<Parameter> Result;
  llvm::SmallVector<llvm::StringRef, 4> Parts;
  Encoded.split(Parts, '_', -1, false);
  for (llvm::StringRef Part : Parts) {
    if (!isKnownAbiType(Part)) {
      continue;
    }
    Result.push_back(Parameter{Part.str(),
                               "arg" + std::to_string(Result.size())});
  }
  return Result;
}

bool Reader::isKnownAbiType(llvm::StringRef Type) {
  if (Type == "address" || Type == "bool" || Type == "string" ||
      Type == "bytes") {
    return true;
  }
  if (Type.consume_front("uint") || Type.consume_front("int")) {
    if (Type.empty()) {
      return true;
    }
    return llvm::all_of(Type, [](char C) { return std::isdigit(C); });
  }
  if (Type.consume_front("bytes")) {
    return !Type.empty() &&
           llvm::all_of(Type, [](char C) { return std::isdigit(C); });
  }
  return false;
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
