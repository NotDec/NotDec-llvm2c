#include "notdec-backends/Solidity/Reader.h"
#include "notdec-backends/Solidity/BodyBuilder.h"
#include "notdec-backends/Solidity/TypePrinter.h"

#include <algorithm>
#include <cctype>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

namespace notdec::backend::solidity {

SourceUnit Reader::read(const llvm::Module &M,
                        const ::notdec::llvm2c::HTypeResult *HT) {
  SourceUnit Unit;
  Unit.Contracts.push_back(readContract(M, HT));
  return Unit;
}

Contract Reader::readContract(const llvm::Module &M,
                              const ::notdec::llvm2c::HTypeResult *HT) {
  Contract Result;
  readEvents(M, Result);
  if (HT != nullptr) {
    readStateVariables(*HT, Result);
  }

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

void Reader::readEvents(const llvm::Module &M, Contract &Result) {
  std::vector<std::string> Names;
  for (const llvm::Function &F : M.functions()) {
    if (F.isDeclaration()) {
      continue;
    }
    for (const llvm::BasicBlock &BB : F) {
      for (const llvm::Instruction &I : BB) {
        const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I);
        if (Call == nullptr) {
          continue;
        }
        std::optional<std::string> Kind =
            BodyBuilder::getStringMetadata(I, "notdec.solidity.event");
        if (!Kind.has_value()) {
          continue;
        }
        std::optional<std::string> Name = BodyBuilder::getEventName(I, *Kind);
        if (!Name.has_value() || Name->empty()) {
          continue;
        }
        if (std::find(Names.begin(), Names.end(), *Name) != Names.end()) {
          continue;
        }
        Names.push_back(*Name);
        Result.Events.push_back(EventDecl{*Name, {}});
      }
    }
  }
}

void Reader::readStateVariables(const ::notdec::llvm2c::HTypeResult &HT,
                                Contract &Result) {
  if (HT.StorageDecl == nullptr) {
    return;
  }

  unsigned Index = 0;
  std::vector<std::string> Names;
  for (const auto &Field : HT.StorageDecl->getFields()) {
    if (Field.isPadding) {
      continue;
    }
    std::string Name = Field.Name.empty()
                           ? "storage_" + std::to_string(Index)
                           : sanitizeIdentifier(Field.Name);
    if (Name.empty()) {
      Name = "storage_" + std::to_string(Index);
    }

    std::string UniqueName = Name;
    unsigned Collision = 1;
    while (std::find(Names.begin(), Names.end(), UniqueName) != Names.end()) {
      UniqueName = Name + "_" + std::to_string(Collision++);
    }
    Names.push_back(UniqueName);
    StateVariable Var;
    Var.Type = TypeRef{TypePrinter::formatType(Field.Type)};
    Var.Name = UniqueName;
    Var.Visibility = "public";
    Result.StateVariables.push_back(std::move(Var));
    ++Index;
  }
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
  Result.Returns = readReturns(F);
  Result.Body = readBody(F);
  return Result;
}

Block Reader::readBody(const llvm::Function &F) {
  Block Result;
  for (std::string Text : BodyBuilder::readBody(F)) {
    Result.Statements.push_back(RawStatement{std::move(Text)});
  }
  return Result;
}

std::vector<Parameter> Reader::readReturns(const llvm::Function &F) {
  std::optional<std::uint64_t> StaticReturnBytes;
  for (const llvm::BasicBlock &BB : F) {
    for (const llvm::Instruction &I : BB) {
      const auto *Call = llvm::dyn_cast<llvm::CallBase>(&I);
      if (Call == nullptr) {
        continue;
      }
      const llvm::Function *Callee = Call->getCalledFunction();
      if (Callee == nullptr || Callee->getName() != "evm_return" ||
          Call->arg_size() < 3) {
        continue;
      }
      const auto *Len = llvm::dyn_cast<llvm::ConstantInt>(Call->getArgOperand(2));
      if (Len == nullptr || Len->getValue().ugt(UINT64_MAX)) {
        continue;
      }
      std::uint64_t Bytes = Len->getZExtValue();
      if (Bytes == 0 || Bytes % 32 != 0) {
        continue;
      }
      if (StaticReturnBytes.has_value() && *StaticReturnBytes != Bytes) {
        return {};
      }
      StaticReturnBytes = Bytes;
    }
  }

  std::vector<Parameter> Result;
  if (!StaticReturnBytes.has_value()) {
    return Result;
  }
  std::uint64_t Count = *StaticReturnBytes / 32;
  for (std::uint64_t I = 0; I < Count; ++I) {
    Result.push_back(Parameter{TypeRef{"uint256"},
                               "ret" + std::to_string(I)});
  }
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
    Result.push_back(Parameter{TypeRef{Part.str()},
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
