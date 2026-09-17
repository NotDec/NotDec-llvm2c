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

namespace {

std::optional<llvm::APInt> constantIntValue(const llvm::Value *V) {
  if (const auto *Int = llvm::dyn_cast_or_null<llvm::ConstantInt>(V)) {
    return Int->getValue();
  }
  return std::nullopt;
}

std::optional<llvm::APInt> constantIntToPtrValue(const llvm::Value *V) {
  if (const auto *Inst = llvm::dyn_cast_or_null<llvm::IntToPtrInst>(V)) {
    return constantIntValue(Inst->getOperand(0));
  }
  if (const auto *Expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(V);
      Expr != nullptr && Expr->getOpcode() == llvm::Instruction::IntToPtr &&
      Expr->getNumOperands() == 1) {
    return constantIntValue(Expr->getOperand(0));
  }
  return std::nullopt;
}

bool isAbiBoolWord(const llvm::Value *V);

bool isEvmCallerValue(const llvm::Value *V) {
  const auto *Call = llvm::dyn_cast_or_null<llvm::CallBase>(V);
  return Call != nullptr && Call->getCalledFunction() != nullptr &&
         Call->getCalledFunction()->getName() == "evm_caller";
}

std::vector<Parameter> eventTopicParameters(const llvm::CallBase &Call) {
  std::vector<Parameter> Params;
  if (Call.arg_size() <= 4) {
    return Params;
  }

  for (unsigned Arg = 4; Arg < Call.arg_size(); ++Arg) {
    const llvm::Value *Topic = Call.getArgOperand(Arg);
    std::string Type = "uint256";
    if (isEvmCallerValue(Topic)) {
      Type = "address";
    } else if (isAbiBoolWord(Topic)) {
      Type = "bool";
    }
    Params.push_back(Parameter{TypeRef{Type},
                               "arg" + std::to_string(Params.size()),
                               /*DataLocation=*/"",
                               /*Indexed=*/true});
  }
  return Params;
}

bool isFreeMemoryPointerInit(const llvm::Instruction &I) {
  const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&I);
  if (Store == nullptr) {
    return false;
  }
  std::optional<llvm::APInt> Pointer =
      constantIntToPtrValue(Store->getPointerOperand());
  std::optional<llvm::APInt> Value = constantIntValue(Store->getValueOperand());
  return Pointer.has_value() && Value.has_value() && *Pointer == 64 &&
         *Value == 128;
}

bool isDemoteSSAAllocaPoint(const llvm::Instruction &I) {
  return llvm::isa<llvm::BitCastInst>(I) &&
         I.getName() == "reg2mem alloca point";
}

std::optional<std::uint64_t> parseStorageSlotName(llvm::StringRef Name) {
  if (!Name.consume_front("slot:") && !Name.consume_front("slot_")) {
    return std::nullopt;
  }
  std::uint64_t Value = 0;
  bool SawDigit = false;
  for (char C : Name) {
    if (!std::isdigit(static_cast<unsigned char>(C))) {
      break;
    }
    SawDigit = true;
    Value = Value * 10 + static_cast<std::uint64_t>(C - '0');
  }
  if (!SawDigit) {
    return std::nullopt;
  }
  return Value;
}

bool isEmptyPayableFallbackSelector(const llvm::Function &F) {
  if (F.isDeclaration() ||
      !F.getName().starts_with("public___function_selector")) {
    return false;
  }
  if (F.size() != 1) {
    return false;
  }

  bool SawMemoryInit = false;
  for (const llvm::Instruction &I : F.front()) {
    if (llvm::isa<llvm::ReturnInst>(I)) {
      continue;
    }
    if (isDemoteSSAAllocaPoint(I)) {
      continue;
    }
    if (isFreeMemoryPointerInit(I)) {
      SawMemoryInit = true;
      continue;
    }
    return false;
  }
  return SawMemoryInit;
}

bool isAbiBoolWord(const llvm::Value *V) {
  if (V == nullptr) {
    return false;
  }
  if (V->getType()->isIntegerTy(1)) {
    return true;
  }
  const auto *Cast = llvm::dyn_cast<llvm::ZExtInst>(V);
  return Cast != nullptr && Cast->getSrcTy()->isIntegerTy(1);
}

std::optional<std::string> signedReturnTypeForWord(const llvm::Value *V) {
  const auto *Call = llvm::dyn_cast_or_null<llvm::CallBase>(V);
  if (Call == nullptr || Call->getCalledFunction() == nullptr) {
    return std::nullopt;
  }
  llvm::StringRef Name = Call->getCalledFunction()->getName();
  if (Name == "evm_sdiv" || Name == "evm_smod" || Name == "evm_sar") {
    return "int256";
  }
  if (Name != "evm_signextend" || Call->arg_size() < 2) {
    return std::nullopt;
  }
  std::optional<llvm::APInt> ByteIndex =
      constantIntValue(Call->getArgOperand(0));
  if (!ByteIndex.has_value() || ByteIndex->ugt(31)) {
    return std::nullopt;
  }
  return "int" + std::to_string((ByteIndex->getZExtValue() + 1) * 8);
}

const llvm::Value *ptrToIntSource(const llvm::Value *V) {
  if (const auto *Inst = llvm::dyn_cast_or_null<llvm::PtrToIntInst>(V)) {
    return Inst->getOperand(0);
  }
  if (const auto *Expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(V);
      Expr != nullptr && Expr->getOpcode() == llvm::Instruction::PtrToInt &&
      Expr->getNumOperands() == 1) {
    return Expr->getOperand(0);
  }
  return nullptr;
}

// MemoryBufferAnalysis rewrites dynamic one-word ABI returns to calloc-backed
// pointers.  In that shape the evm_return offset is an SSA value, so the stored
// return word has to be found by following the returned pointer instead of a
// constant offset.
const llvm::Value *findAllocatedSingleWordReturnValue(
    const llvm::CallBase &Call) {
  const llvm::Value *StorePointer = ptrToIntSource(Call.getArgOperand(1));
  if (StorePointer == nullptr) {
    return nullptr;
  }
  for (auto It = llvm::BasicBlock::const_iterator(&Call), Begin =
                                                      Call.getParent()->begin();
       It != Begin;) {
    --It;
    const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&*It);
    if (Store != nullptr && Store->getPointerOperand() == StorePointer) {
      return Store->getValueOperand();
    }
  }
  return nullptr;
}

const llvm::Value *findStoredReturnValue(const llvm::CallBase &Call,
                                         const llvm::APInt &ReturnOffset) {
  for (auto It = llvm::BasicBlock::const_iterator(&Call), Begin =
                                                      Call.getParent()->begin();
       It != Begin;) {
    --It;
    const auto *Store = llvm::dyn_cast<llvm::StoreInst>(&*It);
    if (Store == nullptr) {
      continue;
    }
    std::optional<llvm::APInt> StoreOffset =
        constantIntToPtrValue(Store->getPointerOperand());
    if (StoreOffset.has_value() && *StoreOffset == ReturnOffset) {
      return Store->getValueOperand();
    }
  }
  return nullptr;
}

bool returnsSingleBoolWord(const llvm::Function &F) {
  bool SawReturn = false;
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
      std::optional<llvm::APInt> ReturnOffset =
          constantIntValue(Call->getArgOperand(1));
      std::optional<llvm::APInt> ReturnLength =
          constantIntValue(Call->getArgOperand(2));
      if (!ReturnLength.has_value() || *ReturnLength != 32) {
        return false;
      }

      // A Solidity bool is ABI-encoded as one 32-byte word whose value is
      // produced from an i1 comparison result.
      const llvm::Value *Stored =
          ReturnOffset.has_value()
              ? findStoredReturnValue(*Call, *ReturnOffset)
              : findAllocatedSingleWordReturnValue(*Call);
      if (!isAbiBoolWord(Stored)) {
        return false;
      }
      SawReturn = true;
    }
  }
  return SawReturn;
}

std::optional<std::string> singleSignedReturnType(const llvm::Function &F) {
  std::optional<std::string> SignedType;
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
      std::optional<llvm::APInt> ReturnOffset =
          constantIntValue(Call->getArgOperand(1));
      std::optional<llvm::APInt> ReturnLength =
          constantIntValue(Call->getArgOperand(2));
      if (!ReturnOffset.has_value() || !ReturnLength.has_value() ||
          *ReturnLength != 32) {
        return std::nullopt;
      }

      const llvm::Value *Stored = findStoredReturnValue(*Call, *ReturnOffset);
      std::optional<std::string> CurrentType = signedReturnTypeForWord(Stored);
      if (!CurrentType.has_value()) {
        return std::nullopt;
      }
      if (SignedType.has_value() && *SignedType != *CurrentType) {
        return std::nullopt;
      }
      SignedType = *CurrentType;
    }
  }
  return SignedType;
}

} // namespace

SourceUnit Reader::read(const llvm::Module &M,
                        const ::notdec::llvm2c::HTypeResult *HT) {
  SourceUnit Unit;
  Unit.Contracts.push_back(readContract(M, HT));
  return Unit;
}

Contract Reader::readContract(const llvm::Module &M,
                              const ::notdec::llvm2c::HTypeResult *HT) {
  Contract Result;
  StorageSlotMap StorageSlots;
  readEvents(M, Result);
  EventParamTypeMap EventParamTypes;
  for (const EventDecl &Event : Result.Events) {
    std::vector<std::string> Types;
    Types.reserve(Event.Parameters.size());
    for (const Parameter &Param : Event.Parameters) {
      Types.push_back(Param.Type.Name);
    }
    EventParamTypes.emplace(Event.Name, std::move(Types));
  }
  if (HT != nullptr) {
    readStateVariables(*HT, Result, StorageSlots);
  }

  std::vector<const llvm::Function *> PublicFunctions;
  const llvm::Function *EmptyPayableFallback = nullptr;
  bool MultipleEmptyPayableFallbacks = false;
  for (const llvm::Function &F : M.functions()) {
    if (isPublicEntryFunction(F)) {
      PublicFunctions.push_back(&F);
      continue;
    }
    if (isEmptyPayableFallbackSelector(F)) {
      if (EmptyPayableFallback != nullptr) {
        MultipleEmptyPayableFallbacks = true;
      } else {
        EmptyPayableFallback = &F;
      }
    }
  }
  std::sort(PublicFunctions.begin(), PublicFunctions.end(),
            [](const llvm::Function *LHS, const llvm::Function *RHS) {
              return LHS->getName() < RHS->getName();
            });

  for (const llvm::Function *F : PublicFunctions) {
    Result.Functions.push_back(
        readFunction(*F, &StorageSlots, &EventParamTypes));
  }
  // Two outlined entries can recover the same selector/base name.  Keep the
  // generated module compilable by disambiguating exact signature duplicates.
  {
    std::map<std::string, unsigned> SeenSignatures;
    for (Function &Fn : Result.Functions) {
      std::string Signature = Fn.Name + "(";
      for (const Parameter &Param : Fn.Parameters) {
        Signature += Param.Type.Name;
        Signature += ",";
        Signature += Param.DataLocation;
        Signature += ";";
      }
      Signature += ")";
      unsigned &Count = SeenSignatures[Signature];
      ++Count;
      if (Count > 1) {
        Fn.Name += "_" + std::to_string(Count);
      }
    }
  }
  if (PublicFunctions.empty() && EmptyPayableFallback != nullptr &&
      !MultipleEmptyPayableFallbacks) {
    Function Fallback;
    Fallback.Name = "fallback";
    Fallback.Visibility = "public payable";
    Fallback.Body = Block{};
    Result.Functions.push_back(std::move(Fallback));
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
        std::vector<Parameter> Params = eventTopicParameters(*Call);
        auto Existing = std::find_if(
            Result.Events.begin(), Result.Events.end(),
            [&](const EventDecl &Event) { return Event.Name == *Name; });
        if (Existing != Result.Events.end()) {
          if (Existing->Parameters.size() == Params.size()) {
            for (std::size_t I = 0; I < Params.size(); ++I) {
              std::string &OldType = Existing->Parameters[I].Type.Name;
              if (OldType != Params[I].Type.Name) {
                if (OldType == "address" || Params[I].Type.Name == "address") {
                  OldType = "address";
                } else if (OldType == "uint256" ||
                           Params[I].Type.Name == "uint256") {
                  OldType = "uint256";
                } else {
                  OldType = "bool";
                }
              }
            }
          }
          continue;
        }
        Names.push_back(*Name);
        Result.Events.push_back(EventDecl{*Name, std::move(Params)});
      }
    }
  }
}

void Reader::readStateVariables(const ::notdec::llvm2c::HTypeResult &HT,
                                Contract &Result,
                                StorageSlotMap &StorageSlots) {
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
    Var.Type = TypeRef{TypePrinter::formatStateVariableType(Field.Type)};
    Var.Name = UniqueName;
    Var.Visibility = "public";
    if (std::optional<std::uint64_t> Slot =
            parseStorageSlotName(Field.Name)) {
      StorageSlots[*Slot] =
          StorageSlotInfo{UniqueName, TypePrinter::isMappingType(Field.Type),
                          TypePrinter::isArrayType(Field.Type)};
    }
    Result.StateVariables.push_back(std::move(Var));
    ++Index;
  }
}

bool Reader::isPublicEntryFunction(const llvm::Function &F) {
  llvm::StringRef Name = F.getName();
  return !F.isDeclaration() && Name.starts_with("public_") &&
         !Name.contains("function_selector");
}

Function Reader::readFunction(const llvm::Function &F,
                               const StorageSlotMap *StorageSlots,
                               const EventParamTypeMap *EventParamTypes) {
  Function Result;
  applyFunctionNameAndParams(F.getName(), Result);

  // Public wrappers always start with (mem, calldata, returndata, env).  Some
  // outlined entry functions additionally carry the outlined private formals
  // as trailing i256 arguments; expose them as Solidity parameters so body
  // expressions do not leak raw LLVM names.
  std::vector<std::string> ArgumentNames(F.arg_size());
  constexpr unsigned kRuntimeArgs = 4;
  const unsigned ExtraArgs =
      F.arg_size() > kRuntimeArgs ? F.arg_size() - kRuntimeArgs : 0;
  for (unsigned I = 0; I < ExtraArgs; ++I) {
    // ABI parameter names parsed from the function name already cover the
    // leading trailing args; only synthesize parameters for truly outlined
    // formals without an ABI spelling.
    std::string Name;
    if (I < Result.Parameters.size()) {
      Name = Result.Parameters[I].Name;
    } else {
      Name = "arg" + std::to_string(I);
      Result.Parameters.push_back(
          Parameter{TypeRef{"uint256"}, Name, /*DataLocation=*/"", false});
    }
    ArgumentNames[kRuntimeArgs + I] = std::move(Name);
  }

  Result.Visibility = "public";
  Result.Returns = readReturns(F);
  Result.Body = readBody(F, StorageSlots, &ArgumentNames, EventParamTypes);
  return Result;
}

Block Reader::readBody(const llvm::Function &F,
                       const StorageSlotMap *StorageSlots,
                       const std::vector<std::string> *ArgumentNames,
                       const EventParamTypeMap *EventParamTypes) {
  return BodyBuilder::readBody(F, StorageSlots, ArgumentNames, EventParamTypes);
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
  std::string ReturnType = "uint256";
  if (Count == 1 && returnsSingleBoolWord(F)) {
    ReturnType = "bool";
  } else if (Count == 1) {
    if (std::optional<std::string> SignedType = singleSignedReturnType(F)) {
      ReturnType = *SignedType;
    }
  }
  for (std::uint64_t I = 0; I < Count; ++I) {
    Result.push_back(Parameter{TypeRef{ReturnType},
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
    Parameter Param{TypeRef{Part.str()},
                    "arg" + std::to_string(Result.size())};
    // Dynamic ABI parameters need an explicit data location on function
    // signatures.  Public/external inputs come from calldata.
    if (Part == "string" || Part == "bytes") {
      Param.DataLocation = "calldata";
    }
    Result.push_back(std::move(Param));
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
