#include "notdec-llvm2c/Interface/StructManager.h"

#include <cassert>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Expr.h>
#include <cstddef>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/JSON.h>
#include <memory>
#include <string>
#include <vector>

namespace notdec {

const char *TypeInfo::getTypeStr() const {
  if (isSimple()) {
    return "simple";
  } else if (isStruct()) {
    return "struct";
  } else if (isUnion()) {
    return "union";
  } else if (isArray()) {
    return "array";
  } else {
    assert(false);
  }
}

void TypeInfo::fixEdge(
    const std::map<const retypd::CGEdge *, const retypd::CGEdge *> &EdgeMap) {
  if (auto UI = getAs<UnionInfo>()) {
    for (size_t i = 0; i < UI->Members.size(); i++) {
      auto Edge = UI->Members[i];
      auto it = EdgeMap.find(Edge);
      if (it != EdgeMap.end()) {
        UI->Members[i] = it->second;
      }
    }
  } else if (auto SI = getAs<StructInfo>()) {
    for (auto &Field : SI->Fields) {
      if (Field.Edge) {
        auto it = EdgeMap.find(Field.Edge);
        if (it != EdgeMap.end()) {
          Field.Edge = it->second;
        }
      }
    }
  } else if (auto AI = getAs<ArrayInfo>()) {
    if (AI->Edge) {
      auto it = EdgeMap.find(AI->Edge);
      if (it != EdgeMap.end()) {
        AI->Edge = it->second;
      }
    }
  } else if (auto TI = getAs<SimpleTypeInfo>()) {
    if (TI->Edge) {
      auto it = EdgeMap.find(TI->Edge);
      if (it != EdgeMap.end()) {
        TI->Edge = it->second;
      }
    }
  } else {
    assert(false && "Unknown TypeInfo");
  }
}

std::optional<std::string> BytesManager::getRange(SimpleRange R) {
  if (R.Size < 0) {
    return std::nullopt;
  }

  OffsetTy reqStart = R.Start;
  OffsetTy reqEnd = R.Start + R.Size;

  // Handle size 0 case
  if (R.Size == 0) {
    for (const auto &entry : Bytes) {
      OffsetTy elemStart = entry.first.Start;
      OffsetTy elemEnd = elemStart + entry.first.Size;
      if (elemStart > reqStart) {
        break;
      }
      if (elemEnd > reqStart) {
        return std::string();
      }
    }
    return std::nullopt;
  }

  std::string result;
  OffsetTy currentPos = reqStart;

  for (const auto &entry : Bytes) {
    const auto &elemRange = entry.first;
    OffsetTy elemStart = elemRange.Start;
    OffsetTy elemEnd = elemStart + elemRange.Size;

    // Since elements are sorted by Start, break early if possible
    if (elemStart >= reqEnd) {
      break;
    }

    // Skip elements that end before currentPos
    if (elemEnd <= currentPos) {
      continue;
    }

    // Check for a gap between currentPos and this element's start
    if (elemStart > currentPos) {
      llvm::errs() << "BytesManager::getRange: a gap!\n";
      return std::nullopt;
    }

    // Calculate the overlapping part for this element
    OffsetTy overlapEnd = std::min(elemEnd, reqEnd);
    OffsetTy dataOffset = currentPos - elemStart;
    OffsetTy dataLength = overlapEnd - currentPos;

    // Append the corresponding substring
    result.append(entry.second.substr(dataOffset, dataLength));

    // Move current position forward
    currentPos = overlapEnd;

    // Early exit if we've covered the entire range
    if (currentPos >= reqEnd) {
      break;
    }
  }

  // Check if the entire range was covered
  if (currentPos >= reqEnd) {
    return result;
  } else {
    return std::nullopt;
  }
}

BytesManager BytesManager::getSubBytes(int64_t Start, int64_t End) {
  BytesManager BM;
  for (auto &Ent : Bytes) {
    assert(Ent.first.Size == Ent.second.size());
    auto EntEnd = Ent.first.Start + Ent.first.Size;
    auto innerStart = std::max(Start, Ent.first.Start);
    auto innerEnd = std::min(End, EntEnd);
    if (innerStart >= innerEnd) {
      continue;
    }
    auto Offset = innerStart - Ent.first.Start;
    auto Size = innerEnd - innerStart;
    auto Data = Ent.second.substr(Offset, Size);

    BM.Bytes.push_back({SimpleRange{.Start = innerStart - Start,
                                    .Size = innerEnd - innerStart},
                        Data});
  }
  return BM;
}

std::shared_ptr<BytesManager> BytesManager::create(llvm::Module &M) {
  std::shared_ptr<BytesManager> BM = std::make_shared<BytesManager>();
  for (auto &G : M.getGlobalList()) {
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

llvm::StringRef decodeCStr1(llvm::StringRef Bytes, int64_t Offset) {
  auto Start = Bytes.data() + Offset;
  auto End = Bytes.data() + Bytes.size();
  auto *Ptr = Start;
  while (Ptr < End && *Ptr != '\0') {
    Ptr++;
  }
  if (*Ptr == '\0') {
    Ptr++;
  }
  return llvm::StringRef(Start, Ptr - Start);
}

std::string BytesManager::decodeCStr(int64_t Offset) {
  for (auto &Ent : Bytes) {
    if (Ent.first.containsOffset(Offset)) {
      auto innerOffset = Offset - Ent.first.Start;
      return decodeCStr1(Ent.second, innerOffset).str();
    }
  }
  return "";
}

/*
  "printf": {
    "constraints": [
      "printf.in_0 <= cstr",
      "cstr.load8 <= #char",
      "printf.out <= #sint"
    ],
    "pni_map": {
      "printf": "func p #1",
      "printf.in_0": "ptr p #2",
      "cstr": "ptr p #2",
      "cstr.load8": "int 8 #3",
      "#char": "int 8 #3",
      "printf.out": "int 32 #4",
      "#sint": "int 32 #4",
      "#double": "float 64 #5"
    }
  },
*/

} // namespace notdec
