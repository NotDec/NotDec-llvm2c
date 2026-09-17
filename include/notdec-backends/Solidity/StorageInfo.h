#ifndef NOTDEC_BACKENDS_SOLIDITY_STORAGEINFO_H
#define NOTDEC_BACKENDS_SOLIDITY_STORAGEINFO_H

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace notdec::backend::solidity {

// Minimal contract-level storage slot information used by the Solidity body
// builder to turn evm.storage.* helper results back into readable expressions.
struct StorageSlotInfo {
  std::string Name;
  bool IsMapping = false;
  bool IsArray = false;
};

// Top-level storage slot number -> generated Solidity state variable.
using StorageSlotMap = std::map<std::uint64_t, StorageSlotInfo>;

// Event name -> recovered parameter type strings, used to insert the right
// address/uint256 casts around topic expressions.
using EventParamTypeMap =
    std::map<std::string, std::vector<std::string>>;

// Solidity parameter name -> recovered ABI type, used by body expression
// recovery for address/bytes coercion decisions.
using ParameterTypeMap = std::map<std::string, std::string>;

} // namespace notdec::backend::solidity

#endif
