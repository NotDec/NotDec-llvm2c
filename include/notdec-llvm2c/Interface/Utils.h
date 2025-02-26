#ifndef _NOTDEC_INTERFACE_UTILS_H_
#define _NOTDEC_INTERFACE_UTILS_H_

#include <llvm/IR/Value.h>
#include <sstream>
namespace notdec {

inline bool hasUser(const llvm::Value *Val, const llvm::User *User) {
  for (auto U : Val->users()) {
    if (U == User) {
      return true;
    }
  }
  return false;
}

template <typename T> std::string int_to_hex(T i) {
  std::stringstream stream;
  stream << "0x"
         //  << std::setfill ('0') << std::setw(sizeof(T)*2)
         << std::hex << i;
  return stream.str();
}

} // namespace notdec

#endif
