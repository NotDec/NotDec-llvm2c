#ifndef NOTDEC_BACKENDS_SOLIDITY_TYPEPRINTER_H
#define NOTDEC_BACKENDS_SOLIDITY_TYPEPRINTER_H

#include <string>

#include "notdec-backends/Core/HType.h"

namespace notdec::backend::solidity {

class TypePrinter {
public:
  static std::string formatType(const notdec::ast::HType *Ty);
  // State-variable declarations keep enough shape for helper expression
  // recovery (mapping key/value, dynamic array element).  Plain member types
  // still go through formatType().
  static std::string formatStateVariableType(const notdec::ast::HType *Ty);
  static bool isMappingType(const notdec::ast::HType *Ty);
  static bool isArrayType(const notdec::ast::HType *Ty);
};

} // namespace notdec::backend::solidity

#endif
