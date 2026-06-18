#ifndef NOTDEC_BACKENDS_SOLIDITY_TYPEPRINTER_H
#define NOTDEC_BACKENDS_SOLIDITY_TYPEPRINTER_H

#include <string>

#include "notdec-backends/Core/HType.h"

namespace notdec::backend::solidity {

class TypePrinter {
public:
  static std::string formatType(const notdec::ast::HType *Ty);
};

} // namespace notdec::backend::solidity

#endif
