
#include "notdec-llvm2c/Dominators.h"

namespace notdec::llvm2c {
template <> void CFGDominatorTreeImpl</*IsPostDom=*/true>::anchor() {}

template <> void CFGDominatorTreeImpl</*IsPostDom=*/false>::anchor() {}

} // namespace notdec::llvm2c
