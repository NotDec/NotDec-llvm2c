#include "notdec-llvm2c/Interface/HType.h"

namespace notdec {

// 静态成员定义
std::map<HTypeFactory::IntegerKey, std::unique_ptr<IntegerType>> HTypeFactory::integerTypes;
std::map<HTypeFactory::FloatKey, std::unique_ptr<FloatingType>> HTypeFactory::floatTypes;
std::map<HTypeFactory::PointerKey, std::unique_ptr<PointerType>> HTypeFactory::pointerTypes;
std::map<HTypeFactory::FunctionKey, std::unique_ptr<FunctionType>> HTypeFactory::functionTypes;

} // namespace notdec
