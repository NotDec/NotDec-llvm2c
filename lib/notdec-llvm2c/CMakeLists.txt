include(AddLLVM)
add_library(notdec-llvm2c SHARED STATIC
    structural-analysis.cpp
    phoenix.cpp
    CFG.cpp
    goto.cpp
    utils.cpp
    PostOrderCFGView.cpp
    Dominators.cpp
    CompoundConditionBuilder.cpp
)

target_link_libraries(notdec-llvm2c
    PUBLIC
    LLVMCore LLVMIRReader LLVMSupport LLVMPasses
    clangAST clangTooling
)

set_target_properties(notdec-llvm2c
    PROPERTIES
        OUTPUT_NAME "notdec-llvm2c"
)
