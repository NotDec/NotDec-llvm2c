include(AddLLVM)
add_library(notdec-llvm2c SHARED STATIC
    StructuralAnalysis.cpp
    Phoenix.cpp
    CFG.cpp
    Goto.cpp
    Utils.cpp
    PostOrderCFGView.cpp
    Dominators.cpp
    CompoundConditionBuilder.cpp
    ASTPrinter/StmtPrinter.cpp
    ASTPrinter/DeclPrinter.cpp
)

target_link_libraries(notdec-llvm2c
    PUBLIC
    LLVMCore LLVMIRReader LLVMSupport LLVMPasses
    clangAST clangTooling
)

target_include_directories(notdec-llvm2c
    PUBLIC
    ../../include
)

set_target_properties(notdec-llvm2c
    PROPERTIES
        OUTPUT_NAME "notdec-llvm2c"
)
