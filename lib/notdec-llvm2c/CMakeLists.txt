include(AddLLVM)
add_library(notdec-llvm2c SHARED STATIC
    ASTManager.cpp
    StructuralAnalysis.cpp
    Phoenix.cpp
    CFG.cpp
    Goto.cpp
    Utils.cpp
    PostOrderCFGView.cpp
    Dominators.cpp
    CompoundConditionBuilder.cpp
    TypeManager.cpp
    Interface/StructManager.cpp
    Interface/ExtValuePtr.cpp
    Interface/ValueNamer.cpp
    Interface/HType.cpp
    Range.cpp
    ASTPrinter/StmtPrinter.cpp
    ASTPrinter/DeclPrinter.cpp
    ASTPrinter/TypePrinter.cpp
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
