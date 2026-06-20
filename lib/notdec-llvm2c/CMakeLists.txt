include(AddLLVM)
add_library(notdec-backend-c SHARED STATIC
    ASTManager.cpp
    StructuralAnalysis.cpp
    CFG.cpp
    Goto.cpp
    StructuredGoto.cpp
    Utils.cpp
    PostOrderCFGView.cpp
    Dominators.cpp
    CompoundConditionBuilder.cpp
    TypeManager.cpp
    ASTPrinter/StmtPrinter.cpp
    ASTPrinter/DeclPrinter.cpp
    ASTPrinter/TypePrinter.cpp
)

add_library(notdec-llvm2c INTERFACE)
target_link_libraries(notdec-llvm2c
    INTERFACE
    notdec-backend-c
)

if (NOTDEC_HAVE_SHARED_LLVM AND NOTDEC_HAVE_SHARED_CLANG)
    target_link_libraries(notdec-backend-c
        PUBLIC
        notdec-backend-core
        notdec-backend-structuring
        notdec_llvm_deps
        notdec_clang_deps
    )
else ()
    target_link_libraries(notdec-backend-c
        PUBLIC
        notdec-backend-core
        notdec-backend-structuring
        LLVMCore LLVMIRReader LLVMSupport LLVMPasses
        clangAST clangTooling
    )
endif ()

target_include_directories(notdec-backend-c
    PUBLIC
    ../../include
)

set_target_properties(notdec-backend-c
    PROPERTIES
        OUTPUT_NAME "notdec-llvm2c"
)
