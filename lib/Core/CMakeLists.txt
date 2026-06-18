add_library(notdec-backend-core STATIC
    ../notdec-llvm2c/Interface/StructManager.cpp
    ../notdec-llvm2c/Interface/ExtValuePtr.cpp
    ../notdec-llvm2c/Interface/ValueNamer.cpp
    ../notdec-llvm2c/Interface/HType.cpp
    ../notdec-llvm2c/Range.cpp
)

if (NOTDEC_HAVE_SHARED_LLVM AND NOTDEC_HAVE_SHARED_CLANG)
    target_link_libraries(notdec-backend-core
        PUBLIC
        notdec_llvm_deps
        notdec_clang_deps
    )
else ()
    target_link_libraries(notdec-backend-core
        PUBLIC
        LLVMCore LLVMIRReader LLVMSupport
        clangAST
    )
endif ()

target_include_directories(notdec-backend-core
    PUBLIC
    ../../include
)

set_target_properties(notdec-backend-core
    PROPERTIES
        OUTPUT_NAME "notdec-backend-core"
)
