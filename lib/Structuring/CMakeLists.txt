add_library(notdec-backend-structuring STATIC
    StructuredCFG.cpp
    GotoStructurer.cpp
    LLVMFunctionCFGBuilder.cpp
)

target_link_libraries(notdec-backend-structuring
    PUBLIC
    notdec-backend-core
)

target_include_directories(notdec-backend-structuring
    PUBLIC
    ../../include
)

set_target_properties(notdec-backend-structuring
    PROPERTIES
        OUTPUT_NAME "notdec-backend-structuring"
)
