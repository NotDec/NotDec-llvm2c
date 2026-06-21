add_library(notdec-backend-structuring STATIC
    StructuredCFG.cpp
    Region.cpp
    RegionOverlay.cpp
    RegionIdentifier.cpp
    MutableRegionGraph.cpp
    RecursiveStructurer.cpp
    StructurerRegistry.cpp
    StructuringEvaluator.cpp
    StructuringOptimizationPass.cpp
    StructuringOptimizationPipeline.cpp
    StructuringQuality.cpp
    GotoManager.cpp
    GotoStructurer.cpp
    PhoenixStructurer.cpp
    SAILRDeoptimization.cpp
    SAILRStructurer.cpp
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
