add_library(notdec-backend-solidity STATIC
    SolidityBackend.cpp
)

target_link_libraries(notdec-backend-solidity
    PUBLIC
    notdec-backend-core
    notdec-backend-structuring
)

target_include_directories(notdec-backend-solidity
    PUBLIC
    ../../include
)

set_target_properties(notdec-backend-solidity
    PROPERTIES
        OUTPUT_NAME "notdec-backend-solidity"
)
