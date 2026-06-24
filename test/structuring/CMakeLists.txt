add_test(NAME structuring-smoke
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_structuring_smoke.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)

add_test(NAME legacy-phoenix-removed
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_legacy_phoenix_removed.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)

add_test(NAME structured-phoenix-available
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_structured_phoenix_available.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)

add_test(NAME shared-structurer-registry
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_shared_structurer_registry.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)

add_test(NAME sailr-bench2-migration
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_sailr_bench2_migration.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)

add_executable(structuring-analysis-test
  structuring_analysis_test.cpp
)
target_link_libraries(structuring-analysis-test
  PRIVATE
  notdec-backend-structuring
  notdec-backend-solidity
)

add_test(NAME structuring-analysis
  COMMAND structuring-analysis-test
)
