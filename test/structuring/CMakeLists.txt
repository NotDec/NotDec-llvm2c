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
