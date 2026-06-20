add_test(NAME structuring-smoke
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/run_structuring_smoke.py
          --notdec-llvm2c $<TARGET_FILE:notdec-llvm2c-exe>
)
