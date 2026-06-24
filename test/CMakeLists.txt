
find_package(Python3 REQUIRED COMPONENTS Interpreter)

add_executable(phi-demote-test
  phi_demote_test.cpp
)
target_link_libraries(phi-demote-test
  PRIVATE
  notdec-backend-c
)
add_test(NAME phi-demote
  COMMAND phi-demote-test
)

add_test(NAME phi-demote-htypes
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_LIST_DIR}/phi_demote_htypes_smoke.py
          --notdec $<TARGET_FILE:notdec>
)

add_subdirectory(sysy)
add_subdirectory(structuring)
