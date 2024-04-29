add_executable(notdec-llvm2c-exe
  notdec-llvm2c.cpp
)

target_link_libraries(notdec-llvm2c-exe 
  notdec-llvm2c
)

set_target_properties(notdec-llvm2c-exe
	PROPERTIES
		OUTPUT_NAME "notdec-llvm2c"
)

install(TARGETS notdec-llvm2c-exe DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
