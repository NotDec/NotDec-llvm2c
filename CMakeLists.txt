cmake_minimum_required(VERSION 3.13.4)
project(notdec-llvm2c
	LANGUAGES C CXX
	VERSION 0.0.1
)
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

include(FindLLVM)

#===============================================================================
# Current project configurations
#===============================================================================

include_directories(${PROJECT_SOURCE_DIR}/include/notdec-llvm2c)

option(NOTDEC_LLVM2C_ENABLE_TESTS "Enable tests." ON)

add_subdirectory(lib)
add_subdirectory(tools)

if (NOTDEC_LLVM2C_ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()
