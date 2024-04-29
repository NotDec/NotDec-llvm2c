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
include_directories(${PROJECT_SOURCE_DIR}/include)

add_subdirectory(lib)
add_subdirectory(tools)
