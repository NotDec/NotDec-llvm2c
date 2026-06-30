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
set(NOTDEC_LLVM2C_BINARYSUB_DIR "" CACHE PATH
    "Path to binarysub source when building notdec-llvm2c standalone")

if (NOT TARGET binarysub_lib)
  if (NOT NOTDEC_LLVM2C_BINARYSUB_DIR)
    get_filename_component(NOTDEC_LLVM2C_DEFAULT_BINARYSUB_DIR
      "${PROJECT_SOURCE_DIR}/../binarysub" ABSOLUTE)
    if (EXISTS "${NOTDEC_LLVM2C_DEFAULT_BINARYSUB_DIR}/CMakeLists.txt")
      set(NOTDEC_LLVM2C_BINARYSUB_DIR
        "${NOTDEC_LLVM2C_DEFAULT_BINARYSUB_DIR}" CACHE PATH
        "Path to binarysub source when building notdec-llvm2c standalone" FORCE)
    endif()
  endif()

  if (NOT NOTDEC_LLVM2C_BINARYSUB_DIR OR
      NOT EXISTS "${NOTDEC_LLVM2C_BINARYSUB_DIR}/CMakeLists.txt")
    message(FATAL_ERROR
      "binarysub_lib target is missing. Build notdec-llvm2c from the main "
      "NotDec tree, place binarysub next to NotDec-llvm2c, or pass "
      "-DNOTDEC_LLVM2C_BINARYSUB_DIR=/path/to/binarysub.")
  endif()

  add_subdirectory("${NOTDEC_LLVM2C_BINARYSUB_DIR}"
    "${CMAKE_BINARY_DIR}/binarysub")
endif()

add_subdirectory(lib)
add_subdirectory(tools)

if (NOTDEC_LLVM2C_ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()
