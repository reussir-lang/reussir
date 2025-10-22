cmake_minimum_required(VERSION 3.31)
project(Reussir LANGUAGES CXX C VERSION 0.1.0)

# Options
option(REUSSIR_ENABLE_TESTS "Build tests for Reussir" ON)
option(REUSSIR_ENABLE_PEDANTIC "Use pedantic compiler flags" OFF)

if (REUSSIR_ENABLE_PEDANTIC)
  message(STATUS "Pedantic mode enabled")
  # For now, we disable broken warnings in MLIR's own code.
  set(DISABLE_CHECKS
    "-Wno-deprecated-declarations -Wno-ambiguous-reversed-operator -Wno-dangling-assignment-gsl"
  )
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra -Wall -Werror -pedantic ${DISABLE_CHECKS}")
endif()

# Adjust output paths
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# C++ Standard
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 23)

# Find LLVM and MLIR
include(cmake/FindLLVM.cmake)
include(cmake/FindMLIR.cmake)
message(STATUS "include dirs for llvm and mlir: ${MLIR_INCLUDE_DIRS};${LLVM_INCLUDE_DIRS}")
set(MLIR_TABLEGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
message(STATUS "Tablegen output directory: ${MLIR_TABLEGEN_OUTPUT_DIR}")

# Include Immer
include(cmake/Immer.cmake)

# Include TPDE
include(cmake/TPDE.cmake)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${MLIR_TABLEGEN_OUTPUT_DIR}
    ${immer_SOURCE_DIR}
)

include_directories(SYSTEM
    ${MLIR_INCLUDE_DIRS}
    ${LLVM_INCLUDE_DIRS}
)

# Subdirectories
add_subdirectory(include)
add_subdirectory(lib)
add_subdirectory(tool)
add_subdirectory(crates)

if (REUSSIR_ENABLE_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()
