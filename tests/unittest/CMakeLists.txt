# Find GTest
find_package(GTest REQUIRED)
include(GoogleTest)

# Add unit test executable
add_executable(reussir-ut)

file(GLOB_RECURSE module_files "*.cppm")
file(GLOB_RECURSE source_files "*.cpp")
target_sources(
    reussir-ut
    PRIVATE FILE_SET CXX_MODULES FILES ${module_files} 
    PRIVATE ${source_files})

target_compile_options(reussir-ut
  PRIVATE
    -fno-exceptions
)

# Link against required libraries
target_link_libraries(reussir-ut
  PRIVATE
    MLIRReussir
    MLIRReussirBridge
    gtest
    gtest_main
    gmock
    gmock_main
)

# Include necessary directories
target_include_directories(reussir-ut
  PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${MLIR_INCLUDE_DIRS}
    ${LLVM_INCLUDE_DIRS}
)

# Register with CTest
gtest_discover_tests(reussir-ut)
