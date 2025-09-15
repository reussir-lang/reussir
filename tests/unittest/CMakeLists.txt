find_package(GTest REQUIRED)

# Add unit test executable
add_executable(reussir_unit_tests
  test_main.cpp
  # Add more test files here as needed
)

# Link against required libraries
target_link_libraries(reussir_unit_tests
  PRIVATE
    MLIRReussir
    gtest
    gtest_main
    gmock
    gmock_main
)

# Include necessary directories
target_include_directories(reussir_unit_tests
  PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${MLIR_INCLUDE_DIRS}
    ${LLVM_INCLUDE_DIRS}
)

# Register with CTest
gtest_discover_tests(reussir_unit_tests)