

# Try to find GTest on the system first
find_package(GTest 1.17.0 QUIET)

if(NOT GTest_FOUND)
  message(STATUS "GTest not found on system, downloading it")
  # If GTest v1.17.0 is not found on system, download it
  include(FetchContent)
  
  FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG        v1.17.0
  )
  
  # For Windows: Prevent overriding the parent project's compiler/linker settings
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  
  # Set all GTest build options
  set(BUILD_GMOCK ON CACHE BOOL "Build GMock" FORCE)
  set(INSTALL_GTEST OFF CACHE BOOL "Install GTest" FORCE)
  
  FetchContent_MakeAvailable(googletest)
else()
  message(STATUS "GTest found on system")
endif()

# Include GTest directories
include(GoogleTest)
