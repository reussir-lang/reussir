# TPDE Fetch Content Configuration
# Fetches TPDE from GitHub and configures it for use in the project

include(FetchContent)

# Set TPDE version to the specified commit
set(TPDE_VERSION "29bcf1841c572fcdc75dd61bb3efff5bfb1c5ac6")

# Disable TPDE_ENABLE_ENCODEGEN as requested
set(TPDE_ENABLE_LLVM ON CACHE BOOL "Disable TPDE LLVM" FORCE)
set(TPDE_ENABLE_ENCODEGEN ON CACHE BOOL "Disable TPDE Encodegen" FORCE)
set(TPDE_INCLUDE_TESTS OFF CACHE BOOL "Disable TPDE tests" FORCE)

# Suppress specific warnings for TPDE
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-ctad-maybe-unsupported -Wno-implicit-fallthrough -Wno-macro-redefined")

# Declare TPDE as a dependency
FetchContent_Declare(
    tpde
    GIT_REPOSITORY https://github.com/tpde2/tpde.git
    GIT_TAG ${TPDE_VERSION}
    GIT_SHALLOW TRUE
)

# Make TPDE available
FetchContent_MakeAvailable(tpde)

# Apply the LLVM 21 support patch
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/tpde-llvm21-support.patch")
    message(STATUS "Applying TPDE LLVM 21 support patch...")
    
    # Get absolute paths
    get_filename_component(PATCH_FILE_ABS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/tpde-llvm21-support.patch" ABSOLUTE)
    get_filename_component(TPDE_SOURCE_ABS "${tpde_SOURCE_DIR}" ABSOLUTE)
    
    message(STATUS "Patch file: ${PATCH_FILE_ABS}")
    message(STATUS "TPDE source directory: ${TPDE_SOURCE_ABS}")
    
    # Check if target files exist
    set(CMAKELISTS_FILE "${TPDE_SOURCE_ABS}/CMakeLists.txt")
    set(TARGET_CPP_FILE "${TPDE_SOURCE_ABS}/tpde-encodegen/src/x64/Target.cpp")
    
    message(STATUS "Checking CMakeLists.txt: ${CMAKELISTS_FILE}")
    message(STATUS "Checking Target.cpp: ${TARGET_CPP_FILE}")
    
    if(EXISTS "${CMAKELISTS_FILE}" AND EXISTS "${TARGET_CPP_FILE}")
        message(STATUS "Target files found, applying patch...")
        
        # Try different patch strip levels and approaches
        set(PATCH_SUCCESS FALSE)
        
        # Try with -p1 (strip one directory level for git patches)
        message(STATUS "Executing: patch -N -f -p1 -i ${PATCH_FILE_ABS}")
        message(STATUS "Working directory: ${TPDE_SOURCE_ABS}")
        execute_process(
            COMMAND patch -N -f -p1 -i ${PATCH_FILE_ABS}
            WORKING_DIRECTORY ${TPDE_SOURCE_ABS}
            RESULT_VARIABLE PATCH_RESULT
            OUTPUT_VARIABLE PATCH_OUTPUT
            ERROR_VARIABLE PATCH_ERROR
        )
        
        if(PATCH_RESULT EQUAL 0)
            set(PATCH_SUCCESS TRUE)
            message(STATUS "TPDE LLVM 21 support patch applied successfully")
            message(STATUS "Patch output: ${PATCH_OUTPUT}")
        else()
            message(WARNING "Failed to apply TPDE LLVM 21 support patch")
            message(WARNING "Patch output: ${PATCH_OUTPUT}")
            message(WARNING "Patch error: ${PATCH_ERROR}")
        endif()
        
        if(NOT PATCH_SUCCESS)
            message(WARNING "TPDE LLVM 21 support patch could not be applied. You may need to apply it manually.")
        endif()
    else()
        message(WARNING "Target files not found:")
        if(NOT EXISTS "${CMAKELISTS_FILE}")
            message(WARNING "  CMakeLists.txt not found at: ${CMAKELISTS_FILE}")
        endif()
        if(NOT EXISTS "${TARGET_CPP_FILE}")
            message(WARNING "  Target.cpp not found at: ${TARGET_CPP_FILE}")
        endif()
        message(WARNING "Cannot apply patch - target files missing.")
    endif()
else()
    message(WARNING "TPDE LLVM 21 support patch not found at cmake/tpde-llvm21-support.patch")
endif()

# Add TPDE include directories to the project
if(TARGET tpde)
    target_include_directories(tpde INTERFACE
        $<BUILD_INTERFACE:${tpde_SOURCE_DIR}/tpde/include>
        $<BUILD_INTERFACE:${tpde_SOURCE_DIR}/tpde-llvm/include>
    )
    
endif()

# Print configuration status
message(STATUS "TPDE version: ${TPDE_VERSION}")
message(STATUS "TPDE_ENABLE_ENCODEGEN: ${TPDE_ENABLE_ENCODEGEN}")
message(STATUS "TPDE source directory: ${tpde_SOURCE_DIR}")
