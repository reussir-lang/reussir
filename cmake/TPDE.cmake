# TPDE Fetch Content Configuration
# Fetches TPDE from GitHub and configures it for use in the project

include(FetchContent)

# Pinned TPDE commit. TPDE has no releases; bump this deliberately and rerun
# the JIT tests, since its LLVM support tracks LLVM main.
set(TPDE_VERSION "9779acf4ada3736e779391da1e4b3369dba08024")

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
)

# Make TPDE available
FetchContent_MakeAvailable(tpde)

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
