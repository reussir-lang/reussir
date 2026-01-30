# Determine build profile based on CMAKE_BUILD_TYPE
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CARGO_PROFILE "dev")
    set(CARGO_PROFILE_DIR "debug")
else()
    set(CARGO_PROFILE "release")
    set(CARGO_PROFILE_DIR "release")
endif()

# Custom target to build reussir-rt with cargo
add_custom_target(reussir-rt-build ALL
    COMMAND ${CMAKE_COMMAND} -E env
        CARGO_TARGET_DIR=${CMAKE_BINARY_DIR}/target
        RUSTFLAGS="-Awarnings"
        cargo build --profile ${CARGO_PROFILE} --quiet
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building reussir-rt library with cargo (${CARGO_PROFILE} profile)"
)

# Custom target to copy the libraries
if(WIN32)
    set(REUSSIR_RT_OUTPUT_DIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
else()
    set(REUSSIR_RT_OUTPUT_DIR ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
endif()

add_custom_target(reussir-rt-install ALL
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_BINARY_DIR}/target/${CARGO_PROFILE_DIR}/deps
        ${REUSSIR_RT_OUTPUT_DIR}
    COMMENT "Installing reussir-rt libraries"
    DEPENDS reussir-rt-build
)

# Read rust-toolchain.toml to get the channel
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/rust-toolchain.toml" RUST_TOOLCHAIN_CONTENT)
string(REGEX MATCH "channel = \"([^\"]+)\"" _ ${RUST_TOOLCHAIN_CONTENT})
set(RUST_CHANNEL ${CMAKE_MATCH_1})

if(NOT RUST_CHANNEL)
    message(FATAL_ERROR "Could not parse channel from rust-toolchain.toml")
endif()

message(STATUS "Target Rust channel: ${RUST_CHANNEL}")

# Define where we want the local toolchain to live (intermediate step)
set(LOCAL_RUSTUP_HOME "${CMAKE_BINARY_DIR}/.rustup")
set(LOCAL_CARGO_HOME "${CMAKE_BINARY_DIR}/.cargo")

# Detect the host triple from rustc -vV.
# This is needed to construct the correct toolchain path and to install
# the correct toolchain variant.
execute_process(
    COMMAND rustc -vV
    OUTPUT_VARIABLE RUSTC_VERSION_INFO
    RESULT_VARIABLE RUSTC_RESULT
)

if(NOT RUSTC_RESULT EQUAL 0)
    message(FATAL_ERROR "Failed to run 'rustc -vV'. Ensure rustc is installed and in PATH.")
endif()

string(REGEX MATCH "host: ([a-zA-Z0-9_-]+)" _ ${RUSTC_VERSION_INFO})
set(RUST_HOST_TRIPLE ${CMAKE_MATCH_1})

if(NOT RUST_HOST_TRIPLE)
    message(FATAL_ERROR "Could not detect host triple from 'rustc -vV' output. Output was: ${RUSTC_VERSION_INFO}")
endif()

# We need to install the toolchain.
# We use a custom command/target to ensure it's there.
# Note: We install the specific toolchain variant matching the host triple
# to ensure the installed path matches what we'll look for during copy.
add_custom_target(reussir-rust-toolchain-fetch
    COMMAND ${CMAKE_COMMAND} -E env
        RUSTUP_HOME=${LOCAL_RUSTUP_HOME}
        CARGO_HOME=${LOCAL_CARGO_HOME}
        rustup toolchain install ${RUST_CHANNEL}-${RUST_HOST_TRIPLE} --profile minimal --no-self-update
    COMMENT "Ensuring Rust toolchain ${RUST_CHANNEL}-${RUST_HOST_TRIPLE} is installed in ${LOCAL_RUSTUP_HOME}"
    USES_TERMINAL
)

set(INSTALLED_TOOLCHAIN_PATH "${LOCAL_RUSTUP_HOME}/toolchains/${RUST_CHANNEL}-${RUST_HOST_TRIPLE}")

add_custom_target(reussir-rust-install ALL
    # Copy bin
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${INSTALLED_TOOLCHAIN_PATH}/bin"
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"
    # Copy lib
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${INSTALLED_TOOLCHAIN_PATH}/lib"
        "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    COMMENT "Installing rust libraries and tools from ${INSTALLED_TOOLCHAIN_PATH}"
    DEPENDS reussir-rt-build reussir-rust-toolchain-fetch
)

# Main reussir-rt target that depends on the complete build process
add_custom_target(reussir-rt ALL
    DEPENDS reussir-rt-install reussir-rust-install
)
