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
add_custom_target(reussir-rt-install ALL
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/target/${CARGO_PROFILE_DIR}/libreussir_rt.a
        ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libreussir-rt.a
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/target/${CARGO_PROFILE_DIR}/libreussir_rt.so
        ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libreussir-rt.so
    COMMENT "Installing reussir-rt libraries"
    DEPENDS reussir-rt-build
)

# Main reussir-rt target that depends on the complete build process
add_custom_target(reussir-rt ALL
    DEPENDS reussir-rt-install
)