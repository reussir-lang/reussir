# Standalone C ABI archive for native C++ tools. Cargo-based backend consumers
# use the rlib dependency and do not link this archive.
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(cache_profile dev)
  set(cache_profile_dir debug)
else()
  set(cache_profile release)
  set(cache_profile_dir release)
endif()

set(cache_target_dir "${CMAKE_BINARY_DIR}/target-ffi-cache")
if(MSVC)
  set(cache_archive "${cache_target_dir}/${REUSSIR_CARGO_TARGET_SUBDIR}${cache_profile_dir}/reussir_ffi_cache.lib")
else()
  set(cache_archive "${cache_target_dir}/${REUSSIR_CARGO_TARGET_SUBDIR}${cache_profile_dir}/libreussir_ffi_cache.a")
endif()
add_custom_target(reussir-ffi-cache-build
  COMMAND ${CMAKE_COMMAND} -E env
    CARGO_TARGET_DIR=${cache_target_dir}
    ${REUSSIR_CARGO_CXX_ENV}
    ${REUSSIR_CARGO} build ${REUSSIR_CARGO_TARGET_ARGS} --locked
    --profile ${cache_profile} --package reussir-ffi-cache --quiet
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  BYPRODUCTS "${cache_archive}"
  COMMENT "Building the Foyer cache C API"
  USES_TERMINAL)
add_library(ReussirFfiCache STATIC IMPORTED GLOBAL)
set_target_properties(ReussirFfiCache PROPERTIES IMPORTED_LOCATION "${cache_archive}")
add_dependencies(ReussirFfiCache reussir-ffi-cache-build)
# Native dependencies of Rust std and the cache's Windows file-I/O support.
# Windows SDK imports cover the windows-targets import archive as well; no
# path into Cargo's package registry is required.
if(WIN32)
  set_property(TARGET ReussirFfiCache PROPERTY INTERFACE_LINK_LIBRARIES
    "advapi32;bcrypt;kernel32;ntdll;userenv;ws2_32;synchronization;dbghelp;legacy_stdio_definitions")
elseif(APPLE)
  set_property(TARGET ReussirFfiCache PROPERTY INTERFACE_LINK_LIBRARIES "System")
else()
  find_package(Threads REQUIRED)
  set_property(TARGET ReussirFfiCache PROPERTY INTERFACE_LINK_LIBRARIES
    "Threads::Threads;${CMAKE_DL_LIBS};m;rt;util")
endif()
add_custom_target(reussir-ffi-cache-test
  COMMAND ${CMAKE_COMMAND} -E env
    CARGO_TARGET_DIR=${cache_target_dir}
    ${REUSSIR_CARGO_CXX_ENV}
    ${REUSSIR_CARGO} test ${REUSSIR_CARGO_TARGET_ARGS} --locked
    --profile ${cache_profile} --package reussir-ffi-cache
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  USES_TERMINAL)
