# Native C++ consumers need the staticlib and rustc's native library closure.
# Rust consumers use the rlib dependency in reussir-backend-sys instead.
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(cache_profile dev)
  set(cache_profile_dir debug)
else()
  set(cache_profile release)
  set(cache_profile_dir release)
endif()

set(cache_library "${CMAKE_BINARY_DIR}/target/${REUSSIR_CARGO_TARGET_SUBDIR}${cache_profile_dir}/${CMAKE_STATIC_LIBRARY_PREFIX}reussir_polyffi_cache${CMAKE_STATIC_LIBRARY_SUFFIX}")
set(cache_native_libs "${CMAKE_CURRENT_BINARY_DIR}/native-libs.rsp")
add_custom_target(reussir-polyffi-cache-build
  COMMAND ${CMAKE_COMMAND} -E env
    CARGO_TARGET_DIR=${CMAKE_BINARY_DIR}/target
    RUSTFLAGS=-Awarnings
    ${REUSSIR_CARGO_CXX_ENV}
    ${CMAKE_COMMAND}
    "-DCARGO_COMMAND=${REUSSIR_CARGO}"
    "-DCARGO_TARGET_ARGS=${REUSSIR_CARGO_TARGET_ARGS}"
    -DPROFILE=${cache_profile}
    -DNATIVE_LIBS=${cache_native_libs}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/build.cmake
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  BYPRODUCTS ${cache_library} ${cache_native_libs}
  COMMENT "Building the polyffi cache binding"
  VERBATIM
)
add_library(ReussirPolyffiCache STATIC IMPORTED GLOBAL)
set_target_properties(ReussirPolyffiCache PROPERTIES
  IMPORTED_LOCATION "${cache_library}"
  INTERFACE_LINK_DEPENDS "${cache_native_libs}"
  INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/include")
add_dependencies(ReussirPolyffiCache reussir-polyffi-cache-build)
if(MSVC)
  target_link_options(ReussirPolyffiCache INTERFACE "LINKER:@${cache_native_libs}")
else()
  # Keep the native libraries after the Rust archive (also with --as-needed).
  target_link_libraries(ReussirPolyffiCache INTERFACE "-Wl,@${cache_native_libs}")
endif()
