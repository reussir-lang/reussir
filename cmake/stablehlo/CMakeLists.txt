include(FetchContent)
list(APPEND CMAKE_MODULE_PATH "${MLIR_CMAKE_DIR}" "${LLVM_CMAKE_DIR}")

# Keep upstream warnings without applying Reussir's -Werror policy.
if(REUSSIR_ENABLE_PEDANTIC)
  add_compile_options(-Wno-error)
endif()

# Upstream add_mlir_* helpers append to global link collections. Keep these
# optional libraries out of Reussir's core backend and expose an explicit target.
foreach(kind DIALECT CONVERSION EXTENSION TRANSLATION)
  get_property(saved_${kind}_libs GLOBAL PROPERTY MLIR_${kind}_LIBS)
endforeach()

set(STABLEHLO_BUILD_EMBEDDED ON)
set(STABLEHLO_ENABLE_BINDINGS_PYTHON OFF)
set(STABLEHLO_ENABLE_STRICT_BUILD OFF)
FetchContent_Declare(stablehlo
  URL https://github.com/openxla/stablehlo/archive/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019.tar.gz
  URL_HASH SHA256=ef74c5fbf4feab186fc1b9968c0ae3509f422e9486faffe6e31a99c3f3e7d12f
  EXCLUDE_FROM_ALL
)
FetchContent_MakeAvailable(stablehlo)

add_library(ReussirStablehlo INTERFACE)
target_include_directories(ReussirStablehlo SYSTEM INTERFACE
  ${stablehlo_SOURCE_DIR}
  ${stablehlo_BINARY_DIR}
)
target_link_libraries(ReussirStablehlo INTERFACE
  StablehloRegister
  StablehloPasses
  StablehloOptimizationPasses
)

foreach(kind DIALECT CONVERSION EXTENSION TRANSLATION)
  set_property(GLOBAL PROPERTY MLIR_${kind}_LIBS "${saved_${kind}_libs}")
endforeach()
