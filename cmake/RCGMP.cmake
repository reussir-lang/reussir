include(FetchContent)

# reussir-lang/mlir-rcgmp provides the `rcgmp` dialect: reference-counted,
# arbitrary-precision integers (`!rcgmp.mpz`) whose small values are tagged
# immediates and whose large values are heap objects backed by GMP's limb
# routines. It supplies the conversions that split the operations into an
# inline fast path and runtime calls (`convert-rcgmp-to-scf`) and lower the
# remaining bridge operations to LLVM, plus the C++ runtime those calls reach.
#
# The runtime needs GMP itself. Check for it here so that a missing GMP points
# at the option that turns the integration off, instead of failing inside the
# fetched project.
find_path(GMP_INCLUDE_DIR NAMES gmp.h)
find_library(GMP_LIBRARY NAMES gmp)
if(NOT GMP_INCLUDE_DIR OR NOT GMP_LIBRARY)
  message(FATAL_ERROR
    "REUSSIR_ENABLE_RCGMP requires GMP 6.2 or newer, but gmp.h or libgmp was "
    "not found. Install GMP (add its prefix to CMAKE_PREFIX_PATH if it is not "
    "in a standard location), or configure with -DREUSSIR_ENABLE_RCGMP=OFF.")
endif()
# rrc records this directory as the run path of products that link GMP, so
# they find it at load time without an environment setting.
get_filename_component(REUSSIR_GMP_LIBRARY_DIR "${GMP_LIBRARY}" DIRECTORY)

set(RCGMP_ENABLE_TESTS OFF CACHE BOOL "Build rcgmp tests" FORCE)
set(RCGMP_ENABLE_BENCHMARKS OFF CACHE BOOL "Build rcgmp benchmarks" FORCE)
# reussir-rt defines the runtime's allocation hooks (`rcgmp_allocate` and
# `rcgmp_dealloc`), so heap integers live on the language heap.
set(RCGMP_RUNTIME_DEFAULT_ALLOCATOR OFF CACHE BOOL "Provide default allocation hooks" FORCE)

# Upstream add_mlir_* helpers append to global link collections. Keep the rcgmp
# libraries out of the `${dialect_libs}` every Reussir library links, and expose
# them through the explicit ReussirRCGMP target instead.
foreach(kind DIALECT CONVERSION EXTENSION TRANSLATION)
  get_property(saved_${kind}_libs GLOBAL PROPERTY MLIR_${kind}_LIBS)
endforeach()

FetchContent_Declare(
  rcgmp
  GIT_REPOSITORY https://github.com/reussir-lang/mlir-rcgmp.git
  GIT_TAG 05ffb84c5f8ddd762bb1d87566d3b585db8988c0
  # Only the libraries Reussir links are built; `rcgmp-opt` is not.
  EXCLUDE_FROM_ALL
)
FetchContent_MakeAvailable(rcgmp)

foreach(kind DIALECT CONVERSION EXTENSION TRANSLATION)
  set_property(GLOBAL PROPERTY MLIR_${kind}_LIBS "${saved_${kind}_libs}")
endforeach()

# The rcgmp libraries rely on the directory-scoped `include_directories` of
# their own project, so the interface target carries the source headers and
# the tablegen-generated `.h.inc` files.
add_library(ReussirRCGMP INTERFACE)
target_include_directories(ReussirRCGMP INTERFACE
  ${rcgmp_SOURCE_DIR}/include
  ${rcgmp_BINARY_DIR}/include
)
target_link_libraries(ReussirRCGMP INTERFACE
  MLIRRCGMP
  MLIRRCGMPToSCF
  MLIRRCGMPToLLVM
)
target_compile_definitions(ReussirRCGMP INTERFACE REUSSIR_ENABLE_RCGMP)

# Nix scan-deps workaround (see lib/Bridge/CMakeLists.txt): GMP's include
# directory is one of the nix cc wrapper's implicit directories, so CMake elides
# its -I flag, and the raw clang-scan-deps cannot find gmp.h while scanning the
# runtime's modules. Raw compile options are never elided.
if(DEFINED ENV{NIX_STORE})
  target_compile_options(RCGMPRuntime PRIVATE "-I${GMP_INCLUDE_DIR}")
endif()
