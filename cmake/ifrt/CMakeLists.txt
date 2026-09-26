# IFRT has no upstream CMake build. Bazel builds its IR support closure;
# header-only repositories keep LLVM/MLIR and StableHLO owned by this CMake build.
include(FetchContent)
FetchContent_GetProperties(stablehlo)

if(NOT CMAKE_SYSTEM_NAME STREQUAL "Linux" OR CMAKE_CROSSCOMPILING OR
   NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  message(FATAL_ERROR "REUSSIR_ENABLE_OPENXLA currently requires native Linux with Clang")
endif()
if(NOT TARGET LLVM)
  message(FATAL_ERROR "REUSSIR_ENABLE_OPENXLA requires the installed LLVM shared library for XLA's host TableGen helper")
endif()
find_package(Python3 REQUIRED COMPONENTS Interpreter)
find_program(REUSSIR_BAZEL_EXECUTABLE NAMES bazel bazelisk REQUIRED)
set(REUSSIR_IFRT_JOBS 8 CACHE STRING "Parallel jobs for the IFRT Bazel build")

FetchContent_Declare(xla
  URL https://github.com/openxla/xla/archive/fc840b37e26bd36c99f32e295324a1700dc12ecd.tar.gz
  URL_HASH SHA256=64a0b2f2fc54fe811d28c148c3aac14b031dd85ecdeb49a8353132c6e3910dee
  EXCLUDE_FROM_ALL
)
FetchContent_MakeAvailable(xla)

# Reuse upstream's TableGen rules with the installed mlir-tblgen executable.
FetchContent_Declare(ifrt_tblgen
  URL https://raw.githubusercontent.com/llvm/llvm-project/e56c2cefc3e7978de4a2d799fa3abec77d75af21/utils/bazel/llvm-project-overlay/mlir/tblgen.bzl
  URL_HASH SHA256=09cce30519ea1e145044c713bded1f7651306bc95ba960e08ced2f4474ffecfa
  DOWNLOAD_NO_EXTRACT TRUE
)
FetchContent_MakeAvailable(ifrt_tblgen)

set(ifrt_archive ${CMAKE_CURRENT_BINARY_DIR}/libReussirIfrt.a)
set(ifrt_serdes_archive ${CMAKE_CURRENT_BINARY_DIR}/libReussirIfrtSerdes.a)
# Bazel checks source dependencies on every invocation. In particular, edits in
# a FetchContent source override must not leave a stale support archive.
add_custom_target(reussir-ifrt-build
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/build.py
    --work ${CMAKE_CURRENT_BINARY_DIR}
    --xla ${xla_SOURCE_DIR}
    --stablehlo ${stablehlo_SOURCE_DIR}
    --stablehlo-build ${stablehlo_BINARY_DIR}
    --llvm-include ${LLVM_INCLUDE_DIRS}
    --mlir-include ${MLIR_INCLUDE_DIRS}
    --tblgen ${MLIR_TABLEGEN_EXE}
    --tblgen-rule ${ifrt_tblgen_SOURCE_DIR}/tblgen.bzl
    --bazel ${REUSSIR_BAZEL_EXECUTABLE}
    --cc ${CMAKE_C_COMPILER} --cxx ${CMAKE_CXX_COMPILER}
    --compiler-launcher ${CMAKE_CXX_COMPILER_LAUNCHER}
    --cxx-flags=${CMAKE_CXX_FLAGS}
    --jobs ${REUSSIR_IFRT_JOBS}
    --output ${ifrt_archive}
    --serdes-output ${ifrt_serdes_archive}
    --host-library $<TARGET_FILE:MLIRTableGen> $<TARGET_FILE:LLVMTableGen>
                   $<TARGET_FILE:LLVM>
    --host-library-dir ${CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES}
  BYPRODUCTS ${ifrt_archive} ${ifrt_serdes_archive}
  DEPENDS StablehloOps StablehloPasses
  USES_TERMINAL
  VERBATIM
)
# Bazel provides the upstream IFRT support objects; their MLIR dependencies
# resolve against the same libraries already used by Reussir.
foreach(kind DIALECT CONVERSION EXTENSION)
  get_property(ifrt_${kind}_libs GLOBAL PROPERTY MLIR_${kind}_LIBS)
endforeach()
add_library(ReussirIfrt STATIC IMPORTED GLOBAL)
set_target_properties(ReussirIfrt PROPERTIES IMPORTED_LOCATION ${ifrt_archive})
add_dependencies(ReussirIfrt reussir-ifrt-build)
target_link_libraries(ReussirIfrt INTERFACE
  ReussirStablehlo StablehloPortableApi
  ${ifrt_DIALECT_libs} ${ifrt_CONVERSION_libs} ${ifrt_EXTENSION_libs}
  MLIRFuncAllExtensions MLIRTransforms MLIRBytecodeReader MLIRBytecodeWriter
  LLVMCore LLVMSupport ${CMAKE_DL_LIBS}
)

add_library(ReussirIfrtSerdes STATIC IMPORTED GLOBAL)
set_target_properties(ReussirIfrtSerdes PROPERTIES
  IMPORTED_LOCATION ${ifrt_serdes_archive})
add_dependencies(ReussirIfrtSerdes reussir-ifrt-build)
target_link_libraries(ReussirIfrtSerdes INTERFACE ReussirIfrt)
