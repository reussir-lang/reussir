# Builds the native half of the reussir-stablehlo crate and tests the crate
# with cargo. The native library depends on the pinned StableHLO and IFRT
# dialect archives only, never on the Reussir compiler or JIT.
#
#   ninja -C build ReussirStablehloRuntime   # native library
#   ninja -C build reussir-stablehlo-test    # the crate's tests

add_library(ReussirStablehloRuntime STATIC
  native/program.cpp native/refine.cpp native/donation.cpp)
set_target_properties(ReussirStablehloRuntime PROPERTIES POSITION_INDEPENDENT_CODE ON)
target_include_directories(ReussirStablehloRuntime PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/native)
target_link_libraries(ReussirStablehloRuntime PRIVATE
  ReussirIfrt ReussirStablehlo MLIRFuncAllExtensions MLIRTransforms
  MLIRParser MLIRBytecodeWriter)

# The archives cargo links after ReussirStablehloRuntime, in link order. MLIR
# and LLVM come from mlir-sys.
set(REUSSIR_STABLEHLO_ARCHIVES
  ReussirIfrt StablehloPortableApi StablehloRegister StablehloSerialization
  StablehloPasses StablehloOptimizationPasses StablehloLinalgTransforms
  StablehloTypeConversion StablehloReplicaGroupUtils StablehloPassUtils
  ChloOps StablehloBroadcastUtils StablehloBroadcastLowering StablehloOps
  StablehloTypeInference VhloOps VhloTypes StablehloAssemblyFormat StablehloBase Version)
add_dependencies(ReussirStablehloRuntime ${REUSSIR_STABLEHLO_ARCHIVES})
file(GENERATE OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/reussir-stablehlo-archives.txt
  CONTENT "$<JOIN:${REUSSIR_STABLEHLO_ARCHIVES},\n>")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(REUSSIR_STABLEHLO_CARGO_PROFILE "dev")
else()
    set(REUSSIR_STABLEHLO_CARGO_PROFILE "release")
endif()

add_custom_target(reussir-stablehlo-test
    COMMAND ${CMAKE_COMMAND} -E env
        CARGO_TARGET_DIR=${CMAKE_BINARY_DIR}/target
        ${REUSSIR_CARGO_CXX_ENV}
        MLIR_SYS_230_PREFIX=${REUSSIR_LLVM_PREFIX}
        REUSSIR_STABLEHLO_LIB_DIR=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
        RUSTFLAGS=-Awarnings
        ${REUSSIR_CARGO} test ${REUSSIR_CARGO_TARGET_ARGS} --locked --profile ${REUSSIR_STABLEHLO_CARGO_PROFILE}
        --package reussir-stablehlo
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    DEPENDS ReussirStablehloRuntime
    COMMENT "Testing reussir-stablehlo with cargo"
    USES_TERMINAL
)
