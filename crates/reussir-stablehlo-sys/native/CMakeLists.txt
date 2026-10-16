add_library(ReussirStablehloSpecializer STATIC specialize.cpp)
set_target_properties(ReussirStablehloSpecializer PROPERTIES POSITION_INDEPENDENT_CODE ON)
target_link_libraries(ReussirStablehloSpecializer PRIVATE
  ReussirIfrt ReussirStablehlo MLIRFuncAllExtensions MLIRTransforms
  MLIRParser MLIRBytecodeWriter)
# Cargo needs optional archives in link order. MLIR/LLVM come from mlir-sys.
# No dependency on ReussirCAPI, the compiler, or reussir-jit.
set(specializer_archives
  ReussirIfrt StablehloPortableApi StablehloRegister StablehloSerialization
  StablehloPasses StablehloOptimizationPasses StablehloLinalgTransforms
  StablehloTypeConversion StablehloReplicaGroupUtils StablehloPassUtils
  ChloOps StablehloBroadcastUtils StablehloBroadcastLowering StablehloOps
  StablehloTypeInference VhloOps VhloTypes StablehloAssemblyFormat StablehloBase Version)
add_dependencies(ReussirStablehloSpecializer ${specializer_archives})
file(GENERATE OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/reussir-stablehlo-archives.txt
  CONTENT "$<JOIN:${specializer_archives},\n>")
add_custom_target(reussir-stablehlo-test
  COMMAND ${CMAKE_COMMAND} -E env
    CARGO_TARGET_DIR=${CMAKE_BINARY_DIR}/target
    ${REUSSIR_CARGO_CXX_ENV}
    MLIR_SYS_230_PREFIX=${REUSSIR_LLVM_PREFIX}
    REUSSIR_STABLEHLO_LIB_DIR=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    RUSTFLAGS=-Awarnings
    ${REUSSIR_CARGO} test ${REUSSIR_CARGO_TARGET_ARGS} --locked --release
      --package reussir-stablehlo
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  DEPENDS ReussirStablehloSpecializer
  USES_TERMINAL)
