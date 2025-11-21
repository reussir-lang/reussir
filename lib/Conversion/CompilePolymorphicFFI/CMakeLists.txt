add_mlir_conversion_library(MLIRReussirCompilePolymorphicFFI
  CompilePolymorphicFFI.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen
  ReussirRustCompiler

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRTransforms
  MLIRTargetLLVMIRImport
  ReussirRustCompiler
)
