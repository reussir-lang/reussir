add_mlir_conversion_library(MLIRReussirInferVariantTag
  InferVariantTag.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen
  MLIRReussirAliasAnalysis

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRTransforms
  MLIRReussirAliasAnalysis
)
