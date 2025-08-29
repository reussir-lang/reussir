add_mlir_conversion_library(MLIRReussirIncDecCancelation
  IncDecCancelation.cpp

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
