add_mlir_conversion_library(MLIRReussirIncDecCancellation
  IncDecCancellation.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen
  MLIRReussirAnalysis

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRTransforms
  MLIRReussirAnalysis
)
