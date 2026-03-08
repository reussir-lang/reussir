add_mlir_conversion_library(MLIRReussirUniqueCarryingRecursionAnalysis
  UniqueCarryingRecursionAnalysis.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRAnalysis
  MLIRFuncDialect
  MLIRReussir
  MLIRSCFDialect
  MLIRTransforms
)
