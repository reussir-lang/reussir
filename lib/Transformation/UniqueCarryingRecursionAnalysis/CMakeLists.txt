add_mlir_library(MLIRReussirUniqueCarryingRecursionAnalysis
  UniqueCarryingRecursionAnalysis.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRAnalysis
  MLIRFuncDialect
  MLIRReussir
  MLIRSCFDialect
  MLIRTransforms
)
