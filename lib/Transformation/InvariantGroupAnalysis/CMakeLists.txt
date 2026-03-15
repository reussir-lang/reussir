add_mlir_library(MLIRReussirInvariantGroupAnalysis
  InvariantGroupAnalysis.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRTransforms
  MLIRAnalysis
)
