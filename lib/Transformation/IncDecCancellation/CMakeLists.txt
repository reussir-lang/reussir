add_mlir_library(MLIRReussirIncDecCancellation
  IncDecCancellation.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen
  MLIRReussirAnalysis

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRTransforms
  MLIRReussirAnalysis
)
