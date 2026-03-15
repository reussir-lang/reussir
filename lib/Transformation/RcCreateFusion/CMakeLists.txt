add_mlir_library(MLIRReussirRcCreateFusion
  RcCreateFusion.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRTransforms
)
