add_mlir_library(MLIRReussirRcCreateSink
  RcCreateSink.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRPass
)
