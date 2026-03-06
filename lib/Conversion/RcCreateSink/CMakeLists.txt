add_mlir_conversion_library(MLIRReussirRcCreateSink
  RcCreateSink.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRPass
)
