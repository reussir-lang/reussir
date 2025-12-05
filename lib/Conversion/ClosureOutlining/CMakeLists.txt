add_mlir_conversion_library(MLIRReussirClosureOutlining
  ClosureOutlining.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRPass
  MLIRFuncDialect
)

