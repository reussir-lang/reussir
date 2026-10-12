add_mlir_conversion_library(MLIRReussirAcquireDropExpansion
  AcquireDropExpansion.cpp
  TailDrop.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRSCFDialect
  MLIRTransforms
  MLIRSync
)
