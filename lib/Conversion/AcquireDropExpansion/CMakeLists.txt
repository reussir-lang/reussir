add_mlir_conversion_library(MLIRReussirAcquireDropExpansion
  AcquireDropExpansion.cpp
  TailDrop.cpp
  WorklistDrop.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRReussirAnalysis
  MLIRArithValueBoundsOpInterfaceImpl
  MLIRFuncDialect
  MLIRMemRefDialect
  MLIRSCFDialect
  MLIRTransforms
  MLIRSync
)
