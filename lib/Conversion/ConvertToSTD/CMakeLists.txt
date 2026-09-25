add_mlir_conversion_library(MLIRReussirConvertToSTD
  ConvertToSTD.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRArithDialect
  MLIRArithValueBoundsOpInterfaceImpl
  MLIRReussir
  MLIRMathDialect
  MLIRMemRefDialect
  MLIRPtrDialect
  MLIRSCFDialect
  MLIRFuncDialect
  MLIRTransforms
  MLIRValueBoundsOpInterface
  MLIRSync
  MLIRSyncConvertSyncToSTD
)
