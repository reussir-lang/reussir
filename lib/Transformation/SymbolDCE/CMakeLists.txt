add_mlir_library(MLIRReussirSymbolDCE
  SymbolDCE.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_LIBS PUBLIC
  MLIRFuncDialect
  MLIRTransforms
)
