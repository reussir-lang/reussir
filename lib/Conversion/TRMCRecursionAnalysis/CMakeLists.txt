add_mlir_conversion_library(MLIRReussirTRMCRecursionAnalysis
  TRMCRecursionAnalysis.cpp

  DEPENDS
    MLIRReussirConversionPassIncGen

  LINK_LIBS PUBLIC
    MLIRFuncDialect
    MLIRIR
    MLIRPass
    MLIRSCFDialect
    MLIRUBDialect
    MLIRLLVMDialect
    MLIRReussir
)
