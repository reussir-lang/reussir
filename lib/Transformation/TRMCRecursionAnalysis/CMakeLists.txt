add_mlir_library(MLIRReussirTRMCRecursionAnalysis
  TRMCRecursionAnalysis.cpp

  DEPENDS
    MLIRReussirTransformationPassIncGen

  LINK_LIBS PUBLIC
    MLIRFuncDialect
    MLIRIR
    MLIRPass
    MLIRSCFDialect
    MLIRUBDialect
    MLIRLLVMDialect
    MLIRReussir
)
