add_mlir_library(MLIRReussirPjrtDonation
  PjrtDonation.cpp
  DEPENDS MLIRReussirTransformationPassIncGen
  LINK_LIBS PUBLIC MLIRReussirAnalysis MLIRReussir MLIRArithDialect
    MLIRFuncDialect MLIRSCFDialect MLIRParser MLIRBytecodeWriter MLIRTransforms
)
