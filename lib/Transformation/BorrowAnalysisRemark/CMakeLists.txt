add_mlir_library(MLIRReussirBorrowAnalysisRemark
  BorrowAnalysisRemark.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRReussirAnalysis
  MLIRFuncDialect
)
