add_mlir_library(MLIRReussirBorrowSpecialization
  BorrowSpecialization.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRReussirAnalysis
  MLIRFuncDialect
)
