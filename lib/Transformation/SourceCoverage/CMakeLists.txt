add_mlir_library(MLIRReussirSourceCoverage
  InstrumentCoverage.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core
  ProfileData
  Support

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRLLVMDialect
  MLIRSCFDialect
  MLIRPass
)
