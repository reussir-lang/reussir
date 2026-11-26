add_mlir_library(MLIRReussirSourceCoverage
  EmitCoverageMapping.cpp
  InstrumentCoverage.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core
  ProfileData
  Support
  TargetParser

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRFuncDialect
  MLIRLLVMDialect
  MLIRPass
)
