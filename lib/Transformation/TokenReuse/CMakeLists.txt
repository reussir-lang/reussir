add_mlir_library(MLIRReussirTokenReuse
  TokenReuse.cpp

  DEPENDS
  MLIRReussirTransformationPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRReussirAnalysis
  MLIRIR
  MLIRPass
  MLIRArithValueBoundsOpInterfaceImpl
  MLIRMemRefDialect
  MLIRValueBoundsOpInterface
)
