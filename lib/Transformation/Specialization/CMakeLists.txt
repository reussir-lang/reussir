add_mlir_library(MLIRReussirSpecialization
  Specialization.cpp

  LINK_LIBS PUBLIC
  MLIRFuncDialect
  MLIRLLVMDialect
)
