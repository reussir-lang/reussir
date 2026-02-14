add_mlir_conversion_library(MLIRReussirAttachNativeTarget
  AttachNativeTarget.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core
  Target
  MC
  Support

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRLLVMDialect
  MLIRDLTIDialect
)
