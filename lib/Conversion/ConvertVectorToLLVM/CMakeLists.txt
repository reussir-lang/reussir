add_mlir_conversion_library(MLIRReussirConvertVectorToLLVM
  ConvertVectorToLLVM.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core
  Support

  LINK_LIBS PUBLIC
  MLIRArmSVETransforms
  MLIRLLVMCommonConversion
  MLIRVectorToLLVM
  MLIRVectorTransforms
)
