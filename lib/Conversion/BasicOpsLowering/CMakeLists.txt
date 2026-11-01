add_mlir_conversion_library(MLIRReussirBasicOpsLowering
  BasicOpsLowering.cpp
  DropStackLowering.cpp
  TargetArrayLowering.cpp
  PjrtLowering.cpp
  CABISignatureConversion.cpp
  DbgInfoConversion.cpp
  VariantDebugInfo.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRArithAttrToLLVMConversion
  MLIRConvertToLLVMInterface
  MLIRReussir
  MLIRReussirTypeConverter
  MLIRLLVMCommonConversion
  MLIRLLVMDialect
  MLIRPtrToLLVM
  MLIRTransforms
  MLIRSync
  MLIRSyncConvertSyncToLLVM
  MLIRSyncTypeConverter
)

if(REUSSIR_ENABLE_RCGMP)
  target_link_libraries(obj.MLIRReussirBasicOpsLowering PRIVATE ReussirRCGMP)
  target_link_libraries(MLIRReussirBasicOpsLowering PUBLIC ReussirRCGMP)
endif()

if(REUSSIR_ENABLE_OPENXLA)
  target_compile_definitions(obj.MLIRReussirBasicOpsLowering PRIVATE REUSSIR_ENABLE_OPENXLA)
  target_link_libraries(MLIRReussirBasicOpsLowering PUBLIC ReussirIfrt)
endif()
