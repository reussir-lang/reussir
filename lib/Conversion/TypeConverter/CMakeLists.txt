add_mlir_conversion_library(MLIRReussirTypeConverter
  TypeConverter.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRDLTIDialect
  MLIRReussir
  MLIRTargetLLVMIRImport
  MLIRLLVMCommonConversion
  MLIRTransforms
  MLIRLLVMDialect
  MLIRArithAttrToLLVMConversion
  MLIRSync
  MLIRSyncTypeConverter
)

if(REUSSIR_ENABLE_OPENXLA)
  target_compile_definitions(obj.MLIRReussirTypeConverter PRIVATE REUSSIR_ENABLE_OPENXLA)
endif()

if(REUSSIR_ENABLE_RCGMP)
  target_link_libraries(obj.MLIRReussirTypeConverter PRIVATE ReussirRCGMP)
  target_link_libraries(MLIRReussirTypeConverter PUBLIC ReussirRCGMP)
endif()
