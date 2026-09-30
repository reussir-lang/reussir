add_mlir_conversion_library(MLIRReussirIFRTJustInTimeTransform
  IFRTJustInTimeTransform.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Support

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRBytecodeWriter
  MLIRFuncDialect
  MLIRTransforms
)

if(REUSSIR_ENABLE_OPENXLA)
  target_compile_definitions(obj.MLIRReussirIFRTJustInTimeTransform
    PRIVATE REUSSIR_ENABLE_OPENXLA)
  target_link_libraries(MLIRReussirIFRTJustInTimeTransform PUBLIC ReussirIfrt)
endif()
