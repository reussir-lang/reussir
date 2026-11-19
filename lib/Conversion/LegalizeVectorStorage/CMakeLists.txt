add_mlir_conversion_library(MLIRReussirLegalizeVectorStorage
  LegalizeVectorStorage.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core
  Support

  LINK_LIBS PUBLIC
  MLIRArmSVEDialect
  MLIRArmSVETransforms
  MLIRFuncDialect
)
