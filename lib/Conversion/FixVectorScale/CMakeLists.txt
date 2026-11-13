add_mlir_conversion_library(MLIRReussirFixVectorScale
  FixVectorScale.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core
  Support

  LINK_LIBS PUBLIC
  MLIRArithDialect
  MLIRVectorDialect
)
