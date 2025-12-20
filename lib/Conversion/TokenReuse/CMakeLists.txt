add_mlir_conversion_library(MLIRReussirTokenReuse
  TokenReuse.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRPass
)
