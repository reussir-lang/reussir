add_mlir_conversion_library(MLIRReussirTokenAllocation
  TokenAllocation.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRTransforms
)

