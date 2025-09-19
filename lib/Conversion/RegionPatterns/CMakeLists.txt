add_mlir_conversion_library(MLIRReussirRegionPatterns
  RegionPatterns.cpp

  DEPENDS
  MLIRReussirConversionPassIncGen

  LINK_COMPONENTS
  Core

  LINK_LIBS PUBLIC
  MLIRReussir
  MLIRTransforms
)
