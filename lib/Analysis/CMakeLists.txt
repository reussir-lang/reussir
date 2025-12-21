get_property(dialect_libs GLOBAL PROPERTY MLIR_DIALECT_LIBS)

add_mlir_library(MLIRReussirAnalysis
  AliasAnalysis.cpp
  LINK_LIBS PUBLIC
    immer
    MLIRAnalysis
    MLIRIR
    MLIRSupport
    ${dialect_libs}
)

