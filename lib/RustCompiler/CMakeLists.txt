add_mlir_library(ReussirRustCompiler
  RustCompiler.cpp
  LINK_COMPONENTS
    Core
    Support
    BitReader
  LINK_LIBS PUBLIC
    ReussirPolyffiCache
)

