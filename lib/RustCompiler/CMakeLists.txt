add_mlir_library(ReussirRustCompiler
  RustCompiler.cpp
  LINK_COMPONENTS
    Core
    Support
    BitReader
    TargetParser
)


# C++ consumers need the standalone Rust static library. Rust consumers link
# the rlib through reussir-backend-sys instead of bundling a second Rust std.
target_link_libraries(ReussirRustCompiler PUBLIC ReussirFfiCache)
