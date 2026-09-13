// RUN: %reussir-opt %s --reussir-convert-to-llvm | %FileCheck %s

// Dynamic arrays can be referenced through boxed storage, but cannot be
// lowered as concrete function arguments or results. Partial conversion
// leaves the unsupported signatures unconverted.
module {
  // CHECK: func.func private @dynamic_argument(!reussir.array<? x i32>)
  func.func private @dynamic_argument(!reussir.array<? x i32>)
  // CHECK: func.func private @mixed_result() -> !reussir.array<4 x ? x i32>
  func.func private @mixed_result() -> !reussir.array<4 x ? x i32>
  // CHECK: llvm.func @static_empty() -> !llvm.array<0 x i32>
  func.func private @static_empty() -> !reussir.array<0 x i32>
}
