// RUN: %reussir-opt %s | %FileCheck %s

// A dynamic-extent array spells a runtime dimension as `?`, mirroring
// memref; static and dynamic dims mix, and shared boxes and unspecified
// references of it are valid types.
module @test {
  // CHECK: func.func private @dynamic() -> !reussir.array<? x i32>
  func.func private @dynamic() -> !reussir.array<? x i32>
  // CHECK: func.func private @mixed() -> !reussir.array<2 x ? x 4 x i64>
  func.func private @mixed() -> !reussir.array<2 x ? x 4 x i64>
  // CHECK: func.func private @boxed() -> !reussir.rc<!reussir.array<? x i32>>
  func.func private @boxed() -> !reussir.rc<!reussir.array<? x i32>>
  // CHECK: func.func private @borrowed(!reussir.ref<!reussir.array<? x i32>>)
  func.func private @borrowed(!reussir.ref<!reussir.array<? x i32>>)
}
