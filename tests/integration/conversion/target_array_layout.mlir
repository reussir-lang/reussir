// RUN: %reussir-opt %s --reussir-convert-to-llvm | %FileCheck %s

// The descriptor follows host pointer/index width. PjRt dimensions remain i64.
module attributes {
  dlti.dl_spec = #dlti.dl_spec<
    !llvm.ptr = dense<32> : vector<4xi64>,
    index = 32 : i32,
    i64 = dense<64> : vector<2xi64>,
    i32 = dense<32> : vector<2xi64>>,
  llvm.data_layout = "e-m:e-p:32:32-i64:64-i128:128-n32:64-S128",
  llvm.target_triple = "wasm32-unknown-unknown"
} {
  // CHECK-LABEL: llvm.func @create
  // CHECK: llvm.alloca {{.*}} x i64
  // CHECK: llvm.sext %arg0 : i32 to i64
  // CHECK: llvm.call @__reussir_pjrt_array_allocate({{.*}}) : (i32, i32, !llvm.ptr, i32, !llvm.ptr) -> !llvm.ptr
  // CHECK: llvm.mlir.constant(16 : i32)
  // CHECK: llvm.call @__reussir_allocate
  // CHECK: !llvm.struct<(i32, struct<(ptr, i32, i32)>)>
  func.func @create(%n: index) -> !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>> {
    %a = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>>
    return %a : !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>>
  }
}
