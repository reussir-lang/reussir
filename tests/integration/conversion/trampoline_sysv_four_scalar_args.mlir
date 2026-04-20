// RUN: %reussir-opt %s --convert-to-llvm | %FileCheck %s

module attributes {llvm.target_triple = "x86_64-pc-linux-gnu"} {
  llvm.func @pick_last(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64) -> i64 {
    llvm.return %arg3 : i64
  }

  reussir.trampoline export "C" @pick_last_ffi = @pick_last
}

// CHECK-LABEL: llvm.func @pick_last_ffi(
// CHECK-SAME: !llvm.ptr
// CHECK-SAME: -> i64
// CHECK: %[[F0:.*]] = llvm.getelementptr %{{.*}}[0, 0] : (!llvm.ptr) -> !llvm.ptr, !llvm.struct<(i64, i64, i64, i64)>
// CHECK: %[[V0:.*]] = llvm.load %[[F0]] : !llvm.ptr -> i64
// CHECK: %[[F1:.*]] = llvm.getelementptr %{{.*}}[0, 1] : (!llvm.ptr) -> !llvm.ptr, !llvm.struct<(i64, i64, i64, i64)>
// CHECK: %[[V1:.*]] = llvm.load %[[F1]] : !llvm.ptr -> i64
// CHECK: %[[F2:.*]] = llvm.getelementptr %{{.*}}[0, 2] : (!llvm.ptr) -> !llvm.ptr, !llvm.struct<(i64, i64, i64, i64)>
// CHECK: %[[V2:.*]] = llvm.load %[[F2]] : !llvm.ptr -> i64
// CHECK: %[[F3:.*]] = llvm.getelementptr %{{.*}}[0, 3] : (!llvm.ptr) -> !llvm.ptr, !llvm.struct<(i64, i64, i64, i64)>
// CHECK: %[[V3:.*]] = llvm.load %[[F3]] : !llvm.ptr -> i64
// CHECK: %[[R:.*]] = llvm.call @pick_last(%[[V0]], %[[V1]], %[[V2]], %[[V3]]) : (i64, i64, i64, i64) -> i64
// CHECK: llvm.return %[[R]] : i64
