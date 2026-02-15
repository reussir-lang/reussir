// RUN: %reussir-opt %s --reussir-lowering-basic-ops | %FileCheck %s

module attributes {llvm.target_triple = "aarch64-unknown-linux-gnu"} {
  llvm.func @id_huge(
      %arg0: !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>)
      -> !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)> {
    llvm.return %arg0 : !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>
  }

  reussir.trampoline "C" @id_huge_ffi = @id_huge
}

// CHECK-LABEL: llvm.func @id_huge_ffi(
// CHECK-SAME: !llvm.ptr {llvm.sret = !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>}
// CHECK-SAME: !llvm.ptr
// CHECK: %[[V:.*]] = llvm.load %{{.*}} : !llvm.ptr -> !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>
// CHECK: %[[R:.*]] = llvm.call @id_huge(%[[V]]) : (!llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>) -> !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>
// CHECK: llvm.store %[[R]], %{{.*}} : !llvm.struct<(i64, i64, i64, i64, i64, i64, i64, i64)>, !llvm.ptr
// CHECK: llvm.return
