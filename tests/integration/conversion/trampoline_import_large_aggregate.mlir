// RUN: %reussir-opt %s --convert-to-llvm | %FileCheck %s

module attributes {llvm.target_triple = "x86_64-pc-linux-gnu"} {
  llvm.func @id_huge_c(
      %out: !llvm.ptr,
      %args: !llvm.ptr)

  reussir.trampoline import "C" @id_huge = @id_huge_c
}

// CHECK-LABEL: llvm.func @id_huge(
// CHECK-SAME: !llvm.ptr
// CHECK-SAME: !llvm.ptr
// CHECK: llvm.call @id_huge_c(%{{.*}}, %{{.*}}) : (!llvm.ptr, !llvm.ptr) -> ()
// CHECK: llvm.return
