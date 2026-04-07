// RUN: %reussir-opt %s --convert-to-llvm | %FileCheck %s

module attributes {llvm.target_triple = "x86_64-pc-linux-gnu"} {
  llvm.func @id_i64_c(%arg0: i64) -> i64

  reussir.trampoline import "C" @id_i64 = @id_i64_c
}

// CHECK-LABEL: llvm.func @id_i64(
// CHECK-SAME: i64
// CHECK-SAME: -> i64
// CHECK: %[[R:.*]] = llvm.call @id_i64_c(%{{.*}}) : (i64) -> i64
// CHECK: llvm.return %[[R]] : i64
