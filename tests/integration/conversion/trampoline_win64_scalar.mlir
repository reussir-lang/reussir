// RUN: %reussir-opt %s --reussir-lowering-basic-ops | %FileCheck %s

module attributes {llvm.target_triple = "x86_64-pc-windows-gnullvm"} {
  llvm.func @id_i64(%arg0: i64) -> i64 {
    llvm.return %arg0 : i64
  }

  reussir.trampoline "C" @id_i64_ffi = @id_i64
}

// CHECK: llvm.func @id_i64_ffi({{.*}}i64{{.*}}) -> i64
// CHECK-NOT: llvm.byval
// CHECK: %[[R:.*]] = llvm.call @id_i64(%{{.*}}) : (i64) -> i64
// CHECK: llvm.return %[[R]] : i64
