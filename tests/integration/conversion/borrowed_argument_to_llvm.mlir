// RUN: %reussir-opt %s --reussir-convert-to-llvm | %FileCheck %s

// Lowering to LLVM drops `reussir.borrowed`: the convention ends with the
// explicit counts, and the parameter is no longer an rc.
module {
  // CHECK-LABEL: llvm.func @lowered(%{{.+}}: !llvm.ptr) -> i64
  // CHECK-NOT: reussir.borrowed
  // CHECK: llvm.return
  func.func private @lowered(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}) -> i64 {
    %c = arith.constant 0 : i64
    return %c : i64
  }
}
