// RUN: %reussir-opt %s --reussir-instrument-coverage \
// RUN:   --reussir-emit-coverage-mapping --symbol-dce \
// RUN:   --reussir-emit-coverage-unused-names | %FileCheck %s
// RUN: %reussir-opt %s --reussir-instrument-coverage \
// RUN:   --reussir-emit-coverage-mapping --symbol-dce \
// RUN:   --reussir-emit-coverage-unused-names --convert-to-llvm -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir -o %t.ll
// RUN: %opt -S -passes=instrprof -enable-name-compression=false %t.ll \
// RUN:   | %FileCheck %s --check-prefix=IR

// `symbol-dce` removes `@dead`, but `llvm.used` keeps its record. The list
// has only the name of `@dead`: the lowering fails on a name that an
// increment uses. The lowering then writes both names.

// CHECK-NOT: func.func private @dead
// CHECK: llvm.mlir.global internal constant @__llvm_coverage_names()
// CHECK-SAME: !llvm.array<1 x ptr>
// CHECK-NEXT: llvm.mlir.poison
// CHECK-NEXT: llvm.mlir.addressof @__profn_dead
// CHECK-NOT: llvm.mlir.addressof
// CHECK: llvm.return

// IR-NOT: __llvm_coverage_names
// IR: @__llvm_prf_nm = private constant [11 x i8] c"\09\00live\01dead", section "__llvm_prf_names"

module attributes {llvm.target_triple = "x86_64-unknown-linux-gnu"} {
  func.func @live() -> i32 {
    %c = arith.constant 0 : i32
    return %c : i32
  } loc(#live)
  func.func private @dead() -> i32 {
    %c = arith.constant 1 : i32
    return %c : i32
  } loc(#dead)
}
#live = loc("a.rr":1:1 to 3:2)
#dead = loc("a.rr":5:1 to 7:2)
