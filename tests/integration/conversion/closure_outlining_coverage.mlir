// RUN: %reussir-opt %s --reussir-closure-outlining | %FileCheck %s

// The counter of a closure body counts the runs of the closure. Outlining
// moves the increment into the evaluate function, and that function takes the
// coverage scope of the closure. The clone and drop functions have no source
// code, so they get no increment and coverage is off for them.

!closure = !reussir.closure<(i32) -> i32>
!token = !reussir.token<align: 8, size: 32>

module @test attributes {
  dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>,
  reussir.coverage.include = [], reussir.coverage.exclude = []
} {
  llvm.mlir.global private constant @__profn_make("make") {addr_space = 0 : i32} : !llvm.array<4 x i8>

  func.func @make() -> !reussir.rc<!closure> attributes {reussir.coverage = true} {
    %name = llvm.mlir.addressof @__profn_make : !llvm.ptr
    %hash = llvm.mlir.constant(7 : i64) : i64
    %num = llvm.mlir.constant(2 : i32) : i32
    %entry = llvm.mlir.constant(0 : i32) : i32
    llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %entry) : (!llvm.ptr, i64, i32, i32) -> ()
    %token = reussir.token.alloc : !token
    %closure = "reussir.closure.create"(%token) ({
    ^bb0(%x: i32):
      %body_name = llvm.mlir.addressof @__profn_make : !llvm.ptr
      %body_hash = llvm.mlir.constant(7 : i64) : i64
      %body_num = llvm.mlir.constant(2 : i32) : i32
      %body = llvm.mlir.constant(1 : i32) : i32
      llvm.call_intrinsic "llvm.instrprof.increment"(%body_name, %body_hash, %body_num, %body) : (!llvm.ptr, i64, i32, i32) -> ()
      %one = arith.constant 1 : i32
      %sum = arith.addi %x, %one : i32
      reussir.closure.yield %sum : i32
    }) {reussir.coverage = true, reussir.coverage_origin = "_RNvC4test4make"} : (!token) -> !reussir.rc<!closure>
    return %closure : !reussir.rc<!closure>
  }
}

// CHECK-LABEL: func.func @make()
// CHECK: %[[ENTRY:.*]] = llvm.mlir.constant(0 : i32) : i32
// CHECK: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.*}}, %{{.*}}, %{{.*}}, %[[ENTRY]])
// CHECK-NOT: llvm.call_intrinsic
// CHECK: return

// CHECK: func.func private @{{.*}}8evaluate(
// CHECK-SAME: ["reussir-coverage", "on"]
// CHECK-SAME: reussir.coverage = true
// CHECK-SAME: reussir.coverage_origin = "_RNvC4test4make"
// CHECK: llvm.mlir.addressof @__profn_make
// CHECK: %[[BODY:.*]] = llvm.mlir.constant(1 : i32) : i32
// CHECK: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.*}}, %{{.*}}, %{{.*}}, %[[BODY]])
// CHECK: arith.addi
// CHECK: return

// CHECK: func.func private @{{.*}}5clone(
// CHECK-SAME: "nosanitize_coverage"
// CHECK-SAME: reussir.coverage = false
// CHECK-NOT: llvm.call_intrinsic
// CHECK: func.func private @{{.*}}4drop(
// CHECK-SAME: "nosanitize_coverage"
// CHECK-SAME: reussir.coverage = false
// CHECK-NOT: llvm.call_intrinsic
// CHECK: reussir.closure.vtable
