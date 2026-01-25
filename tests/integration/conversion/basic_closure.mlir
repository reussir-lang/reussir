// RUN: %reussir-opt \
// RUN:   -reussir-closure-outlining \
// RUN:   -convert-scf-to-cf \
// RUN:   -reussir-lowering-basic-ops \
// RUN:   -reconcile-unrealized-casts \
// RUN:   -o %t.mlir %s
// RUN: %FileCheck %s --check-prefix=CHECK-MLIR < %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | \
// RUN:   %FileCheck %s --check-prefix=CHECK-LLVM
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | \
// RUN:   %opt -S -O3 | \
// RUN:   %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:    %rpath_flag %extra_sys_libs
// RUN: %t.exe

// Test basic closure creation, application, and evaluation
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // Helper function that creates and returns a closure
  func.func private @create_add_one_closure() -> !reussir.rc<!reussir.closure<(i32) -> i32>> attributes { llvm.linkage = #llvm.linkage<internal> } {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 32>
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
      token(%token : !reussir.token<align: 8, size: 32>)
      body {
        ^bb0(%v0 : i32):
          %one = arith.constant 1 : i32
          %add = arith.addi %v0, %one : i32 
          reussir.closure.yield %add : i32
      }
    }
    return %closure : !reussir.rc<!reussir.closure<(i32) -> i32>>
  }

  // Main function: create closure, apply 41, eval should give 42, return 42-42=0
  func.func @main() -> i32 {
    %closure = func.call @create_add_one_closure() : () -> !reussir.rc<!reussir.closure<(i32) -> i32>>
    %c41 = arith.constant 41 : i32
    %c42 = arith.constant 42 : i32
    %applied = reussir.closure.apply (%c41 : i32) to (%closure : !reussir.rc<!reussir.closure<(i32) -> i32>>) : !reussir.rc<!reussir.closure<() -> i32>>
    %evaluated = reussir.closure.eval (%applied : !reussir.rc<!reussir.closure<() -> i32>>) : i32
    %res = arith.subi %evaluated, %c42 : i32
    func.return %res : i32
  }
}

// CHECK-MLIR-DAG: llvm.func @__reussir_allocate
// CHECK-MLIR-DAG: llvm.call @__reussir_allocate

// CHECK-LLVM-DAG: declare ptr @__reussir_allocate
// CHECK-LLVM-DAG: call ptr @__reussir_allocate

