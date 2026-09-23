// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-convert-to-std,control-flow-sink,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' \
// RUN:   | %FileCheck %s

// Construction: the instantiated token computes `header + n * elemsize`,
// allocation takes the generic entry point with the runtime size, and the
// header stores are canonical (offset 0, size n, stride 1).

!dv = !reussir.array<? x i32>
!rc_dv = !reussir.rc<!dv>

module {
  // CHECK-LABEL: llvm.func @make
  // CHECK-DAG: %[[C4:.+]] = llvm.mlir.constant(4 : index) : i64
  // CHECK-DAG: %[[C32:.+]] = llvm.mlir.constant(32 : index) : i64
  // CHECK: %[[BYTES0:.+]] = llvm.mul %arg0, %[[C4]]
  // CHECK: %[[BYTES:.+]] = llvm.add %[[BYTES0]], %[[C32]]
  // CHECK: %[[TOK:.+]] = llvm.call @__reussir_allocate(%{{.+}}, %[[BYTES]])
  // CHECK-DAG: llvm.getelementptr %[[TOK]][0, 1]
  // CHECK-DAG: llvm.getelementptr %[[TOK]][0, 2]
  // CHECK-DAG: llvm.getelementptr %[[TOK]][0, 3]
  // CHECK-DAG: llvm.getelementptr %[[TOK]][0, 0]
  func.func @make(%n: index) -> !rc_dv {
    %init = arith.constant 42 : i32
    %rc = reussir.array.create init(%init : i32) extents(%n) : !rc_dv
    return %rc : !rc_dv
  }
}
