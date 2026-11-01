// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward tail-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=NOTAIL --implicit-check-not=reussir.musttail
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --reussir-convert-to-std --canonicalize --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize --verify-each -o %t.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.mlir | %opt -passes=verify -S | %FileCheck %s --check-prefix=LLVM

!foreign = !reussir.rc<!reussir.ffi_object<"Capture", @capture_drop>>
!plain = !reussir.rc<i64>
!closure = !reussir.rc<!reussir.closure<(!foreign, !plain) -> i64>>

module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func private @capture_drop(!foreign)
  func.func @create() -> !closure {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 40>
    %closure = reussir.closure.create -> !closure {
      token(%token : !reussir.token<align: 8, size: 40>)
      body {
      ^bb0(%first: !foreign, %second: !plain):
        reussir.rc.dec(%first : !foreign) cleanup(@capture_drop)
        reussir.rc.dec(%second : !plain)
        %zero = arith.constant 0 : i64
        reussir.closure.yield %zero : i64
      }
    }
    return %closure : !closure
  }
}

// Later captures stay pending until the first capture's cleanup finishes.
// CHECK-LABEL: func.func private @{{.*}}drop.worklist.forward
// CHECK: reussir.drop.defer @_RNvC{{[0-9]+}}REUSSIR_DROP_free_forward{{[A-Za-z0-9_]+}}
// CHECK: scf.if
// CHECK-NEXT: reussir.drop.defer @_RNvC{{[0-9]+}}REUSSIR_DROP_walk_forward{{[A-Za-z0-9_]+}}
// CHECK: scf.if
// CHECK-NEXT: func.call @_RNvC{{[0-9]+}}REUSSIR_DROP_walk_forward{{[A-Za-z0-9_]+}}{{.*}}reussir.musttail
// CHECK-NOT: reussir.drop.defer
// CHECK: return
// NOTAIL-LABEL: func.func private @{{.*}}drop.worklist.forward
// NOTAIL: func.call @_RNvC{{[0-9]+}}REUSSIR_DROP_walk_forward{{[A-Za-z0-9_]+}}
// LLVM: musttail call void @_RNvC{{[0-9]+}}REUSSIR_DROP_walk_forward{{[A-Za-z0-9_]+}}
// LLVM-NEXT: ret void
