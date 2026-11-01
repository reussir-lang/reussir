// RUN: %reussir-opt %s --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward tail-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=NOTAIL --implicit-check-not=reussir.musttail

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
// CHECK: reussir.drop.defer @__reussir_drop.free.forward
// CHECK: scf.if
// CHECK-NEXT: reussir.drop.defer @__reussir_drop.walk.forward
// CHECK: scf.if
// CHECK-NEXT: func.call @__reussir_drop.walk.forward{{.*}}reussir.musttail
// CHECK-NOT: reussir.drop.defer
// CHECK: return
// NOTAIL-LABEL: func.func private @{{.*}}drop.worklist.forward
// NOTAIL: func.call @__reussir_drop.walk.forward
