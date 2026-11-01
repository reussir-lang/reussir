// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse))' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop.run

!inner = !reussir.rc<i64 atomic>
!cell = !reussir.cell<!inner mutex>
!rc = !reussir.rc<!cell atomic>
!token = !reussir.nullable<!reussir.token<align: 8, size: 24>>

module {
  // An ordinary protected release completes before its caller continues.
  // CHECK-LABEL: func.func private @__reussir_locked_drop.forward
  // CHECK: reussir.rc.fetch_sub
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop.run
  // CHECK: return
  // CHECK-LABEL: func.func @public_payload
  // CHECK: sync.mutex.critical_section
  // CHECK: call @__reussir_locked_drop.forward
  // CHECK-LABEL: func.func @owned_cell
  // CHECK: reussir.rc.fetch_sub
  // CHECK: sync.mutex.critical_section
  // CHECK: call @__reussir_locked_drop.forward
  // CHECK: reussir.token.free
  func.func @public_payload(%ref: !reussir.ref<!cell unspecified atomic>) {
    reussir.ref.drop(%ref : !reussir.ref<!cell unspecified atomic>)
    return
  }
  func.func @owned_cell(%rc: !rc) {
    %token = reussir.rc.dec(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }
}
