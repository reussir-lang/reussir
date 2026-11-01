// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-rc-decrement-expansion{defer-normal=true},reussir-acquire-drop-expansion,func.func(reussir-token-reuse),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true})' --verify-each | %FileCheck %s

!inner = !reussir.rc<i64 atomic>
!cell = !reussir.cell<!inner mutex>
!rc = !reussir.rc<!cell atomic>
!token = !reussir.nullable<!reussir.token<align: 8, size: 24>>

module {
  // Scalar payloads are freed directly inside the protected callback.
  // CHECK-LABEL: func.func private @__reussir_locked_drop.worklist.forward
  // CHECK: reussir.rc.fetch_sub
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop.run
  // CHECK: return
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
  // CHECK: reussir.drop.run @__reussir_drop.release.forward
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
