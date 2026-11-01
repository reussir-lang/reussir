// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s

!node_ = !reussir.record<compound "Node" incomplete>
!node = !reussir.record<compound "Node" { i64, !reussir.nullable<!reussir.rc<!node_ shared>> }>
!rc = !reussir.rc<!node shared>
!token = !reussir.nullable<!reussir.token<align: 8, size: 24>>

module attributes {reussir.drop_order = "backward"} {
  // CHECK-NOT: external_release.worklist
  // CHECK: func.func private @external_release(
  func.func private @external_release(!rc) attributes {reussir.foreign_drop}

  // CHECK-LABEL: func.func private @bridge.worklist.backward
  // CHECK: reussir.rc.fetch
  // CHECK: reussir.rc.defer
  // CHECK-NOT: reussir.drop.run
  // CHECK: return
  // CHECK-LABEL: func.func @bridge
  // CHECK-NEXT: reussir.drop.join @bridge.worklist.backward
  // CHECK-NEXT: return
  func.func @bridge(%rc: !rc) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }

  // Public entry points finish their own work before returning, even when
  // called by a foreign hook inside another drain.
  // CHECK-LABEL: func.func @public
  // CHECK: reussir.drop.run @__reussir_drop.release.backward
  func.func @public(%rc: !rc) {
    %token = reussir.rc.dec(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }
}
