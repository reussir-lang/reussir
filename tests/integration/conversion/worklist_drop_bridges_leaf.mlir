// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --implicit-check-not='{{(leaf|pair|shape|atomic)}}.worklist'
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s --implicit-check-not='{{(leaf|pair|shape|atomic)}}.worklist'

// A foreign drop of a box whose payload is trivially copyable only frees the
// box: it keeps its body and enters no destruction scope. A payload with a
// managed member, or a regional box, still gets the bridge.

!leaf = !reussir.record<compound "Leaf" { i64, f64 }>
!point = !reussir.record<compound "Point" [value] { i64, f64 }>
!pair = !reussir.record<compound "Pair" { !point, !point }>
!shape = !reussir.record<variant "Shape" { !point, !reussir.record<compound "Shape::Empty" [value] {}> }>
!foreign = !reussir.rc<!reussir.ffi_object<"Handle", @handle_drop>>
!held = !reussir.record<compound "Held" { i64, !leaf }>
!owner = !reussir.record<compound "Owner" { i64, !foreign }>
!token24 = !reussir.nullable<!reussir.token<align: 8, size: 24>>
!token40 = !reussir.nullable<!reussir.token<align: 8, size: 40>>
!dynamic = !reussir.nullable<!reussir.token<align: 8, size: ?>>

module {
  func.func private @handle_drop(!foreign)

  // CHECK-LABEL: func.func @leaf(
  // CHECK-NOT: reussir.drop
  // CHECK: reussir.rc.fetch
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @leaf(%rc: !reussir.rc<!leaf shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!leaf shared>) : !token24
    reussir.token.free(%token : !token24)
    return
  }

  // Value records in the payload are stored in line.
  // CHECK-LABEL: func.func @pair(
  // CHECK-NOT: reussir.drop
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @pair(%rc: !reussir.rc<!pair shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!pair shared>) : !token40
    reussir.token.free(%token : !token40)
    return
  }

  // CHECK-LABEL: func.func @shape(
  // CHECK-NOT: reussir.drop
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @shape(%rc: !reussir.rc<!shape shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!shape shared>) : !dynamic
    reussir.token.free(%token : !dynamic)
    return
  }

  // CHECK-LABEL: func.func @atomic(
  // CHECK-NOT: reussir.drop
  // CHECK: reussir.rc.fetch_sub
  // CHECK: reussir.token.free
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @atomic(%rc: !reussir.rc<!leaf shared atomic>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!leaf shared atomic>) : !token24
    reussir.token.free(%token : !token24)
    return
  }

  // A record member of a shared record type is a link to another box.
  // CHECK-LABEL: func.func private @held.worklist.
  // CHECK-LABEL: func.func @held(
  // CHECK: reussir.drop.join @held.worklist.
  func.func @held(%rc: !reussir.rc<!held shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!held shared>) : !token24
    reussir.token.free(%token : !token24)
    return
  }

  // CHECK-LABEL: func.func private @owner.worklist.
  // CHECK-LABEL: func.func @owner(
  // CHECK: reussir.drop.join @owner.worklist.
  func.func @owner(%rc: !reussir.rc<!owner shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!owner shared>) : !token24
    reussir.token.free(%token : !token24)
    return
  }

  // CHECK-LABEL: func.func private @rigid.worklist.
  // CHECK-LABEL: func.func @rigid(
  // CHECK: reussir.drop.join @rigid.worklist.
  func.func @rigid(%rc: !reussir.rc<!leaf rigid>) attributes {reussir.foreign_drop} {
    reussir.rc.dec(%rc : !reussir.rc<!leaf rigid>)
    return
  }
}
