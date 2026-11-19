// REQUIRES: rcgmp
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --implicit-check-not='{{(two|nest|val)}}.worklist'
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s --implicit-check-not='{{(two|nest|val)}}.worklist'

// A foreign drop of a box of scalars and integers releases the integers in
// place and frees the box: it keeps its body, as a box of scalars does. A
// variant qualifies when every arm does. A box or a foreign object in the
// payload keeps the bridge.

!handle = !reussir.rc<!reussir.ffi_object<"Handle", @handle_drop>>
!two = !reussir.record<compound "Two" { !rcgmp.mpz, i64, !rcgmp.mpz }>
!inner = !reussir.record<compound "Inner" [value] { !rcgmp.mpz, f64 }>
!nest = !reussir.record<compound "Nest" { !inner, !inner }>
!vnat = !reussir.record<compound "Val::N" [value] { !rcgmp.mpz }>
!vnone = !reussir.record<compound "Val::None" [value] { }>
!val = !reussir.record<variant "Val" { !vnone, !vnat }>
!leaf = !reussir.record<compound "Leaf" { i64 }>
!mbox = !reussir.record<compound "Mixed::B" [value] { !leaf }>
!mixed = !reussir.record<variant "Mixed" { !vnat, !mbox }>
!held = !reussir.record<compound "Held" { !rcgmp.mpz, !leaf }>
!owner = !reussir.record<compound "Owner" { !rcgmp.mpz, !handle }>
!t24 = !reussir.nullable<!reussir.token<align: 8, size: 24>>
!t32 = !reussir.nullable<!reussir.token<align: 8, size: 32>>
!t40 = !reussir.nullable<!reussir.token<align: 8, size: 40>>
!dyn = !reussir.nullable<!reussir.token<align: 8, size: ?>>
module {
  func.func private @handle_drop(!handle)

  // CHECK-LABEL: func.func @two(
  // CHECK-NOT: reussir.drop
  // CHECK: call @{{.*}}drop_in_place
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @two(%rc: !reussir.rc<!two shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!two shared>) : !t32
    reussir.token.free(%token : !t32)
    return
  }

  // Value records in the payload are stored in line.
  // CHECK-LABEL: func.func @nest(
  // CHECK-NOT: reussir.drop
  // CHECK: call @{{.*}}drop_in_place
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @nest(%rc: !reussir.rc<!nest shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!nest shared>) : !t40
    reussir.token.free(%token : !t40)
    return
  }

  // CHECK-LABEL: func.func @val(
  // CHECK-NOT: reussir.drop
  // CHECK: call @{{.*}}drop_in_place
  // CHECK-NOT: reussir.drop
  // CHECK: return
  func.func @val(%rc: !reussir.rc<!val shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!val shared>) : !dyn
    reussir.token.free(%token : !dyn)
    return
  }

  // One arm of the variant holds a box.
  // CHECK-LABEL: func.func private @mixed.worklist.
  // CHECK-LABEL: func.func @mixed(
  // CHECK: reussir.drop.join @mixed.worklist.
  func.func @mixed(%rc: !reussir.rc<!mixed shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!mixed shared>) : !dyn
    reussir.token.free(%token : !dyn)
    return
  }

  // CHECK-LABEL: func.func private @held.worklist.
  // CHECK-LABEL: func.func @held(
  // CHECK: reussir.drop.join @held.worklist.
  func.func @held(%rc: !reussir.rc<!held shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!held shared>) : !t24
    reussir.token.free(%token : !t24)
    return
  }

  // CHECK-LABEL: func.func private @owner.worklist.
  // CHECK-LABEL: func.func @owner(
  // CHECK: reussir.drop.join @owner.worklist.
  func.func @owner(%rc: !reussir.rc<!owner shared>) attributes {reussir.foreign_drop} {
    %token = reussir.rc.dec(%rc : !reussir.rc<!owner shared>) : !t24
    reussir.token.free(%token : !t24)
    return
  }
}
