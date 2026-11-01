// REQUIRES: rcgmp
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true' | %FileCheck %s --check-prefix=OUTLINE

// An integer member is one word that retains and releases through rcgmp.

!pair = !reussir.record<compound "BigPair" [value] { !rcgmp.mpz, i64, !rcgmp.mpz }>
!node_ = !reussir.record<variant "BigList" incomplete>
!nil = !reussir.record<compound "BigList::Nil" [value] { }>
!cons = !reussir.record<compound "BigList::Cons" [value] { !rcgmp.mpz, !node_ }>
!node = !reussir.record<variant "BigList" { !nil, !cons }>

module {
  // CHECK-LABEL: func.func @acquire_pair
  // CHECK: %[[LO:.*]] = reussir.ref.load
  // CHECK: rcgmp.retain %[[LO]] : !rcgmp.mpz
  // CHECK-NOT: rcgmp.retain
  // CHECK: %[[HI:.*]] = reussir.ref.load
  // CHECK: rcgmp.retain %[[HI]] : !rcgmp.mpz
  // CHECK: return
  func.func @acquire_pair(%ref: !reussir.ref<!pair>) {
    reussir.ref.acquire(%ref : !reussir.ref<!pair>)
    return
  }

  // A run-time delta loops over single retains; a constant one unrolls.
  // CHECK-LABEL: func.func @acquire_delta
  // CHECK: scf.for
  // CHECK: rcgmp.retain
  // CHECK-LABEL: func.func @acquire_twice
  // CHECK-COUNT-2: rcgmp.retain
  // CHECK-NOT: rcgmp.retain
  func.func @acquire_delta(%ref: !reussir.ref<!rcgmp.mpz>, %n: index) {
    reussir.ref.acquire(%ref : !reussir.ref<!rcgmp.mpz>) by %n
    return
  }
  func.func @acquire_twice(%ref: !reussir.ref<!rcgmp.mpz>) {
    %c2 = arith.constant 2 : index
    reussir.ref.acquire(%ref : !reussir.ref<!rcgmp.mpz>) by %c2
    return
  }

  // CHECK-LABEL: func.func @drop_pair
  // CHECK-COUNT-2: rcgmp.release
  // CHECK: return
  func.func @drop_pair(%ref: !reussir.ref<!pair>) {
    reussir.ref.drop(%ref : !reussir.ref<!pair>)
    return
  }

  // CHECK-LABEL: func.func @drop_cons
  // CHECK: reussir.record.coerce[1]
  // CHECK: %[[HEAD:.*]] = reussir.ref.load
  // CHECK: rcgmp.release %[[HEAD]] : !rcgmp.mpz
  // CHECK: reussir.rc.dec
  // OUTLINE: func.func private @[[MPZ_DROP:[^ (]+]](%{{.*}}: !reussir.ref<!rcgmp.mpz>)
  // OUTLINE: rcgmp.release
  // OUTLINE: func.func private @[[CONS_DROP:_RNvC[0-9]+REUSSIR_DROP_walk_forward[A-Za-z0-9_]+]](%{{.*}}: !reussir.ref<!reussir.record<compound "BigList::Cons"
  // OUTLINE: reussir.drop.defer @[[MPZ_DROP]]
  // OUTLINE-LABEL: func.func @drop_cons
  // OUTLINE: reussir.drop.run @[[CONS_DROP]]
  func.func @drop_cons(%ref: !reussir.ref<!node>) {
    reussir.ref.drop(%ref : !reussir.ref<!node>) variant [1]
    return
  }
}
