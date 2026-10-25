// REQUIRES: rcgmp
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-rc-decrement-expansion,reussir-acquire-drop-expansion{expand-decrement=1},func.func(reussir-inc-dec-cancellation))' | %FileCheck %s

// An integer member retained before its node is released cancels like an rc
// member: on the unique path the node's release would drop the member, so the
// retain moves into the shared path and that release disappears.

!node = !reussir.record<compound "Node" { !rcgmp.mpz, i64 }>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> } {
  func.func private @opaque()

  // CHECK-LABEL: func.func @keep_member
  // CHECK: %[[K:.+]] = reussir.ref.load
  // CHECK-NOT: rcgmp.retain
  // CHECK: scf.if
  // CHECK-NOT: rcgmp.release
  // CHECK: } else {
  // CHECK: rcgmp.retain %[[K]]
  func.func @keep_member(%n : !reussir.rc<!node>) -> !rcgmp.mpz {
    %b = reussir.rc.borrow(%n : !reussir.rc<!node>) : !reussir.ref<!node shared>
    %p = reussir.ref.project(%b : !reussir.ref<!node shared>) [0] : !reussir.ref<!rcgmp.mpz shared>
    %k = reussir.ref.load(%p : !reussir.ref<!rcgmp.mpz shared>) : !rcgmp.mpz
    rcgmp.retain %k : !rcgmp.mpz
    %t = reussir.rc.dec(%n : !reussir.rc<!node>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %k : !rcgmp.mpz
  }

  // A call between the retain and the release may consume the member, so
  // the pair stays.
  // CHECK-LABEL: func.func @call_blocks_cancellation
  // CHECK: rcgmp.retain
  // CHECK: call @opaque
  // CHECK: scf.if
  // CHECK: rcgmp.release
  func.func @call_blocks_cancellation(%n : !reussir.rc<!node>) -> !rcgmp.mpz {
    %b = reussir.rc.borrow(%n : !reussir.rc<!node>) : !reussir.ref<!node shared>
    %p = reussir.ref.project(%b : !reussir.ref<!node shared>) [0] : !reussir.ref<!rcgmp.mpz shared>
    %k = reussir.ref.load(%p : !reussir.ref<!rcgmp.mpz shared>) : !rcgmp.mpz
    rcgmp.retain %k : !rcgmp.mpz
    func.call @opaque() : () -> ()
    %t = reussir.rc.dec(%n : !reussir.rc<!node>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %k : !rcgmp.mpz
  }
}
