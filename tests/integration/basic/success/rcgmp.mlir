// REQUIRES: rcgmp
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

// rcgmp integers sit beside Reussir's own types in one module.
module {
  func.func @scale(%x: !rcgmp.mpz, %rc: !reussir.rc<i64>) -> !rcgmp.mpz {
    %big = rcgmp.constant 340282366920938463463374607431768211456 : !rcgmp.mpz
    rcgmp.retain %x : !rcgmp.mpz
    %twice = rcgmp.addi %x, %x : !rcgmp.mpz
    %r = rcgmp.muli %twice, %big : !rcgmp.mpz
    reussir.rc.dec (%rc : !reussir.rc<i64>)
    return %r : !rcgmp.mpz
  }
}

// CHECK-LABEL: func.func @scale
// CHECK-SAME: (%[[X:.+]]: !rcgmp.mpz, %{{.+}}: !reussir.rc<i64>) -> !rcgmp.mpz
// CHECK: %[[BIG:.+]] = rcgmp.constant 340282366920938463463374607431768211456 : !rcgmp.mpz
// CHECK: rcgmp.retain %[[X]] : !rcgmp.mpz
// CHECK: %[[TWICE:.+]] = rcgmp.addi %[[X]], %[[X]] : !rcgmp.mpz
// CHECK: rcgmp.muli %[[TWICE]], %[[BIG]] : !rcgmp.mpz
