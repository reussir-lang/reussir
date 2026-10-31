// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

// A borrowed parameter round-trips as a unit argument attribute.
module {
  // CHECK-LABEL: func.func private @read(
  // CHECK-SAME: %{{.+}}: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}, %{{.+}}: !reussir.rc<i64>)
  func.func private @read(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}, %y: !reussir.rc<i64>) -> i64 {
    %bx = reussir.rc.borrow(%x : !reussir.rc<i64>) : !reussir.ref<i64 shared>
    %v = reussir.ref.load(%bx : !reussir.ref<i64 shared>) : i64
    reussir.rc.dec(%y : !reussir.rc<i64>)
    return %v : i64
  }
}
