// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s

!arr = !reussir.array<2 x !reussir.rc<i64>>

module {
  func.func @drop_array(%ref : !reussir.ref<!arr>) {
    reussir.ref.drop(%ref : !reussir.ref<!arr>)
    return
  }
}

// CHECK-LABEL: func.func @drop_array(
// CHECK-DAG: %[[IDX0:.+]] = arith.constant 0 : index
// CHECK-DAG: %[[IDX1:.+]] = arith.constant 1 : index
// CHECK: %[[R0:.+]] = reussir.array.project(%arg0 : !reussir.ref<!reussir.array<2 x !reussir.rc<i64>>>, %[[IDX0]] : index) : !reussir.ref<!reussir.rc<i64>>
// CHECK: %[[V0:.+]] = reussir.ref.load(%[[R0]] : !reussir.ref<!reussir.rc<i64>>) : !reussir.rc<i64>
// CHECK: reussir.rc.dec(%[[V0]] : !reussir.rc<i64>)
// CHECK: %[[R1:.+]] = reussir.array.project(%arg0 : !reussir.ref<!reussir.array<2 x !reussir.rc<i64>>>, %[[IDX1]] : index) : !reussir.ref<!reussir.rc<i64>>
// CHECK: %[[V1:.+]] = reussir.ref.load(%[[R1]] : !reussir.ref<!reussir.rc<i64>>) : !reussir.rc<i64>
// CHECK: reussir.rc.dec(%[[V1]] : !reussir.rc<i64>)
