// RUN: %reussir-opt %s --symbol-dce --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc --verify-each | %FileCheck %s
// CHECK-DAG: func.func private @release
// CHECK-DAG: func.func private @step
// CHECK-DAG: func.func private @release_box
// CHECK-DAG: func.func private @joined
// CHECK: reussir.rc.defer @release_box
// CHECK: reussir.drop.join @joined
// CHECK: reussir.drop.run @release
// CHECK: reussir.drop.defer @release
// CHECK: reussir.drop.defer_indexed @step
func.func private @release(%arg: !reussir.ref<i64>) {
  return
}
func.func private @step(%arg: !reussir.ref<i64>, %index: index) {
  return
}
func.func private @release_box(%box: !reussir.rc<i64>) {
  return
}
func.func private @joined(%arg: !reussir.ref<i64>) {
  return
}
func.func @callbacks(%arg: !reussir.ref<i64>, %box: !reussir.rc<i64>) {
  %zero = arith.constant 0 : index
  reussir.rc.defer @release_box(%box : !reussir.rc<i64>)
  reussir.drop.join @joined(%arg : !reussir.ref<i64>)
  reussir.drop.run @release(%arg : !reussir.ref<i64>)
  reussir.drop.defer @release(%arg : !reussir.ref<i64>)
  reussir.drop.defer_indexed @step(%arg : !reussir.ref<i64>, %zero)
  return
}
