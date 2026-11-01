// RUN: %reussir-opt %s --split-input-file --verify-diagnostics
func.func @rc_missing(%box: !reussir.rc<i64>) {
  // expected-error @+1 {{requires a release function symbol}}
  reussir.rc.defer @absent(%box : !reussir.rc<i64>)
  return
}
// -----
func.func private @bad(%box: !reussir.rc<i32>)
func.func @rc_mismatch(%box: !reussir.rc<i64>) {
  // expected-error @+1 {{release function must match the operands and return nothing}}
  reussir.rc.defer @bad(%box : !reussir.rc<i64>)
  return
}
// -----
func.func private @release(%box: !reussir.rc<i64 flex>)
func.func @rc_flex(%box: !reussir.rc<i64 flex>) {
  // expected-error @+1 {{requires a shared RC box}}
  reussir.rc.defer @release(%box : !reussir.rc<i64 flex>)
  return
}
// -----
func.func @join_missing(%arg: !reussir.ref<i64>) {
  // expected-error @+1 {{requires a release function symbol}}
  reussir.drop.join @absent(%arg : !reussir.ref<i64>)
  return
}
// -----
func.func @missing(%arg: !reussir.ref<i64>) {
  // expected-error @+1 {{requires a release function symbol}}
  reussir.drop.run @absent(%arg : !reussir.ref<i64>)
  return
}
// -----
func.func private @bad(%arg: !reussir.ref<i64>) -> i64
func.func @result(%arg: !reussir.ref<i64>) {
  // expected-error @+1 {{release function must match the operands and return nothing}}
  reussir.drop.defer @bad(%arg : !reussir.ref<i64>)
  return
}
// -----
func.func private @bad(%arg: i64)
func.func @object(%arg: i64) {
  // expected-error @+1 {{requires a reference or RC pointer}}
  reussir.drop.run @bad(%arg : i64)
  return
}
// -----
func.func private @bad(%arg: !reussir.ref<i64>)
func.func @indexed(%arg: !reussir.ref<i64>) {
  %zero = arith.constant 0 : index
  // expected-error @+1 {{release function must match the operands and return nothing}}
  reussir.drop.defer_indexed @bad(%arg : !reussir.ref<i64>, %zero)
  return
}
