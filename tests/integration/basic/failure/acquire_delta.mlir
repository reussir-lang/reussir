// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func @negative_rc(%rc: !reussir.rc<i32>) {
  %n = arith.constant -1 : index
  // expected-error @+1 {{acquisition delta must be non-negative}}
  reussir.rc.inc(%rc : !reussir.rc<i32>) by %n
  return
}
// -----
func.func @negative_ref(%ref: !reussir.ref<!reussir.rc<i32>>) {
  %n = arith.constant -1 : index
  // expected-error @+1 {{acquisition delta must be non-negative}}
  reussir.ref.acquire(%ref : !reussir.ref<!reussir.rc<i32>>) by %n
  return
}
