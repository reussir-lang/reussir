// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics
func.func @bad_input(%input: i32) {
  // expected-error @+1 {{requires IFRT arrays and a control result}}
  %ready = reussir.pjrt.after %input : (i32) -> !ifrt.control
  return
}
// -----
func.func @bad_control() {
  // expected-error @+1 {{requires IFRT arrays and a control result}}
  %ready = reussir.pjrt.after : () -> i1
  return
}
