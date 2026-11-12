// RUN: %reussir-opt %s --verify-diagnostics

!pair = !reussir.record<compound "Pair" {i64, i64}>
func.func @destructure(%rc: !reussir.rc<!pair>) {
  // expected-error @+1 {{an unsafe decrement cannot destructure}}
  reussir.rc.dec(%rc : !reussir.rc<!pair>) {boundMembers = array<i64: 0>, unsafe}
  return
}
