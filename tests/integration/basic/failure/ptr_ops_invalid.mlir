// RUN: %reussir-opt %s -verify-diagnostics

func.func @addr_of_integer(%p: i64) -> i64 {
  // expected-error @+1 {{operand #0 must be opaque pointer, but got 'i64'}}
  %addr = reussir.ptr.addr %p : i64 -> i64
  return %addr : i64
}
