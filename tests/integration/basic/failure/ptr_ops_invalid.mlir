// RUN: %reussir-opt %s -verify-diagnostics -split-input-file

func.func @addr_of_integer(%p: i64) -> i64 {
  // expected-error @+1 {{operand #0 must be pointer type, but got 'i64'}}
  %addr = reussir.ptr.addr %p : i64 -> i64
  return %addr : i64
}

// -----

func.func @from_addr_to_integer(%a: i64) -> i64 {
  // expected-error @+1 {{result #0 must be pointer type, but got 'i64'}}
  %p = reussir.ptr.from_addr %a : i64 -> i64
  return %p : i64
}

// -----

func.func @mask_with_float(%p: !ptr.ptr<#ptr.generic_space>, %m: f64) -> !ptr.ptr<#ptr.generic_space> {
  // expected-error @+1 {{operand #1 must be signless integer or index, but got 'f64'}}
  %q = reussir.ptr.mask %p, %m : !ptr.ptr<#ptr.generic_space>, f64
  return %q : !ptr.ptr<#ptr.generic_space>
}
