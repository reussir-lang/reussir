// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

// expected-error @+1 {{'reussir.borrowed' argument #0 must be a shared !reussir.rc}}
func.func private @scalar(%x: i64 {reussir.borrowed = #reussir.borrowed}) -> i64 {
  return %x : i64
}

// -----

// expected-error @+1 {{'reussir.borrowed' must be #reussir.borrowed}}
func.func private @valued(%x: !reussir.rc<i64> {reussir.borrowed = 1 : i64}) {
  return
}

// -----

// expected-error @+1 {{'reussir.borrowed' requires a non-public function}}
func.func @public(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}) {
  return
}

// -----

// expected-error @+1 {{'reussir.borrowed' requires a function body}}
func.func private @declaration(%x: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed})
