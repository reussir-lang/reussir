// RUN: %reussir-opt %s -verify-diagnostics

!arr = !reussir.array<2 x i64>

module {
  func.func private @extract_oob(%arr : !arr) -> i64 {
    // expected-error @+1 {{'reussir.array.extract' op index out of bounds: 2 >= 2}}
    %elt = reussir.array.extract(%arr : !arr)[2] : i64
    return %elt : i64
  }
}
