// RUN: %reussir-opt %s -verify-diagnostics

!arr = !reussir.array<2 x i64>

module {
  func.func private @insert_type_mismatch(%arr : !arr, %value : i32) -> !arr {
    // expected-error @+1 {{'reussir.array.insert' op value type must match array element type, got 'i32' but expected 'i64'}}
    %updated = reussir.array.insert(%arr : !arr)[0](%value : i32) : !arr
    return %updated : !arr
  }
}
