// RUN: %reussir-opt %s -verify-diagnostics

!pair = !reussir.record<compound "Pair" [value] { i32, i64 }>
!sum = !reussir.record<variant "Sum" {!pair}>

module {
  func.func @bad(%a: i32, %b: i64) {
    %0 = reussir.record.compound(%a, %b : i32, i64) : !pair
    // expected-error @+1 {{expected exactly one payload form: either a value or compound fields}}
    %1 = "reussir.rc.create_variant"(%0, %a, %b) <{operandSegmentSizes = array<i32: 1, 2, 0, 0>, tag = 0 : index}> : (!pair, i32, i64) -> !reussir.rc<!sum>
    return
  }
}
