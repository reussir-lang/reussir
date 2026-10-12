// `reussir-sccp` runs SCCP across calls while the sum over callees of the
// square of their call sites is within `max-call-site-pairs`, and on each
// function alone otherwise. @callee has two call sites (4 pairs); the three
// calls of the declaration @log do not count.

// RUN: %reussir-opt %s --reussir-sccp | %FileCheck %s --check-prefix=MODULE
// RUN: %reussir-opt %s '--reussir-sccp=max-call-site-pairs=4' | %FileCheck %s --check-prefix=MODULE
// RUN: %reussir-opt %s '--reussir-sccp=max-call-site-pairs=3' | %FileCheck %s --check-prefix=FUNCTION
// RUN: %reussir-opt %s '--reussir-sccp=max-call-site-pairs=0' | %FileCheck %s --check-prefix=FUNCTION
// RUN: %reussir-opt %s '--reussir-sccp=max-call-site-pairs=18446744073709551615' | %FileCheck %s --check-prefix=MODULE

module {
  // Across calls, the argument is 41 at both call sites and the result 42.
  // MODULE-LABEL: func.func private @callee
  // MODULE: %[[C42:.*]] = arith.constant 42 : i64
  // MODULE: return %[[C42]]
  // FUNCTION-LABEL: func.func private @callee
  // FUNCTION: arith.addi
  func.func private @callee(%x: i64) -> i64 {
    %one = arith.constant 1 : i64
    %r = arith.addi %x, %one : i64
    return %r : i64
  }

  // MODULE-LABEL: func.func @first
  // MODULE: %[[C42:.*]] = arith.constant 42 : i64
  // MODULE: return %[[C42]]
  // FUNCTION-LABEL: func.func @first
  // FUNCTION: %[[R:.*]] = call @callee
  // FUNCTION: return %[[R]]
  func.func private @log(i64)

  func.func @first() -> i64 {
    %c = arith.constant 41 : i64
    func.call @log(%c) : (i64) -> ()
    func.call @log(%c) : (i64) -> ()
    func.call @log(%c) : (i64) -> ()
    %r = func.call @callee(%c) : (i64) -> i64
    return %r : i64
  }

  // Within a function the folding happens either way.
  // MODULE-LABEL: func.func @second
  // FUNCTION-LABEL: func.func @second
  // FUNCTION: %[[C43:.*]] = arith.constant 43 : i64
  // FUNCTION: call @callee(%{{.*}})
  // FUNCTION: return
  func.func @second() -> i64 {
    %c = arith.constant 41 : i64
    %two = arith.constant 2 : i64
    %d = arith.addi %c, %two : i64
    %r = func.call @callee(%c) : (i64) -> i64
    %s = arith.addi %r, %d : i64
    return %s : i64
  }
}
