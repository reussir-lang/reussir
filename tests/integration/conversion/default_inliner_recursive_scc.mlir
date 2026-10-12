// The default inliner limits recursive expansion: a call into a recursive
// SCC that an inlining copied in is not inlined in turn. Before, the inliner
// followed the copied calls through every chain of distinct members of the
// SCC (it only stops a chain when a callee repeats on it).

// RUN: %reussir-opt %s --reussir-default-inliner | %FileCheck %s --implicit-check-not=reussir.inliner_home

module {
  // @a -> @b -> @c -> @a: one recursive SCC of small functions.
  func.func private @a(%x: i64) -> i64 {
    %zero = arith.constant 0 : i64
    %one = arith.constant 1 : i64
    %done = arith.cmpi eq, %x, %zero : i64
    %r = scf.if %done -> (i64) {
      scf.yield %zero : i64
    } else {
      %y = arith.subi %x, %one : i64
      %z = func.call @b(%y) : (i64) -> i64
      scf.yield %z : i64
    }
    return %r : i64
  }
  func.func private @b(%x: i64) -> i64 {
    %two = arith.constant 2 : i64
    %y = arith.muli %x, %two : i64
    %z = func.call @c(%y) : (i64) -> i64
    return %z : i64
  }
  func.func private @c(%x: i64) -> i64 {
    %three = arith.constant 3 : i64
    %y = arith.divui %x, %three : i64
    %z = func.call @a(%y) : (i64) -> i64
    return %z : i64
  }

  // Every call the program wrote is inlined once (@c, then @a, have one use
  // left and move into their caller), and every call that came with an
  // inlined body stays a call: @b keeps one copy of each member and its
  // call to itself, and @main one copy of @a and of what @a holds.
  // CHECK-LABEL: func.func private @b
  // CHECK: arith.muli
  // CHECK: arith.divui
  // CHECK: arith.cmpi
  // CHECK-NOT: arith.cmpi
  // CHECK: call @b
  // CHECK-NOT: call
  // CHECK: return
  // CHECK-LABEL: func.func @main
  // CHECK: arith.cmpi
  // CHECK: arith.muli
  // CHECK: arith.divui
  // CHECK: arith.cmpi
  // CHECK-NOT: arith.cmpi
  // CHECK: call @b
  // CHECK-NOT: call
  // CHECK: return
  func.func @main(%x: i64) -> i64 {
    %r = func.call @a(%x) : (i64) -> i64
    return %r : i64
  }
}
