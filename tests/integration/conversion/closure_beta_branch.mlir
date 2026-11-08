// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(func.func(reussir-closure-beta-reduction))' \
// RUN: | %FileCheck %s

// A fresh closure consumed in the arms of a branch, as an inlined
// `match self { Pass => next(), Fail(_) => self }` leaves it: the chain moves
// into the evaluating arm and is reduced there; the release in the other arm
// becomes the release of the captures.

!rc_i64 = !reussir.rc<i64>
!fn2 = !reussir.closure<(!rc_i64, i64) -> i64>
!fn1 = !reussir.closure<(i64) -> i64>
!fn0 = !reussir.closure<() -> i64>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @eval_or_drop(%cond: i1, %p: !rc_i64, %x: i64) -> i64 {
    %c = reussir.closure.create -> !reussir.rc<!fn2> {
      body {
        ^bb0(%owned: !rc_i64, %v: i64):
          reussir.rc.dec(%owned : !rc_i64)
          %s = arith.addi %v, %v : i64
          reussir.closure.yield %s : i64
      }
    }
    %c1 = reussir.closure.apply(%p : !rc_i64) to (%c : !reussir.rc<!fn2>) : !reussir.rc<!fn1>
    %c2 = reussir.closure.apply(%x : i64) to (%c1 : !reussir.rc<!fn1>) : !reussir.rc<!fn0>
    %r = scf.if %cond -> (i64) {
      %u = reussir.closure.uniqify(%c2 : !reussir.rc<!fn0>) : !reussir.rc<!fn0>
      %v = reussir.closure.eval(%u : !reussir.rc<!fn0>) : i64
      scf.yield %v : i64
    } else {
      reussir.rc.dec(%c2 : !reussir.rc<!fn0>)
      scf.yield %x : i64
    }
    return %r : i64
  }

  // Never evaluated: only the captures are released.
  func.func @drop_only(%p: !rc_i64, %x: i64) {
    %c = reussir.closure.create -> !reussir.rc<!fn2> {
      body {
        ^bb0(%owned: !rc_i64, %v: i64):
          reussir.rc.dec(%owned : !rc_i64)
          reussir.closure.yield %v : i64
      }
    }
    %c1 = reussir.closure.apply(%p : !rc_i64) to (%c : !reussir.rc<!fn2>) : !reussir.rc<!fn1>
    %c2 = reussir.closure.apply(%x : i64) to (%c1 : !reussir.rc<!fn1>) : !reussir.rc<!fn0>
    reussir.rc.dec(%c2 : !reussir.rc<!fn0>)
    return
  }

  // Evaluated in both arms: moving the body into one arm is not enough, and
  // copying it is not done, so the closure stays.
  func.func @eval_in_both(%cond: i1, %x: i64) -> i64 {
    %c = reussir.closure.create -> !reussir.rc<!fn1> {
      body {
        ^bb0(%v: i64):
          reussir.closure.yield %v : i64
      }
    }
    %c1 = reussir.closure.apply(%x : i64) to (%c : !reussir.rc<!fn1>) : !reussir.rc<!fn0>
    %r = scf.if %cond -> (i64) {
      %v = reussir.closure.eval(%c1 : !reussir.rc<!fn0>) : i64
      scf.yield %v : i64
    } else {
      %v = reussir.closure.eval(%c1 : !reussir.rc<!fn0>) : i64
      scf.yield %v : i64
    }
    return %r : i64
  }
}

// CHECK-LABEL: func.func @eval_or_drop
// CHECK-NOT: reussir.closure
// CHECK: scf.if
// CHECK: reussir.rc.dec(%arg1
// CHECK: arith.addi %arg2, %arg2
// CHECK: } else {
// CHECK-NEXT: reussir.rc.dec(%arg1
// CHECK-NOT: reussir.closure

// CHECK-LABEL: func.func @drop_only
// CHECK-NEXT: reussir.rc.dec(%arg0
// CHECK-NEXT: return

// CHECK-LABEL: func.func @eval_in_both
// CHECK: reussir.closure.create
// CHECK: scf.if
