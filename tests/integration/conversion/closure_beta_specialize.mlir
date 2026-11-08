// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-closure-beta-reduction)' \
// RUN: | %FileCheck %s

// A closure literal passed to a function that only retains, evaluates,
// releases, or forwards it to its own recursive calls: the call goes to a
// copy of the callee that takes the literal's captures instead.

!rc_i64 = !reussir.rc<i64>
!fn = !reussir.closure<(i64) -> i64>
!fn_cap = !reussir.closure<(!rc_i64, i64, i64) -> i64>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func private @apply_n(%n: i64, %acc: i64, %f: !reussir.rc<!fn>) -> i64 {
    %zero = arith.constant 0 : i64
    %one = arith.constant 1 : i64
    %done = arith.cmpi sle, %n, %zero : i64
    %r = scf.if %done -> (i64) {
      reussir.rc.dec(%f : !reussir.rc<!fn>)
      scf.yield %acc : i64
    } else {
      reussir.rc.inc(%f : !reussir.rc<!fn>)
      %u = reussir.closure.uniqify(%f : !reussir.rc<!fn>) : !reussir.rc<!fn>
      %next = reussir.closure.eval(%u : !reussir.rc<!fn>) with(%acc : i64) : i64
      %m = arith.subi %n, %one : i64
      %rec = func.call @apply_n(%m, %next, %f) : (i64, i64, !reussir.rc<!fn>) -> i64
      scf.yield %rec : i64
    }
    return %r : i64
  }

  func.func @caller(%p: !rc_i64, %k: i64, %n: i64) -> i64 {
    %zero = arith.constant 0 : i64
    %c = reussir.closure.create -> !reussir.rc<!fn_cap> {
      body {
        ^bb0(%owned: !rc_i64, %step: i64, %acc: i64):
          reussir.rc.dec(%owned : !rc_i64)
          %s = arith.addi %acc, %step : i64
          reussir.closure.yield %s : i64
      }
    }
    %c1 = reussir.closure.apply(%p : !rc_i64) to (%c : !reussir.rc<!fn_cap>) : !reussir.rc<!reussir.closure<(i64, i64) -> i64>>
    %c2 = reussir.closure.apply(%k : i64) to (%c1 : !reussir.rc<!reussir.closure<(i64, i64) -> i64>>) : !reussir.rc<!fn>
    %r = func.call @apply_n(%n, %zero, %c2) : (i64, i64, !reussir.rc<!fn>) -> i64
    return %r : i64
  }
}

// CHECK-LABEL: func.func private @apply_n(
// CHECK-LABEL: func.func @caller(
// CHECK-NOT: reussir.closure
// CHECK: call @[[SPEC:apply_n\.closure[_0-9]*]](%arg2, %{{.*}}, %arg0, %arg1)
// CHECK: func.func private @[[SPEC]](%{{.*}}: i64, %{{.*}}: i64, %[[P:.*]]: !reussir.rc<i64>, %[[K:.*]]: i64)
// CHECK-NOT: reussir.closure
// CHECK: reussir.rc.dec(%[[P]]
// CHECK: reussir.rc.inc(%[[P]]
// CHECK: reussir.rc.dec(%[[P]]
// CHECK: arith.addi %{{.*}}, %[[K]]
// CHECK: call @[[SPEC]](%{{.*}}, %{{.*}}, %[[P]], %[[K]])
