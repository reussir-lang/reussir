// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop. --implicit-check-not=.worklist
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-closure-outlining --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=backward' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop. --implicit-check-not=.worklist

module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @empty() -> !reussir.rc<!reussir.closure<() -> i64>> {
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<() -> i64>> {
      body {
        %zero = arith.constant 0 : i64
        reussir.closure.yield %zero : i64
      }
    }
    return %closure : !reussir.rc<!reussir.closure<() -> i64>>
  }

  func.func @scalar() -> !reussir.rc<!reussir.closure<(i64) -> i64>> {
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i64) -> i64>> {
      body {
      ^bb0(%value: i64):
        reussir.closure.yield %value : i64
      }
    }
    return %closure : !reussir.rc<!reussir.closure<(i64) -> i64>>
  }
}

// CHECK-LABEL: func.func private @{{.*}}4drop
// CHECK: reussir.token.free
// CHECK: return
// CHECK-LABEL: func.func private @{{.*}}4drop
// CHECK: reussir.token.free
// CHECK: return
