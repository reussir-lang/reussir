// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s

module {
  // CHECK-LABEL: func.func @driver(
  // CHECK: attributes {reussir.carrying_uniqueness}
  func.func @driver(%rc: !reussir.rc<i64>) -> !reussir.rc<i64> {
    %c2 = arith.constant 2 : i64
    %0 = func.call @loop(%rc, %c2) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
    return %0 : !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func @loop(
  // CHECK: attributes {reussir.carrying_uniqueness}
  // CHECK: %[[COND:.+]] = arith.cmpi eq, %arg1, %{{.+}} : i64
  // CHECK: %[[IF:.+]] = scf.if %[[COND]] -> (!reussir.rc<i64>) {
  // CHECK: } else {
  // CHECK: %[[NEXT:.+]] = reussir.rc.create value(%arg1 : i64) : !reussir.rc<i64>
  // CHECK: %[[RECURSE:.+]] = func.call @loop.unique(%[[NEXT]], %{{.+}}) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
  // CHECK: scf.yield %[[RECURSE]] : !reussir.rc<i64>
  // CHECK: } {reussir.carrying_uniqueness}
  func.func @loop(%rc: !reussir.rc<i64>, %n: i64) -> !reussir.rc<i64> {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %result = scf.if %cond -> (!reussir.rc<i64>) {
      scf.yield %rc : !reussir.rc<i64>
    } else {
      %next = reussir.rc.create value(%n : i64) : !reussir.rc<i64>
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @loop(%next, %n1) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
      scf.yield %rec : !reussir.rc<i64>
    }
    return %result : !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func private @loop.unique(
  // CHECK: attributes {llvm.linkage = #llvm.linkage<internal>, reussir.carrying_uniqueness}
  // CHECK: reussir.rc.assume_unique(%arg0 : !reussir.rc<i64>)
  // CHECK: %[[COND2:.+]] = arith.cmpi eq, %arg1, %{{.+}} : i64
  // CHECK: %[[IF2:.+]] = scf.if %[[COND2]] -> (!reussir.rc<i64>) {
  // CHECK: } else {
  // CHECK: %[[NEXT2:.+]] = reussir.rc.create value(%arg1 : i64) : !reussir.rc<i64>
  // CHECK: %[[RECURSE2:.+]] = func.call @loop.unique(%[[NEXT2]], %{{.+}}) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
  // CHECK: scf.yield %[[RECURSE2]] : !reussir.rc<i64>
  // CHECK: } {reussir.carrying_uniqueness}
}
