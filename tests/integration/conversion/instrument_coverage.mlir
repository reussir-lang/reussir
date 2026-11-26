// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s
// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-debuginfo \
// RUN:   | %reussir-opt --reussir-instrument-coverage --mlir-print-local-scope \
// RUN:   | %FileCheck %s

// CHECK-DAG: llvm.mlir.global private constant @__profn_choose("choose")
// CHECK-DAG: llvm.mlir.global private constant @__profn_count("count")
// CHECK-DAG: llvm.mlir.global private constant @__profn_a.rr_helper("a.rr:helper")

module attributes {reussir.coverage.include = []} {
  // CHECK-LABEL: func.func @choose(
  // CHECK-SAME: reussir.coverage.map = {counters = 3 : i32, hash = [[HASH:-?[0-9]+]] : i64, name = "choose",
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("a.rr":1:1 to 7:2)}, {counter = 1 : i32, span = loc("a.rr":3:5 to :10)}, {counter = 2 : i32, span = loc("a.rr":5:5 to :10)}]}
  // CHECK: %[[NAME:.+]] = llvm.mlir.addressof @__profn_choose : !llvm.ptr
  // CHECK-NEXT: %[[FUNCTION_HASH:.+]] = llvm.mlir.constant([[HASH]] : i64) : i64
  // CHECK-NEXT: %[[COUNTERS:.+]] = llvm.mlir.constant(3 : i32) : i32
  // CHECK-NEXT: %[[INDEX:.+]] = llvm.mlir.constant(0 : i32) : i32
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"(%[[NAME]], %[[FUNCTION_HASH]], %[[COUNTERS]], %[[INDEX]]) : (!llvm.ptr, i64, i32, i32) -> ()
  // CHECK: scf.if
  // CHECK: llvm.mlir.constant([[HASH]] : i64)
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.addi
  // CHECK: } else {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.muli
  func.func @choose(%c: i1, %x: i64) -> i64 {
    %r = scf.if %c -> (i64) {
      %a = arith.addi %x, %x : i64 loc("a.rr":3:5 to 3:10)
      scf.yield %a : i64 loc("a.rr":3:5 to 3:10)
    } else {
      %m = arith.muli %x, %x : i64 loc("a.rr":5:5 to 5:10)
      scf.yield %m : i64 loc("a.rr":5:5 to 5:10)
    } loc("a.rr":2:3 to 6:4)
    return %r : i64 loc("a.rr":2:3 to 6:4)
  } loc("a.rr":1:1 to 7:2)

  // CHECK-LABEL: func.func @count(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("a.rr":8:1 to 14:2)}, {counter = 1 : i32, span = loc("a.rr":10:9 to :14)}, {counter = 2 : i32, span = loc("a.rr":11:5 to :15)}]}
  // CHECK: llvm.mlir.constant(0 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK: scf.while
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.cmpi
  // CHECK: } do {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  func.func @count(%n: i64) -> i64 {
    %zero = arith.constant 0 : i64 loc("a.rr":9:13 to 9:14)
    %r = scf.while (%i = %zero) : (i64) -> i64 {
      %lt = arith.cmpi slt, %i, %n : i64 loc("a.rr":10:9 to 10:14)
      scf.condition(%lt) %i : i64 loc("a.rr":10:9 to 10:14)
    } do {
    ^bb0(%i: i64):
      %one = arith.constant 1 : i64 loc("a.rr":11:14 to 11:15)
      %next = arith.addi %i, %one : i64 loc("a.rr":11:5 to 11:15)
      scf.yield %next : i64 loc("a.rr":11:5 to 11:15)
    } loc("a.rr":10:3 to 12:4)
    return %r : i64 loc("a.rr":13:3 to 13:4)
  } loc("a.rr":8:1 to 14:2)

  // Local linkage puts the file name into the PGO name. A region with no
  // located op gets no counter.
  // CHECK-LABEL: func.func @helper(
  // CHECK-SAME: name = "a.rr:helper", regions = [{counter = 0 : i32, span = loc("a.rr":15:1 to 18:2)}]}
  // CHECK: llvm.mlir.addressof @__profn_a.rr_helper
  // CHECK: scf.if
  // CHECK-NOT: llvm.instrprof.increment
  // CHECK: return
  func.func @helper(%c: i1, %x: i64) attributes {llvm.linkage = #llvm.linkage<internal>} {
    scf.if %c {
      %a = arith.addi %x, %x : i64 loc(unknown)
      scf.yield loc(unknown)
    } loc("a.rr":16:3 to 16:20)
    return loc("a.rr":17:3 to 17:9)
  } loc("a.rr":15:1 to 18:2)

  // CHECK-LABEL: func.func @unlocated(
  // CHECK-NOT: reussir.coverage.map
  // CHECK-NOT: llvm.instrprof.increment
  // CHECK: return
  func.func @unlocated(%x: i64) -> i64 {
    return %x : i64 loc(unknown)
  } loc(unknown)

  // CHECK-LABEL: func.func @excluded(
  // CHECK-NOT: reussir.coverage.map
  // CHECK-NOT: llvm.instrprof.increment
  // CHECK: return
  func.func @excluded(%x: i64) -> i64 attributes {reussir.coverage = false} {
    return %x : i64 loc("a.rr":20:3 to 20:9)
  } loc("a.rr":19:1 to 21:2)

  // An op with the location of the op that holds its region is a part of
  // that op: the `else` that the frontend makes for an `if` with no `else`,
  // and the regions that the lowering makes for one call.
  // CHECK-LABEL: func.func @ping(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("r.rr":1:22 to 5:2)}, {counter = 1 : i32, span = loc("r.rr":3:9 to :15)}]}
  func.func @ping(%c: i1) {
    scf.if %c {
      func.call @tick() : () -> () loc("r.rr":3:9 to 3:15)
      scf.yield loc("r.rr":3:9 to 3:15)
    } else {
      scf.yield loc("r.rr":2:5 to 4:6)
    } loc("r.rr":2:5 to 4:6)
    return loc("r.rr":1:22 to 5:2)
  } loc("r.rr":1:22 to 5:2)

  // CHECK-LABEL: func.func @fold(
  // CHECK-SAME: counters = 1 : i32
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("s.rr":4:20 to 6:2)}]}
  func.func @fold(%p: i1, %n: index) {
    %flag = arith.xori %p, %p : i1 loc("s.rr":5:5 to 5:30)
    scf.if %flag {
      func.call @tick() : () -> () loc("s.rr":5:5 to 5:30)
      scf.yield loc("s.rr":5:5 to 5:30)
    } else {
      scf.yield loc("s.rr":5:5 to 5:30)
    } loc("s.rr":5:5 to 5:30)
    %c0 = arith.constant 0 : index loc("s.rr":5:5 to 5:30)
    %c1 = arith.constant 1 : index loc("s.rr":5:5 to 5:30)
    scf.for %i = %c0 to %n step %c1 {
      func.call @tick() : () -> () loc("s.rr":5:5 to 5:30)
      scf.yield loc("s.rr":5:5 to 5:30)
    } loc("s.rr":5:5 to 5:30)
    return loc("s.rr":4:20 to 6:2)
  } loc("s.rr":4:20 to 6:2)

  func.func private @tick()

  // CHECK: func.func private @external(i64) -> i64{{$}}
  func.func private @external(i64) -> i64 loc("a.rr":22:1 to 22:20)
}
