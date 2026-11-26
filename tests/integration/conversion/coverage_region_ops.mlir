// RUN: %reussir-opt %s --reussir-instrument-coverage | %FileCheck %s

// The body of region.run runs once each time the op runs, so it has no
// counter. The scf.if in the body has one counter for each branch.
// CHECK-LABEL: func.func @regional
// CHECK-SAME: counters = 3 : i32
// CHECK: reussir.region.run
// CHECK-NEXT: ^bb0(%{{.*}}: !reussir.region):
// CHECK-NEXT: scf.if
func.func @regional(%flag: i1, %x: i64) -> i64 {
  %r = reussir.region.run -> i64 {
  ^bb0(%region: !reussir.region):
    %v = scf.if %flag -> i64 {
      %one = arith.constant 1 : i64 loc("a.rr":3:20 to 3:21)
      scf.yield %one : i64 loc("a.rr":3:20 to 3:21)
    } else {
      scf.yield %x : i64 loc("a.rr":3:31 to 3:32)
    } loc("a.rr":3:9 to 3:34)
    reussir.region.yield %v : i64 loc("a.rr":3:9 to 3:34)
  } loc("a.rr":2:5 to 4:6)
  return %r : i64 loc("a.rr":2:5 to 4:6)
} loc("a.rr":1:1 to 5:2)

// scf.execute_region runs its region exactly once.
// CHECK-LABEL: func.func @once
// CHECK-SAME: counters = 1 : i32
func.func @once(%x: i64) -> i64 {
  %r = scf.execute_region -> i64 {
    scf.yield %x : i64 loc("a.rr":7:5 to 7:6)
  } loc("a.rr":7:5 to 7:6)
  return %r : i64 loc("a.rr":7:5 to 7:6)
} loc("a.rr":6:1 to 8:2)

// A StableHLO kernel runs on the device, so it gets no counters.
// CHECK-LABEL: func.func @kernel
// CHECK-NOT: reussir.coverage.map
// CHECK-NOT: llvm.instrprof.increment
// CHECK: return
func.func @kernel(%x: i64) -> i64 attributes {reussir.kernel_function} {
  return %x : i64 loc("a.rr":10:5 to 10:6)
} loc("a.rr":9:1 to 11:2)
