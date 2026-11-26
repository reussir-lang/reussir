// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s

// A closure body runs each time that the program calls the closure, not when
// the program makes the closure. Thus the body gets its own counter, and its
// increment is at the start of the body. A branch op in the body gets its
// counters in the body too.

!fn = !reussir.closure<(i64) -> i64>

module {
  // CHECK-LABEL: func.func @make()
  // CHECK-SAME: reussir.coverage.map = {counters = 4 : i32, hash = [[HASH:-?[0-9]+]] : i64, name = "make",
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("a.rr":2:1 to 5:2)}, {counter = 1 : i32, span = loc("a.rr":3:5 to :39)}, {counter = 2 : i32, span = loc("a.rr":3:24 to :26)}, {counter = 3 : i32, span = loc("a.rr":3:36 to :37)}, {false_counter = 3 : i32, span = loc("a.rr":3:16 to :21), true_counter = 2 : i32}]}
  // CHECK: %[[ENTRY:.+]] = llvm.mlir.constant(0 : i32) : i32
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.+}}, %{{.+}}, %{{.+}}, %[[ENTRY]])
  // CHECK: reussir.closure.create
  // CHECK: ^bb0(%{{.+}}: i64):
  // CHECK-NEXT: llvm.mlir.addressof @__profn_make
  // CHECK-NEXT: llvm.mlir.constant([[HASH]] : i64)
  // CHECK-NEXT: llvm.mlir.constant(4 : i32)
  // CHECK-NEXT: %[[BODY:.+]] = llvm.mlir.constant(1 : i32) : i32
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.+}}, %{{.+}}, %{{.+}}, %[[BODY]])
  // CHECK: scf.if
  // CHECK: %[[THEN:.+]] = llvm.mlir.constant(2 : i32) : i32
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.+}}, %{{.+}}, %{{.+}}, %[[THEN]])
  // CHECK: } else {
  // CHECK: %[[ELSE:.+]] = llvm.mlir.constant(3 : i32) : i32
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.+}}, %{{.+}}, %{{.+}}, %[[ELSE]])
  // CHECK: reussir.closure.yield
  func.func @make() -> !reussir.rc<!fn> {
    %c = reussir.closure.create -> !reussir.rc<!fn> {
      body {
        ^bb0(%x: i64):
          %zero = arith.constant 0 : i64 loc("a.rr":3:20 to 3:21)
          %neg = arith.cmpi slt, %x, %zero : i64 loc("a.rr":3:16 to 3:21)
          %y = scf.if %neg -> (i64) {
            %z = arith.subi %zero, %x : i64 loc("a.rr":3:24 to 3:26)
            scf.yield %z : i64 loc("a.rr":3:24 to 3:26)
          } else {
            scf.yield %x : i64 loc("a.rr":3:36 to 3:37)
          } loc("a.rr":3:13 to 3:39)
          reussir.closure.yield %y : i64 loc("a.rr":3:5 to 3:39)
      }
    } loc("a.rr":3:5 to 3:39)
    return %c : !reussir.rc<!fn> loc("a.rr":4:5 to 4:6)
  } loc("a.rr":2:1 to 5:2)
}
