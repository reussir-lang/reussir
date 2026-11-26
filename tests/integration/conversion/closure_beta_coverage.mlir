// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-closure-beta-reduction)' \
// RUN: | %FileCheck %s

// Beta reduction moves the increment of a closure body with the body. The
// increment stays where the body runs, so the counter counts the runs of
// the closure. A closure that never runs loses its body and its increment.
// Each closure body below has a different counter index.

!fn = !reussir.closure<(i64) -> i64>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  llvm.mlir.global private constant @__profn_cov("cov") {addr_space = 0 : i32} : !llvm.array<3 x i8>

  // The closure runs 0 times.
  func.func @never() {
    %c = reussir.closure.create -> !reussir.rc<!fn> {
      body {
        ^bb0(%x: i64):
          %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
          %hash = llvm.mlir.constant(7 : i64) : i64
          %num = llvm.mlir.constant(5 : i32) : i32
          %index = llvm.mlir.constant(1 : i32) : i32
          llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %index) : (!llvm.ptr, i64, i32, i32) -> ()
          reussir.closure.yield %x : i64
      }
    }
    reussir.rc.dec(%c : !reussir.rc<!fn>)
    return
  }

  // The closure runs 1 time.
  func.func @once(%v: i64) -> i64 {
    %c = reussir.closure.create -> !reussir.rc<!fn> {
      body {
        ^bb0(%x: i64):
          %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
          %hash = llvm.mlir.constant(7 : i64) : i64
          %num = llvm.mlir.constant(5 : i32) : i32
          %index = llvm.mlir.constant(2 : i32) : i32
          llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %index) : (!llvm.ptr, i64, i32, i32) -> ()
          %y = arith.muli %x, %x : i64
          reussir.closure.yield %y : i64
      }
    }
    %u = reussir.closure.uniqify(%c : !reussir.rc<!fn>) : !reussir.rc<!fn>
    %a = reussir.closure.apply(%v : i64) to (%u : !reussir.rc<!fn>) : !reussir.rc<!reussir.closure<() -> i64>>
    %r = reussir.closure.eval(%a : !reussir.rc<!reussir.closure<() -> i64>>) : i64
    return %r : i64
  }

  // The closure runs once in each iteration of the loop.
  func.func @many(%n: index) -> i64 {
    %c = reussir.closure.create -> !reussir.rc<!fn> {
      body {
        ^bb0(%x: i64):
          %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
          %hash = llvm.mlir.constant(7 : i64) : i64
          %num = llvm.mlir.constant(5 : i32) : i32
          %index = llvm.mlir.constant(3 : i32) : i32
          llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %index) : (!llvm.ptr, i64, i32, i32) -> ()
          %y = arith.addi %x, %x : i64
          reussir.closure.yield %y : i64
      }
    }
    %lb = arith.constant 0 : index
    %step = arith.constant 1 : index
    %zero = arith.constant 0 : i64
    %r = scf.for %i = %lb to %n step %step iter_args(%acc = %zero) -> (i64) {
      reussir.rc.inc(%c : !reussir.rc<!fn>)
      %iv = arith.index_cast %i : index to i64
      %y = reussir.closure.eval(%c : !reussir.rc<!fn>) with(%iv : i64) : i64
      %next = arith.addi %acc, %y : i64
      scf.yield %next : i64
    }
    reussir.rc.dec(%c : !reussir.rc<!fn>)
    return %r : i64
  }

  // A combinator runs the closure `n` times.
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

  func.func @combinator(%n: i64) -> i64 {
    %zero = arith.constant 0 : i64
    %c = reussir.closure.create -> !reussir.rc<!fn> {
      body {
        ^bb0(%x: i64):
          %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
          %hash = llvm.mlir.constant(7 : i64) : i64
          %num = llvm.mlir.constant(5 : i32) : i32
          %index = llvm.mlir.constant(4 : i32) : i32
          llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %index) : (!llvm.ptr, i64, i32, i32) -> ()
          %one = arith.constant 1 : i64
          %y = arith.addi %x, %one : i64
          reussir.closure.yield %y : i64
      }
    }
    %r = func.call @apply_n(%n, %zero, %c) : (i64, i64, !reussir.rc<!fn>) -> i64
    return %r : i64
  }
}

// CHECK-LABEL: func.func @never()
// CHECK-NOT: llvm.call_intrinsic
// CHECK: return

// CHECK-LABEL: func.func @once(
// CHECK-NOT: reussir.closure
// CHECK: %[[ONCE:.*]] = llvm.mlir.constant(2 : i32) : i32
// CHECK: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.*}}, %{{.*}}, %{{.*}}, %[[ONCE]])
// CHECK: arith.muli
// CHECK: return

// CHECK-LABEL: func.func @many(
// CHECK-NOT: reussir.closure
// CHECK: %[[MANY:.*]] = llvm.mlir.constant(3 : i32) : i32
// CHECK: scf.for
// CHECK: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.*}}, %{{.*}}, %{{.*}}, %[[MANY]])
// CHECK: arith.addi
// CHECK: scf.yield
// CHECK: return

// CHECK-LABEL: func.func @combinator(
// CHECK-NOT: reussir.closure
// CHECK: call @[[SPEC:apply_n\.closure[_0-9]*]](
// CHECK: func.func private @[[SPEC]](
// CHECK-NOT: reussir.closure
// CHECK: %[[COMBINATOR:.*]] = llvm.mlir.constant(4 : i32) : i32
// CHECK: scf.if
// CHECK: } else {
// CHECK: llvm.call_intrinsic "llvm.instrprof.increment"(%{{.*}}, %{{.*}}, %{{.*}}, %[[COMBINATOR]])
// CHECK: call @[[SPEC]](
