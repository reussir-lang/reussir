// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s --check-prefix=BORROW
// RUN: %reussir-opt %s --reussir-partial-move | %FileCheck %s --check-prefix=FUSION
// RUN: %reussir-opt %s --reussir-rc-create-fusion --reussir-trmc-recursion-analysis | %FileCheck %s --check-prefix=TRMC

// Source coverage puts an `llvm.instrprof.increment` at the entry of each
// function and region. The increments do not stop these optimizations.

!list_ = !reussir.record<variant "List" incomplete>
!nil = !reussir.record<compound "List::Nil" [value] { }>
!cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !nil, !cons }>
!L = !reussir.rc<!list>
!state = !reussir.record<compound "State" [value] {i64, !list}>
!queue = !reussir.record<compound "Queue" {i64, !list, !state}>
!Q = !reussir.rc<!queue>

module {
  llvm.mlir.global private constant @__profn_cov("cov") {addr_space = 0 : i32} : !llvm.array<3 x i8>

  // A read-only traversal keeps the entry for a last use.
  // BORROW-LABEL: func.func @last_search(
  // BORROW: call @mem.borrowed_1.entry_1(
  func.func private @mem(%x: i64, %xs: !L) -> i1 {
    %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
    %hash = llvm.mlir.constant(1 : i64) : i64
    %num = llvm.mlir.constant(3 : i32) : i32
    %c0 = llvm.mlir.constant(0 : i32) : i32
    %c1 = llvm.mlir.constant(1 : i32) : i32
    %c2 = llvm.mlir.constant(2 : i32) : i32
    llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c0) : (!llvm.ptr, i64, i32, i32) -> ()
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> i1 {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c1) : (!llvm.ptr, i64, i32, i32) -> ()
        reussir.rc.dec(%xs : !L)
        %f = arith.constant false
        reussir.scf.yield %f : i1
      }
      [1] -> {
      ^bb0(%cell: !reussir.ref<!cons shared>):
        llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c2) : (!llvm.ptr, i64, i32, i32) -> ()
        %hr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [0] : !reussir.ref<i64 shared>
        %h = reussir.ref.load(%hr : !reussir.ref<i64 shared>) : i64
        %tr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [1] : !reussir.ref<!L shared>
        %t = reussir.ref.load(%tr : !reussir.ref<!L shared>) : !L
        reussir.rc.inc(%t : !L)
        reussir.rc.dec(%xs : !L)
        %eq = arith.cmpi eq, %x, %h : i64
        %res = scf.if %eq -> (i1) {
          reussir.rc.dec(%t : !L)
          %tt = arith.constant true
          scf.yield %tt : i1
        } else {
          %rec = func.call @mem(%x, %t) : (i64, !L) -> i1
          scf.yield %rec : i1
        }
        reussir.scf.yield %res : i1
      }
    }
    return %r : i1
  }

  func.func @last_search(%xs: !L) -> i1 {
    %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
    %hash = llvm.mlir.constant(1 : i64) : i64
    %num = llvm.mlir.constant(1 : i32) : i32
    %c0 = llvm.mlir.constant(0 : i32) : i32
    llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c0) : (!llvm.ptr, i64, i32, i32) -> ()
    %key = arith.constant -1 : i64
    %r = func.call @mem(%key, %xs) : (i64, !L) -> i1
    return %r : i1
  }

  // The member retains fuse into the release across an increment, as an
  // inlined callee leaves it.
  // FUSION-LABEL: func.func @rebuild(
  // FUSION-NOT: reussir.rc.inc
  // FUSION: reussir.rc.dec(%{{.+}} : !reussir.rc<!reussir.record<compound "Queue"
  // FUSION-SAME: boundMembers = array<i64: 2, 1>
  func.func @rebuild(%q: !Q, %len: i64) -> !queue {
    %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
    %hash = llvm.mlir.constant(1 : i64) : i64
    %num = llvm.mlir.constant(1 : i32) : i32
    %c0 = llvm.mlir.constant(0 : i32) : i32
    %ref = reussir.rc.borrow (%q : !Q) : !reussir.ref<!queue>
    %front_slot = reussir.ref.project (%ref : !reussir.ref<!queue>) [1] : !reussir.ref<!L>
    %front = reussir.ref.load (%front_slot : !reussir.ref<!L>) : !L
    reussir.rc.inc (%front : !L)
    llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c0) : (!llvm.ptr, i64, i32, i32) -> ()
    %state_slot = reussir.ref.project (%ref : !reussir.ref<!queue>) [2] : !reussir.ref<!state>
    %state = reussir.ref.load (%state_slot : !reussir.ref<!state>) : !state
    %spilled = reussir.ref.spilled (%state : !state) : !reussir.ref<!state>
    reussir.ref.acquire (%spilled : !reussir.ref<!state>)
    reussir.rc.dec (%q : !Q)
    %rebuilt = reussir.record.compound(%len, %front, %state : i64, !L, !state) : !queue
    return %rebuilt : !queue
  }

  // An increment between the self call and the constructor can run before
  // the call.
  // TRMC-LABEL: func.func @count_down(
  // TRMC: call @count_down.trmc(
  func.func @count_down(%n: i64) -> !L {
    %name = llvm.mlir.addressof @__profn_cov : !llvm.ptr
    %hash = llvm.mlir.constant(1 : i64) : i64
    %num = llvm.mlir.constant(1 : i32) : i32
    %c0 = llvm.mlir.constant(0 : i32) : i32
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!L) {
      %nil = reussir.record.compound : !nil
      %variant = reussir.record.variant[0] (%nil : !nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !L
      scf.yield %list : !L
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      %rest = func.call @count_down(%next) : (i64) -> !L
      llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c0) : (!llvm.ptr, i64, i32, i32) -> ()
      %cons = reussir.record.compound(%n, %rest : i64, !L) : !cons
      %variant = reussir.record.variant[1] (%cons : !cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !L
      scf.yield %list : !L
    }
    return %result : !L
  }
}
