// RUN: %reussir-opt %s --reussir-instrument-coverage --reussir-emit-coverage-mapping \
// RUN:   --reussir-borrow-specialization | %FileCheck %s

// The borrowed clone comes after the instrumentation. It increments the
// counters of the original with the PGO name and the hash of the original,
// and it gets no record of its own. llvm-cov thus adds the counts of the
// clone to the original.

!list_ = !reussir.record<variant "List" incomplete>
!nil = !reussir.record<compound "List::Nil" [value] { }>
!cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !nil, !cons }>
!L = !reussir.rc<!list>

// CHECK: llvm.mlir.global private constant @__profn_list.rr_mem("list.rr:mem")
// CHECK-NOT: @__profn_
module {
  // CHECK-LABEL: func.func private @mem(
  // CHECK: llvm.mlir.addressof @__profn_list.rr_mem
  // CHECK-NEXT: llvm.mlir.constant([[HASH:-?[0-9]+]] : i64)
  // CHECK-NEXT: llvm.mlir.constant([[COUNTERS:[0-9]+]] : i32)
  func.func private @mem(%x: i64, %xs: !L) -> i1 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> i1 {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        reussir.rc.dec(%xs : !L)
        %f = arith.constant false loc("list.rr":3:14 to 3:19)
        reussir.scf.yield %f : i1
      }
      [1] -> {
      ^bb0(%cell: !reussir.ref<!cons shared>):
        %hr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [0] : !reussir.ref<i64 shared>
        %h = reussir.ref.load(%hr : !reussir.ref<i64 shared>) : i64
        %tr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [1] : !reussir.ref<!L shared>
        %t = reussir.ref.load(%tr : !reussir.ref<!L shared>) : !L
        reussir.rc.inc(%t : !L)
        reussir.rc.dec(%xs : !L)
        %eq = arith.cmpi eq, %x, %h : i64 loc("list.rr":5:12 to 5:18)
        %res = scf.if %eq -> (i1) {
          reussir.rc.dec(%t : !L)
          %tt = arith.constant true loc("list.rr":5:21 to 5:25)
          scf.yield %tt : i1
        } else {
          %rec = func.call @mem(%x, %t) : (i64, !L) -> i1 loc("list.rr":5:35 to 5:46)
          scf.yield %rec : i1
        }
        reussir.scf.yield %res : i1
      }
    }
    return %r : i1
  } loc("list.rr":1:1 to 7:2)

  // The caller keeps the list, so the call goes to a borrowed clone.
  // CHECK-LABEL: func.func @search(
  // CHECK: call @mem.borrowed_1(
  func.func @search(%xs: !L) -> i1 {
    %key = arith.constant 1 : i64
    reussir.rc.inc(%xs : !L)
    %found = func.call @mem(%key, %xs) : (i64, !L) -> i1
    reussir.rc.dec(%xs : !L)
    return %found : i1
  } loc(unknown)
}

// CHECK: llvm.mlir.global linkonce_odr hidden constant @__covrec_
// CHECK-NOT: llvm.mlir.global {{.*}}@__covrec_

// CHECK-LABEL: func.func private @mem.borrowed_1(
// CHECK: llvm.mlir.addressof @__profn_list.rr_mem
// CHECK-NEXT: llvm.mlir.constant([[HASH]] : i64)
// CHECK-NEXT: llvm.mlir.constant([[COUNTERS]] : i32)
// CHECK-NEXT: llvm.mlir.constant(0 : i32)
// CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
// CHECK: call @mem.borrowed_1(
