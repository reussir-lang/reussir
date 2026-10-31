// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s

!list_ = !reussir.record<variant "List" incomplete>
!nil = !reussir.record<compound "List::Nil" [value] { }>
!cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !nil, !cons }>
!L = !reussir.rc<!list>

module {
  // The owned original stays as it is for its last-use callers.
  // CHECK-LABEL: func.func private @mem(
  // CHECK: reussir.rc.dec
  // CHECK: call @mem(
  func.func private @mem(%x: i64, %xs: !L) -> i1 {
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> i1 {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        reussir.rc.dec(%xs : !L)
        %f = arith.constant false
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

  // reussir-lang/reussir#762: the caller keeps the list, so its retain goes
  // and the call borrows; the recursive call is the list's last use.
  // CHECK-LABEL: func.func private @search(
  // CHECK: } else {
  // CHECK-NOT: reussir.rc.inc
  // CHECK: call @mem.borrowed_1(
  // CHECK: call @search(
  func.func private @search(%k: i64, %xs: !L, %hits: i64) -> i64 {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %done = arith.cmpi eq, %k, %c0 : i64
    %r = scf.if %done -> (i64) {
      reussir.rc.dec(%xs : !L)
      scf.yield %hits : i64
    } else {
      reussir.rc.inc(%xs : !L)
      %neg = arith.subi %c0, %k : i64
      %found = func.call @mem(%neg, %xs) : (i64, !L) -> i1
      %k1 = arith.subi %k, %c1 : i64
      %h1 = arith.addi %hits, %c1 : i64
      %h = arith.select %found, %h1, %hits : i64
      %rec = func.call @search(%k1, %xs, %h) : (i64, !L, i64) -> i64
      scf.yield %rec : i64
    }
    return %r : i64
  }

  // A retain separated from its call by a release of something else might
  // be what keeps the list alive until the call: the call stays owned.
  // CHECK-LABEL: func.func private @unclear(
  // CHECK: reussir.rc.inc(%arg1
  // CHECK: call @mem(
  func.func private @unclear(%xs: !L, %ys: !L) -> i1 {
    %x = arith.constant 1 : i64
    reussir.rc.inc(%ys : !L)
    reussir.rc.dec(%xs : !L)
    %a = func.call @mem(%x, %ys) : (i64, !L) -> i1
    reussir.rc.dec(%ys : !L)
    return %a : i1
  }

  // The clone never releases its borrowed list. The loaded tail keeps its
  // counts for now, so the recursive call passes it owned.
  // CHECK-LABEL: func.func private @mem.borrowed_1(
  // CHECK-SAME: %{{.+}}: !reussir.rc<{{.+}}> {reussir.borrowed = #reussir.borrowed})
  // CHECK-SAME: attributes {llvm.linkage = #llvm.linkage<internal>}
  // CHECK-NOT: reussir.rc.dec(%arg1
  // CHECK: reussir.rc.inc(%[[TAIL:.+]] :
  // CHECK-NOT: reussir.rc.dec(%arg1
  // CHECK: call @mem(%arg0, %[[TAIL]])
  // CHECK: return
}
