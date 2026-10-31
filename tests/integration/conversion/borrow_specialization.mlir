// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s
// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s --check-prefix=TWICE

!list_ = !reussir.record<variant "List" incomplete>
!nil = !reussir.record<compound "List::Nil" [value] { }>
!cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !nil, !cons }>
!L = !reussir.rc<!list>
!box = !reussir.record<compound "Box" { !list, i64 }>
!B = !reussir.rc<!box>

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

  // A live-through call from the function to itself: the loaded tail is
  // retained twice, once for the borrowed call and once for the external
  // consumer. The clone must keep exactly the consumer's retain.
  // TWICE-LABEL: func.func private @twice(
  // TWICE: call @twice.borrowed_0(
  func.func private @sink(!L) -> i64
  func.func private @twice(%xs: !L) -> i64 {
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> i64 {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        reussir.rc.dec(%xs : !L)
        %z = arith.constant 0 : i64
        reussir.scf.yield %z : i64
      }
      [1] -> {
      ^bb0(%cell: !reussir.ref<!cons shared>):
        %tr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [1] : !reussir.ref<!L shared>
        %t = reussir.ref.load(%tr : !reussir.ref<!L shared>) : !L
        reussir.rc.inc(%t : !L)
        reussir.rc.dec(%xs : !L)
        reussir.rc.inc(%t : !L)
        %a = func.call @twice(%t) : (!L) -> i64
        %b = func.call @sink(%t) : (!L) -> i64
        %s = arith.addi %a, %b : i64
        reussir.scf.yield %s : i64
      }
    }
    return %r : i64
  }
  // TWICE-LABEL: func.func private @twice.borrowed_0(
  // TWICE: reussir.ref.load
  // TWICE-NEXT: reussir.rc.inc
  // TWICE-NOT: reussir.rc.inc
  // TWICE: call @twice.borrowed_0(
  // TWICE-NEXT: call @sink(

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

  // `f(b.list, b)` with a list loaded from an owned box: its retain only
  // takes ownership of it, and the callee frees the box, and the list with
  // it, before reading the list. The call stays owned.
  // CHECK-LABEL: func.func private @field_and_box(
  // CHECK: reussir.rc.inc
  // CHECK-NEXT: call @drop_then_read(
  func.func private @drop_then_read(%xs: !L, %b: !B) -> i1 {
    reussir.rc.dec(%b : !B)
    %x = arith.constant 1 : i64
    %r = func.call @mem(%x, %xs) : (i64, !L) -> i1
    return %r : i1
  }
  func.func private @field_and_box(%b: !B) -> i1 {
    %r = reussir.rc.borrow(%b : !B) : !reussir.ref<!box shared>
    %f = reussir.ref.project(%r : !reussir.ref<!box shared>) [0] : !reussir.ref<!L shared>
    %xs = reussir.ref.load(%f : !reussir.ref<!L shared>) : !L
    reussir.rc.inc(%xs : !L)
    %a = func.call @drop_then_read(%xs, %b) : (!L, !B) -> i1
    return %a : i1
  }

  // Returns a loan: the loan keeps its retain (the caller gets a reference),
  // only the parameter's release goes.
  func.func private @tail(%xs: !L) -> !L {
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> !L {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        reussir.rc.inc(%xs : !L)
        reussir.rc.dec(%xs : !L)
        %n = reussir.record.compound : !nil
        %v = reussir.record.variant [0] (%n : !nil) : !list
        %e = reussir.rc.create value(%v : !list) : !L
        reussir.scf.yield %e : !L
      }
      [1] -> {
      ^bb0(%cell: !reussir.ref<!cons shared>):
        %tr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [1] : !reussir.ref<!L shared>
        %t = reussir.ref.load(%tr : !reussir.ref<!L shared>) : !L
        reussir.rc.inc(%t : !L)
        reussir.rc.dec(%xs : !L)
        reussir.scf.yield %t : !L
      }
    }
    return %r : !L
  }
  func.func private @peek(%xs: !L) -> !L {
    reussir.rc.inc(%xs : !L)
    %t = func.call @tail(%xs) : (!L) -> !L
    reussir.rc.dec(%xs : !L)
    return %t : !L
  }

  // Mutual recursion: each clone calls the other's clone.
  func.func private @even(%x: !L, %n: i64) -> i1 {
    %c0 = arith.constant 0 : i64
    %z = arith.cmpi eq, %n, %c0 : i64
    %r = scf.if %z -> (i1) {
      reussir.rc.dec(%x : !L)
      %t = arith.constant true
      scf.yield %t : i1
    } else {
      %c1 = arith.constant 1 : i64
      %m = arith.subi %n, %c1 : i64
      %o = func.call @odd(%x, %m) : (!L, i64) -> i1
      scf.yield %o : i1
    }
    return %r : i1
  }
  func.func private @odd(%x: !L, %n: i64) -> i1 {
    %e = func.call @even(%x, %n) : (!L, i64) -> i1
    return %e : i1
  }
  // CHECK-LABEL: func.func @parity(
  // CHECK-NOT: reussir.rc.inc
  // CHECK: call @even.borrowed_0(
  func.func @parity(%x: !L, %n: i64) -> i1 {
    reussir.rc.inc(%x : !L)
    %e = func.call @even(%x, %n) : (!L, i64) -> i1
    reussir.rc.dec(%x : !L)
    return %e : i1
  }

  // The clone never releases its borrowed list, and the loaded tail needs
  // no reference either: the recursive call borrows it too.
  // CHECK-LABEL: func.func private @mem.borrowed_1(
  // CHECK-SAME: %{{.+}}: !reussir.rc<{{.+}}> {reussir.borrowed = #reussir.borrowed})
  // CHECK-SAME: attributes {llvm.linkage = #llvm.linkage<internal>}
  // CHECK-NOT: reussir.rc.inc
  // CHECK-NOT: reussir.rc.dec
  // CHECK: call @mem.borrowed_1(
  // CHECK: return

  // CHECK-LABEL: func.func private @tail.borrowed_0(
  // CHECK-NOT: reussir.rc.dec
  // CHECK: reussir.ref.load
  // CHECK-NEXT: reussir.rc.inc
  // CHECK-NOT: reussir.rc.dec
  // CHECK: return

  // CHECK-LABEL: func.func private @even.borrowed_0(
  // CHECK-NOT: reussir.rc.dec
  // CHECK: call @odd.borrowed_0(
  // CHECK-LABEL: func.func private @odd.borrowed_0(
  // CHECK: call @even.borrowed_0(
}
