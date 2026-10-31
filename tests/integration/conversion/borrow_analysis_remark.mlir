// RUN: %reussir-opt %s --reussir-borrow-analysis-remark --remarks-filter=Borrow --remark-format=emitRemark --verify-diagnostics

// Frontend-shaped code: every parameter is owned, a caller that keeps a
// value retains it before the call, and the callee releases it.

!list_ = !reussir.record<variant "List" incomplete>
!nil = !reussir.record<compound "List::Nil" [value] { }>
!cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !nil, !cons }>
!L = !reussir.rc<!list>
!box = !reussir.record<compound "Box" { !list, i64 }>
!B = !reussir.rc<!box>

module {
  // Only reads the list: the parameter is borrowable, recursion included.
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=mem | Parameter=1,}}
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
          // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=mem | Argument=1, Callee=mem,}}
          %rec = func.call @mem(%x, %t) : (i64, !L) -> i1
          scf.yield %rec : i1
        }
        reussir.scf.yield %res : i1
      }
    }
    return %r : i1
  }

  // reussir-lang/reussir#762: the list stays live across every query.
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=search | Parameter=1,}}
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
      // expected-remark @+1 {{LiveThrough | Category:Borrow:Analysis | Function=search | Argument=1, Callee=mem,}}
      %found = func.call @mem(%neg, %xs) : (i64, !L) -> i1
      %k1 = arith.subi %k, %c1 : i64
      %h1 = arith.addi %hits, %c1 : i64
      %h = arith.select %found, %h1, %hits : i64
      // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=search | Argument=1, Callee=search,}}
      %rec = func.call @search(%k1, %xs, %h) : (i64, !L, i64) -> i64
      scf.yield %rec : i64
    }
    return %r : i64
  }

  // Returning the parameter consumes it: not borrowable.
  func.func private @keep(%xs: !L) -> !L {
    return %xs : !L
  }

  // The other arm's use does not run after the call: last use.
  func.func private @branches(%c: i1, %xs: !L) -> i1 {
    %x = arith.constant 1 : i64
    %r = scf.if %c -> (i1) {
      // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=branches | Argument=1, Callee=mem,}}
      %a = func.call @mem(%x, %xs) : (i64, !L) -> i1
      scf.yield %a : i1
    } else {
      %k = func.call @keep(%xs) : (!L) -> !L
      reussir.rc.dec(%k : !L)
      %f = arith.constant false
      scf.yield %f : i1
    }
    return %r : i1
  }

  // A use after the enclosing `scf.if` runs after the call.
  func.func private @after(%c: i1, %xs: !L) -> i1 {
    %x = arith.constant 1 : i64
    %r = scf.if %c -> (i1) {
      reussir.rc.inc(%xs : !L)
      // expected-remark @+1 {{LiveThrough | Category:Borrow:Analysis | Function=after | Argument=1, Callee=mem,}}
      %a = func.call @mem(%x, %xs) : (i64, !L) -> i1
      scf.yield %a : i1
    } else {
      %f = arith.constant false
      scf.yield %f : i1
    }
    %k = func.call @keep(%xs) : (!L) -> !L
    reussir.rc.dec(%k : !L)
    return %r : i1
  }

  // A retain outside the call's block does not feed it, so there is no
  // retain the specialization could drop: the call stays owned.
  func.func private @far(%c: i1, %xs: !L) -> i1 {
    %x = arith.constant 1 : i64
    reussir.rc.inc(%xs : !L)
    %r = scf.if %c -> (i1) {
      // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=far | Argument=1, Callee=mem,}}
      %a = func.call @mem(%x, %xs) : (i64, !L) -> i1
      scf.yield %a : i1
    } else {
      reussir.rc.dec(%xs : !L)
      %f = arith.constant false
      scf.yield %f : i1
    }
    %k = func.call @keep(%xs) : (!L) -> !L
    reussir.rc.dec(%k : !L)
    return %r : i1
  }

  // Releases the box before it reads the list.
  // expected-remark @+2 {{BorrowableParameter | Category:Borrow:Analysis | Function=drop_then_read | Parameter=0,}}
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=drop_then_read | Parameter=1,}}
  func.func private @drop_then_read(%xs: !L, %b: !B) -> i1 {
    reussir.rc.dec(%b : !B)
    %x = arith.constant 1 : i64
    // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=drop_then_read | Argument=1, Callee=mem,}}
    %r = func.call @mem(%x, %xs) : (i64, !L) -> i1
    return %r : i1
  }

  // `f(b.list, b)`: the retain only takes ownership of the loaded list,
  // which is not used again, and the callee may free the box, and the list
  // with it, before it reads the list. Both arguments stay owned.
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=field_and_box | Parameter=0,}}
  func.func private @field_and_box(%b: !B) -> i1 {
    %r = reussir.rc.borrow(%b : !B) : !reussir.ref<!box shared>
    %f = reussir.ref.project(%r : !reussir.ref<!box shared>) [0] : !reussir.ref<!L shared>
    %xs = reussir.ref.load(%f : !reussir.ref<!L shared>) : !L
    reussir.rc.inc(%xs : !L)
    // expected-remark @+2 {{LastUse | Category:Borrow:Analysis | Function=field_and_box | Argument=0, Callee=drop_then_read,}}
    // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=field_and_box | Argument=1, Callee=drop_then_read,}}
    %a = func.call @drop_then_read(%xs, %b) : (!L, !B) -> i1
    return %a : i1
  }

  // Rebuilds the list from loans (the tail is retained and handed on): the
  // parameter itself is still never consumed, so it is borrowable. The
  // owned body's release of it is the reuse donor at a last-use call.
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=inc_all | Parameter=0,}}
  func.func private @inc_all(%xs: !L) -> !L {
    %ref = reussir.rc.borrow(%xs : !L) : !reussir.ref<!list shared>
    %r = reussir.record.dispatch(%ref : !reussir.ref<!list shared>) -> !L {
      [0] -> {
      ^bb0(%nil: !reussir.ref<!nil shared>):
        reussir.rc.dec(%xs : !L)
        %n = reussir.record.compound : !nil
        %v = reussir.record.variant [0] (%n : !nil) : !list
        %e = reussir.rc.create value(%v : !list) : !L
        reussir.scf.yield %e : !L
      }
      [1] -> {
      ^bb0(%cell: !reussir.ref<!cons shared>):
        %hr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [0] : !reussir.ref<i64 shared>
        %h = reussir.ref.load(%hr : !reussir.ref<i64 shared>) : i64
        %tr = reussir.ref.project(%cell : !reussir.ref<!cons shared>) [1] : !reussir.ref<!L shared>
        %t = reussir.ref.load(%tr : !reussir.ref<!L shared>) : !L
        reussir.rc.inc(%t : !L)
        reussir.rc.dec(%xs : !L)
        %one = arith.constant 1 : i64
        %h1 = arith.addi %h, %one : i64
        // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=inc_all | Argument=0, Callee=inc_all,}}
        %rest = func.call @inc_all(%t) : (!L) -> !L
        %c = reussir.record.compound (%h1, %rest : i64, !L) : !cons
        %v = reussir.record.variant [1] (%c : !cons) : !list
        %e = reussir.rc.create value(%v : !list) : !L
        reussir.scf.yield %e : !L
      }
    }
    return %r : !L
  }

  // The same callee from two contexts: only the call whose caller keeps the
  // list may borrow, so the unique loop keeps its in-place reuse.
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=unique_loop | Parameter=0,}}
  func.func private @unique_loop(%xs: !L) -> !L {
    // expected-remark @+1 {{LastUse | Category:Borrow:Analysis | Function=unique_loop | Argument=0, Callee=inc_all,}}
    %ys = func.call @inc_all(%xs) : (!L) -> !L
    return %ys : !L
  }
  // expected-remark @+1 {{BorrowableParameter | Category:Borrow:Analysis | Function=shared_loop | Parameter=0,}}
  func.func private @shared_loop(%xs: !L) -> i64 {
    reussir.rc.inc(%xs : !L)
    // expected-remark @+1 {{LiveThrough | Category:Borrow:Analysis | Function=shared_loop | Argument=0, Callee=inc_all,}}
    %ys = func.call @inc_all(%xs) : (!L) -> !L
    reussir.rc.dec(%ys : !L)
    reussir.rc.dec(%xs : !L)
    %z = arith.constant 0 : i64
    return %z : i64
  }
}
