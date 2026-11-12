// RUN: %reussir-opt %s \
// RUN:   --reussir-rc-create-fusion \
// RUN:   --reussir-trmc-recursion-analysis | %FileCheck %s

!list_ = !reussir.record<variant "List" incomplete>
!list_cons = !reussir.record<compound "List::Cons" [value] {i64, !list_}>
!list_nil = !reussir.record<compound "List::Nil" [value] {}>
!list = !reussir.record<variant "List" {!list_cons, !list_nil}>
!rc_list = !reussir.rc<!list>
!box = !reussir.rc<i64>
!cell = !reussir.rc<!reussir.cell<i64>>

module {
  // Linearizing the self call would run the store before the recursion.
  // CHECK-LABEL: func.func @store_after(
  // CHECK-NOT: .trmc
  // CHECK: call @store_after(
  // CHECK-NEXT: memref.store
  func.func @store_after(%n: i64, %slot: memref<i64>) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      %rest = func.call @store_after(%next, %slot) : (i64, memref<i64>) -> !rc_list
      memref.store %n, %slot[] : memref<i64>
      %cons = reussir.record.compound(%n, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }

  // A cell may be written by the recursion, so a read of it stays after.
  // CHECK-LABEL: func.func @cell_read_after(
  // CHECK-NOT: .trmc
  // CHECK: call @cell_read_after(
  // CHECK: reussir.ref.load
  func.func @cell_read_after(%n: i64, %cell: !cell) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      %rest = func.call @cell_read_after(%next, %cell) : (i64, !cell) -> !rc_list
      %ref = reussir.rc.borrow(%cell : !cell) : !reussir.ref<!reussir.cell<i64>>
      %slot = reussir.ref.project(%ref : !reussir.ref<!reussir.cell<i64>>) [0] : !reussir.ref<i64>
      %seen = reussir.ref.load(%slot : !reussir.ref<i64>) : i64
      %cons = reussir.record.compound(%seen, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }

  // A read of a shared box does not block the rewrite.
  // CHECK-LABEL: func.func @box_read_after(
  // CHECK: call @box_read_after.trmc(
  func.func @box_read_after(%n: i64, %box: !box) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      %rest = func.call @box_read_after(%next, %box) : (i64, !box) -> !rc_list
      %ref = reussir.rc.borrow(%box : !box) : !reussir.ref<i64>
      %seen = reussir.ref.load(%ref : !reussir.ref<i64>) : i64
      %cons = reussir.record.compound(%seen, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }

  // An ordinary release after the self call does not block the rewrite.
  // CHECK-LABEL: func.func @release_after(
  // CHECK: call @release_after.trmc(
  func.func @release_after(%n: i64, %box: !box) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      reussir.rc.dec(%box : !box)
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      reussir.rc.inc(%box : !box)
      %rest = func.call @release_after(%next, %box) : (i64, !box) -> !rc_list
      reussir.rc.dec(%box : !box)
      %cons = reussir.record.compound(%n, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }

  // A division may trap, and the recursion may not return, so it stays after.
  // CHECK-LABEL: func.func @div_after(
  // CHECK-NOT: .trmc
  // CHECK: call @div_after(
  // CHECK-NEXT: arith.divsi
  func.func @div_after(%n: i64, %d: i64) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      %rest = func.call @div_after(%next, %d) : (i64, i64) -> !rc_list
      %quotient = arith.divsi %n, %d : i64
      %cons = reussir.record.compound(%quotient, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }

  // A manual release after the self call keeps its retain held across it.
  // CHECK-LABEL: func.func @manual_release_after(
  // CHECK-NOT: .trmc
  // CHECK: call @manual_release_after(
  // CHECK-NEXT: reussir.rc.dec{{.*}}{unsafe}
  func.func @manual_release_after(%n: i64, %box: !box) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      reussir.rc.dec(%box : !box)
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      reussir.rc.inc(%box : !box) {unsafe}
      reussir.rc.inc(%box : !box)
      %rest = func.call @manual_release_after(%next, %box) : (i64, !box) -> !rc_list
      reussir.rc.dec(%box : !box) {unsafe}
      reussir.rc.dec(%box : !box)
      %cons = reussir.record.compound(%n, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }
}
