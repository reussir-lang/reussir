// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s

!nil = !reussir.record<compound "Choice.Nil" [value] {}>
!left = !reussir.record<compound "Choice.Left" [value] {i64}>
!empty = !reussir.record<compound "Choice.Empty" [value] {}>
!right = !reussir.record<compound "Choice.Right" [value] {i64}>
!choice = !reussir.record<variant "Choice" {!nil, !left, !empty, !right}>
!rc_choice = !reussir.rc<!choice>

module attributes {reussir.special_ptr_tag = "tbi"} {
  // Multiple boxed arms retain a single dispatch covering every tag.
  // CHECK-LABEL: func.func @choose(
  // CHECK-NOT: reussir.rc.compare_immortal
  // CHECK-NOT: scf.if
  // CHECK: %[[TAG:.+]] = reussir.record.tag
  // CHECK-NEXT: %{{.+}} = scf.index_switch %[[TAG]] -> i64
  // CHECK: case 0
  // CHECK: case 1
  // CHECK: case 2
  // CHECK: case 3
  // CHECK-NOT: reussir.rc.compare_immortal
  // CHECK-NOT: scf.if
  // CHECK: return
  func.func @choose(%value: !rc_choice) -> i64 {
    %ref = reussir.rc.borrow (%value : !rc_choice) : !reussir.ref<!choice>
    %result = reussir.record.dispatch (%ref : !reussir.ref<!choice>) -> i64 {
      [0] -> {
        ^bb0(%nil: !reussir.ref<!nil>):
        %zero = arith.constant 0 : i64
        reussir.scf.yield %zero : i64
      }
      [1] -> {
        ^bb1(%left: !reussir.ref<!left>):
        %slot = reussir.ref.project (%left : !reussir.ref<!left>) [0] : !reussir.ref<i64>
        %value = reussir.ref.load (%slot : !reussir.ref<i64>) : i64
        reussir.scf.yield %value : i64
      }
      [2] -> {
        ^bb2(%empty: !reussir.ref<!empty>):
        %two = arith.constant 2 : i64
        reussir.scf.yield %two : i64
      }
      [3] -> {
        ^bb3(%right: !reussir.ref<!right>):
        %slot = reussir.ref.project (%right : !reussir.ref<!right>) [0] : !reussir.ref<i64>
        %value = reussir.ref.load (%slot : !reussir.ref<i64>) : i64
        reussir.scf.yield %value : i64
      }
    }
    return %result : i64
  }
}
