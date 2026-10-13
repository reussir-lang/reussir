; RUN: %reussir-llvm-opt --passes='function(loop-simplify,reussir-linear-recurrence-matexp)' %s -o %t.ll
; RUN: %FileCheck %s < %t.ll
; RUN: %lli %t.ll
;
; The live-out uses a and c; closing their updates adds b, then d. Within
; each expression, discover PHIs in header order rather than pointer order.
; Distinct seeds make both the initial state and closure order observable.
;
; CHECK-LABEL: define i64 @state_order(
; CHECK: reussir.matexp.done:
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_a
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_c
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_b
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_d

define i64 @state_order(i64 %seed_a, i64 %seed_b, i64 %seed_c, i64 %seed_d, i32 %n) {
entry:
  br label %loop
loop:
  %i = phi i32 [ 1, %entry ], [ %inext, %loop ]
  %a = phi i64 [ %seed_a, %entry ], [ %b, %loop ]
  %b = phi i64 [ %seed_b, %entry ], [ %bnext, %loop ]
  %c = phi i64 [ %seed_c, %entry ], [ %d, %loop ]
  %d = phi i64 [ %seed_d, %entry ], [ %dnext, %loop ]
  %bnext = add i64 %c, %d
  %twice_a = shl i64 %a, 1
  %dnext = add i64 %twice_a, %b
  %out = add i64 %a, %c
  %inext = add i32 %i, 1
  %done = icmp eq i32 %i, %n
  br i1 %done, label %exit, label %loop
exit:
  ret i64 %out
}

; Reversing the header PHIs must reverse the first expression's discovery
; order even though its operands remain a + c. Closure then adds d and b.
; CHECK-LABEL: define i64 @reverse_order(
; CHECK: reussir.matexp.done:
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_c
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_a
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_d
; CHECK-NEXT: mul i64 %reussir.matexp.acc, %seed_b

define i64 @reverse_order(i64 %seed_a, i64 %seed_b, i64 %seed_c, i64 %seed_d, i32 %n) {
entry:
  br label %loop
loop:
  %i = phi i32 [ 1, %entry ], [ %inext, %loop ]
  %d = phi i64 [ %seed_d, %entry ], [ %dnext, %loop ]
  %c = phi i64 [ %seed_c, %entry ], [ %d, %loop ]
  %b = phi i64 [ %seed_b, %entry ], [ %bnext, %loop ]
  %a = phi i64 [ %seed_a, %entry ], [ %b, %loop ]
  %bnext = add i64 %c, %d
  %twice_a = shl i64 %a, 1
  %dnext = add i64 %twice_a, %b
  %out = add i64 %a, %c
  %inext = add i32 %i, 1
  %done = icmp eq i32 %i, %n
  br i1 %done, label %exit, label %loop
exit:
  ret i64 %out
}

define i32 @main() {
  %r1 = call i64 @state_order(i64 1, i64 2, i64 3, i64 4, i32 1)
  %r2 = call i64 @state_order(i64 1, i64 2, i64 3, i64 4, i32 2)
  %r3 = call i64 @state_order(i64 1, i64 2, i64 3, i64 4, i32 3)
  %r4 = call i64 @state_order(i64 1, i64 2, i64 3, i64 4, i32 4)
  %rev = call i64 @reverse_order(i64 1, i64 2, i64 3, i64 4, i32 4)
  %badrev = icmp ne i64 %rev, 19
  %bad1 = icmp ne i64 %r1, 4
  %bad2 = icmp ne i64 %r2, 6
  %bad3 = icmp ne i64 %r3, 11
  %bad4 = icmp ne i64 %r4, 19
  %b12 = or i1 %bad1, %bad2
  %b34 = or i1 %bad3, %bad4
  %forwardbad = or i1 %b12, %b34
  %bad = or i1 %forwardbad, %badrev
  %exit = zext i1 %bad to i32
  ret i32 %exit
}
