; RUN: %rrc %s --emit asm -O aggressive --target-triple x86_64-unknown-linux-gnu --target-cpu znver5 -o %t.s
; RUN: %FileCheck %s < %t.s

; A division feeding the cold select operand should execute only on that
; path. The native backend must run SelectOptimize before instruction selection.
; CHECK-LABEL: choose:
; CHECK: test
; CHECK: j{{e|ne}}
; CHECK: div
; CHECK: ret

define i64 @choose(i64 %value, i64 %divisor, i1 %use_value) {
  %quotient = udiv i64 %value, %divisor
  %result = select i1 %use_value, i64 %value, i64 %quotient, !prof !0
  ret i64 %result
}

!0 = !{!"branch_weights", i32 2000, i32 1}
