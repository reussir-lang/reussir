; RUN: %rrc %s --target-triple x86_64-unknown-linux-gnu --target-cpu x86-64 -O default --emit llvm-ir -o %t.default.ll
; RUN: %FileCheck %s < %t.default.ll
; RUN: %rrc %s --target-triple x86_64-unknown-linux-gnu --target-cpu x86-64 -O aggressive --emit llvm-ir -o %t.aggressive.ll
; RUN: %FileCheck %s < %t.aggressive.ll
; RUN: %rrc %s --target-triple x86_64-unknown-linux-gnu --target-cpu x86-64 -O none --emit llvm-ir -o %t.none.ll
; RUN: %FileCheck %s --check-prefix=NONE < %t.none.ll

; Straight-line stores require SLP, independently of loop vectorization.
; Pin the target so the expected vector width is independent of the host.
; CHECK-LABEL: define void @fill_four(
; CHECK-NOT: store i32
; CHECK: store <4 x i32>
; CHECK-NOT: store i32
; CHECK: ret void
; NONE-LABEL: define void @fill_four(
; NONE-NOT: store <
; NONE-COUNT-4: store i32
; NONE-NOT: store <
; NONE: ret void
define void @fill_four(ptr %dst, i32 %value) {
  store i32 %value, ptr %dst, align 4
  %p1 = getelementptr i32, ptr %dst, i64 1
  store i32 %value, ptr %p1, align 4
  %p2 = getelementptr i32, ptr %dst, i64 2
  store i32 %value, ptr %p2, align 4
  %p3 = getelementptr i32, ptr %dst, i64 3
  store i32 %value, ptr %p3, align 4
  ret void
}
