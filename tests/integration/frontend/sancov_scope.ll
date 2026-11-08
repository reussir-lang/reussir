; RUN: %rrc %s --emit llvm-ir --fuzzer --sancov-include demo -o %t.ll
; RUN: %FileCheck %s < %t.ll
;
; CHECK-LABEL: define{{.*}} @_RNvC4demo4body(
; CHECK: @__sancov_gen_
; CHECK-LABEL: define{{.*}} @generated_harness(
; CHECK-NOT: @__sancov_gen_
; CHECK-NOT: __sanitizer_cov_trace
; CHECK: call{{.*}} @_RNvC4demo4body({{.*}}) #[[CALL:[0-9]+]]
; CHECK: }
; CHECK: attributes #[[CALL]] = { noinline }

define i64 @_RNvC4demo4body(i64 %x) {
entry:
  %c = icmp sgt i64 %x, 1000
  br i1 %c, label %yes, label %no
yes:
  %a = mul i64 %x, 7
  ret i64 %a
no:
  %b = add i64 %x, 9
  ret i64 %b
}

define i64 @generated_harness(i64 %x) #0 {
entry:
  %c = icmp eq i64 %x, 42
  %arg = select i1 %c, i64 43, i64 %x
  %result = call i64 @_RNvC4demo4body(i64 %arg)
  ret i64 %result
}

attributes #0 = { "reussir-coverage-origin"="_RNvC4test7harness" }
