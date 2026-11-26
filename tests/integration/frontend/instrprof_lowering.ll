; RUN: %rrc %s --emit llvm-ir --instrument-coverage -o %t.ll
; RUN: %FileCheck %s < %t.ll
; RUN: %rrc %s --emit llvm-ir -o %t.plain.ll
; RUN: %FileCheck %s --check-prefix=PLAIN < %t.plain.ll
;
; CHECK-DAG: @__profc_count = {{.*}}global [1 x i64] zeroinitializer
; CHECK-DAG: @__profd_count = {{.*}}global
; CHECK-LABEL: define{{.*}} @count(
; CHECK-NOT: @llvm.instrprof.increment
; CHECK: load i64, ptr @__profc_count
; CHECK: store i64 {{.*}}, ptr @__profc_count
;
; PLAIN-NOT: @__profc_count
; PLAIN: call void @llvm.instrprof.increment(ptr {{.*}}@__profn_count, i64 1, i32 1, i32 0)

@__profn_count = private constant [5 x i8] c"count"

define i64 @count(i64 %x) {
entry:
  call void @llvm.instrprof.increment(ptr @__profn_count, i64 1, i32 1, i32 0)
  %y = add i64 %x, 1
  ret i64 %y
}
