; RUN: %reussir-llvm-opt %s -passes='reussir-retcon-drop,verify' -o - | %FileCheck %s
; RUN: %reussir-llvm-opt %s -passes='reussir-retcon-drop,coro-early,cgscc(coro-split),coro-cleanup,reussir-retcon-drop-abi,verify' -o - | %FileCheck %s --check-prefix=SPLIT

target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare void @observe(ptr)
declare void @use_payload(ptr)

define internal void @locked(ptr %p) {
  call void @drop(ptr %p)
  ret void
}

; CHECK-LABEL: define linkonce_odr void @drop(
; CHECK: call i64 @__reussir_dtor_executor_size()
; CHECK: call i64 @__reussir_dtor_executor_align()
; CHECK: call ptr @__reussir_create_dtor_executor
; CHECK: call void @__reussir_run_dtor
; CHECK: call void @__reussir_destroy_dtor_executor
; CHECK: ret void

define linkonce_odr void @drop(ptr %p) "reussir.retcon_drop" {
  %payload = alloca [128 x i8], align 128
  call void @use_payload(ptr %payload)
  call void @locked(ptr %p)
  %child = load ptr, ptr %p
  %has = icmp ne ptr %child, null
  br i1 %has, label %recurse, label %done
recurse:
  call void @drop(ptr %child)
  call void @drop(ptr %child)
  br label %done
done:
  call void @use_payload(ptr %payload)
  call void @observe(ptr %p)
  ret void
}

; CHECK-LABEL: define private {{.*}} @drop.start(
; CHECK: call token @llvm.coro.id.retcon
; CHECK: call ptr @llvm.coro.begin
; CHECK: call void @locked
; CHECK-NOT: call void @drop(
; CHECK: call i1 (...) @llvm.coro.suspend.retcon.i1
; CHECK: call i1 (...) @llvm.coro.suspend.retcon.i1
; CHECK: call void @observe
; CHECK: call void @llvm.coro.end
; CHECK-LABEL: define private ptr @drop.allocate(
; CHECK: call ptr @__reussir_allocate(i64 128,
; SPLIT-LABEL: define private {{.*}} @drop.start(
; SPLIT: call ptr @drop.allocate
; SPLIT: call void @locked
; SPLIT-LABEL: define internal {{.*}} @drop.start.resume.0(
; SPLIT-NOT: call void @locked
; SPLIT-LABEL: define internal {{.*}} @drop.start.resume.1(
; SPLIT: call void @observe
; SPLIT: call void @__reussir_dealloc_unsized

; SPLIT-LABEL: define private void @drop.start.callback(ptr %0, ptr %1, ptr %2)
; SPLIT: %[[STEP:.*]] = call { ptr, ptr } @drop.start
; SPLIT: %[[RESUME:.*]] = extractvalue { ptr, ptr } %[[STEP]], 0
; SPLIT: %[[VALID:.*]] = icmp ne ptr %[[RESUME]], null
; SPLIT: %[[CHILD:.*]] = extractvalue { ptr, ptr } %[[STEP]], 1
; SPLIT: select i1 %[[VALID]], ptr %[[CHILD]], ptr null
; SPLIT: store { ptr, ptr }
; SPLIT: ret void
