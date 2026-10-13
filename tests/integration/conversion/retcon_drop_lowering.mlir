// RUN: %reussir-opt %s --reussir-convert-to-llvm --verify-each | %reussir-translate --reussir-to-llvmir -o %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,verify' -o - | %FileCheck %s --check-prefix=SPLIT

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  llvm.func @observe(!llvm.ptr)
  llvm.func @use_payload(!llvm.ptr)
  llvm.func internal @locked(%p: !llvm.ptr) {
    llvm.call @drop(%p) : (!llvm.ptr) -> ()
    llvm.return
  }

  // CHECK: @drop.routine = private global { ptr, i64, i64 } { ptr @drop.start, i64 48, i64 8 }
  // CHECK-LABEL: define linkonce_odr void @drop(
  // CHECK: call i64 @__reussir_dtor_executor_size()
  // CHECK: call i64 @__reussir_dtor_executor_align()
  // CHECK: call ptr @__reussir_create_dtor_executor
  // CHECK: call void @__reussir_run_dtor
  // CHECK: call void @__reussir_destroy_dtor_executor
  // CHECK: ret void
  llvm.func linkonce_odr @drop(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    %one = llvm.mlir.constant(1 : i64) : i64
    %payload = llvm.alloca %one x !llvm.array<128 x i8> {alignment = 128 : i64} : (i64) -> !llvm.ptr
    llvm.call @use_payload(%payload) : (!llvm.ptr) -> ()
    llvm.call @locked(%p) : (!llvm.ptr) -> ()
    %child = llvm.load %p : !llvm.ptr -> !llvm.ptr
    %null = llvm.mlir.zero : !llvm.ptr
    %has = llvm.icmp "ne" %child, %null : !llvm.ptr
    llvm.cond_br %has, ^recurse, ^done
  ^recurse:
    reussir.dtor.suspend @drop(%child : !llvm.ptr)
    reussir.dtor.suspend @drop(%child : !llvm.ptr)
    llvm.br ^done
  ^done:
    llvm.call @use_payload(%payload) : (!llvm.ptr) -> ()
    llvm.call @observe(%p) : (!llvm.ptr) -> ()
    llvm.return
  }
}

// CHECK-LABEL: define private {{.*}} @drop.start(
// CHECK: call token @llvm.coro.id.retcon(i32 48, i32 8,
// CHECK: call ptr @llvm.coro.begin
// CHECK: call void @locked
// CHECK-NOT: call void @drop(
// CHECK: call i1 (...) @llvm.coro.suspend.retcon.i1
// CHECK: call i1 (...) @llvm.coro.suspend.retcon.i1
// CHECK: call void @observe
// CHECK: call void @llvm.coro.end
// CHECK-LABEL: define private ptr @drop.allocate(
// CHECK-NEXT: unreachable
// SPLIT: @drop.routine = private constant { ptr, i64, i64 } { ptr @drop.start.callback, i64 {{[0-9]+}}, i64 128 }
// SPLIT-LABEL: define linkonce_odr void @drop(
// SPLIT: call ptr @__reussir_create_dtor_executor
// SPLIT: call void @__reussir_run_dtor
// SPLIT-NOT: call void @__reussir_finish_dtor
// SPLIT: ret void
// SPLIT-LABEL: define private {{.*}} @drop.start(
// SPLIT-NOT: call ptr @drop.allocate
// SPLIT: call void @locked
// SPLIT-LABEL: define internal {{.*}} @drop.start.resume.0(
// SPLIT-NOT: call void @locked
// SPLIT-LABEL: define internal {{.*}} @drop.start.resume.1(
// SPLIT: call void @observe
// SPLIT-NOT: call void @drop.deallocate
// SPLIT-LABEL: define private ptr @drop.start.callback(ptr %0, ptr %1, ptr %2)
// SPLIT: %[[STEP:.*]] = call { ptr, ptr } @drop.start
// SPLIT: %[[RESUME:.*]] = extractvalue { ptr, ptr } %[[STEP]], 0
// SPLIT: %[[VALID:.*]] = icmp ne ptr %[[RESUME]], null
// SPLIT-NEXT: br i1 %[[VALID]], label %yield, label %done
// SPLIT: yield:
// SPLIT: %[[CHILD:.*]] = extractvalue { ptr, ptr } %[[STEP]], 1
// SPLIT: store ptr %[[CHILD]], ptr %2
// SPLIT: done:
// SPLIT-NEXT: ret ptr %[[RESUME]]
