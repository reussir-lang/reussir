// RUN: %reussir-opt %s --reussir-convert-to-llvm --verify-each | %reussir-translate --reussir-to-llvmir -o %t.ll
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,verify' -o - | %FileCheck %s
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,default<O2>,verify' -o - | %FileCheck %s --check-prefix=TAIL

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  llvm.func @observe(!llvm.ptr)
  llvm.func @use_payload(!llvm.ptr)

  // CHECK-LABEL: define linkonce_odr void @small(
  // CHECK: %frame = alloca [{{[0-9]+}} x i8], align 8
  // CHECK: call void @small.start.callback(ptr %frame, ptr %{{.*}}, ptr %step)
  // CHECK: %[[STEP:.*]] = load { ptr, ptr }, ptr %step
  // CHECK: %[[RESUME:.*]] = extractvalue { ptr, ptr } %[[STEP]], 0
  // CHECK: %[[HAS:.*]] = icmp ne ptr %[[RESUME]], null
  // CHECK: br i1 %[[HAS]], label %suspended, label %done
  // CHECK: suspended:
  // CHECK: %[[CHILD:.*]] = extractvalue { ptr, ptr } %[[STEP]], 1
  // CHECK: call void @__reussir_finish_dtor(ptr %frame, ptr %[[RESUME]], ptr %[[CHILD]])
  // CHECK: br label %done
  // CHECK: done:
  // CHECK-NEXT: ret void
  llvm.func linkonce_odr @small(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    reussir.dtor.suspend @small(%p : !llvm.ptr)
    llvm.call @observe(%p) : (!llvm.ptr) -> ()
    llvm.return
  }

  // An address-taken root frame must not inhibit callers' tail-recursion elimination.
  // TAIL-LABEL: define {{.*}} @walk(
  // TAIL-NOT: alloca
  // TAIL: phi
  // TAIL-NOT: call {{.*}} @walk(
  // TAIL: br i1 {{.*}}, label %{{.*}}, label %tailrecurse
  // TAIL-NEXT: }
  llvm.func @walk(%p: !llvm.ptr, %n: i64) -> i64 {
    %zero = llvm.mlir.constant(0 : i64) : i64
    %one = llvm.mlir.constant(1 : i64) : i64
    %done = llvm.icmp "eq" %n, %zero : i64
    llvm.cond_br %done, ^done, ^next
  ^next:
    llvm.call @small(%p) : (!llvm.ptr) -> ()
    %remaining = llvm.sub %n, %one : i64
    %result = llvm.call @walk(%p, %remaining) : (!llvm.ptr, i64) -> i64
    llvm.return %result : i64
  ^done:
    llvm.return %n : i64
  }

  // Large roots must not make the native stack unbounded, even at ordinary alignment.
  // CHECK-LABEL: define linkonce_odr void @large(
  // CHECK: call ptr @__reussir_create_dtor_executor
  // CHECK: call void @__reussir_run_dtor
  // CHECK-NOT: call void @__reussir_finish_dtor
  // CHECK: ret void
  llvm.func linkonce_odr @large(%p: !llvm.ptr) attributes {reussir.retcon_drop} {
    %one = llvm.mlir.constant(1 : i64) : i64
    %payload = llvm.alloca %one x !llvm.array<1024 x i8> {alignment = 8 : i64} : (i64) -> !llvm.ptr
    llvm.call @use_payload(%payload) : (!llvm.ptr) -> ()
    reussir.dtor.suspend @large(%p : !llvm.ptr)
    llvm.call @use_payload(%payload) : (!llvm.ptr) -> ()
    llvm.return
  }
}
