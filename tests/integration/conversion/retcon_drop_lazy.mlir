// RUN: %reussir-opt %s --reussir-convert-to-llvm --canonicalize --verify-each -o %t.mlir
// RUN: %FileCheck %s --check-prefix=MLIR < %t.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.mlir -o %t.ll
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,function(instcombine,simplifycfg),verify' -o - | %FileCheck %s
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,default<O2>,verify' -o - | %FileCheck %s --check-prefix=TAIL

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  llvm.func @observe(!llvm.ptr)
  llvm.func @use_payload(!llvm.ptr)

  // MLIR owns both root paths and keeps the eventual frame bounds symbolic.
  // MLIR-LABEL: llvm.func linkonce_odr @small(
  // MLIR-SAME: passthrough = ["noinline"]
  // MLIR: %[[MAXALIGN:.*]] = llvm.mlir.constant(16 : i64)
  // MLIR: %[[MAXSIZE:.*]] = llvm.mlir.constant(256 : i64)
  // MLIR: %[[DESC:.*]] = llvm.load
  // MLIR: %[[SIZE:.*]] = llvm.extractvalue %[[DESC]][1]
  // MLIR: %[[ALIGN:.*]] = llvm.extractvalue %[[DESC]][2]
  // MLIR: llvm.icmp "ule" %[[SIZE]], %[[MAXSIZE]]
  // MLIR: llvm.icmp "ule" %[[ALIGN]], %[[MAXALIGN]]
  // MLIR: llvm.cond_br
  // MLIR: llvm.alloca %[[SIZE]] x i8 {alignment = 16 : i64}
  // MLIR: llvm.call @small.start
  // MLIR: llvm.call @__reussir_finish_dtor
  // MLIR: llvm.call @__reussir_create_dtor_executor
  // MLIR: llvm.call @__reussir_run_dtor

  // The finalized descriptor folds through ordinary LLVM passes, without a
  // dedicated LLVM wrapper rewrite. Direct root calls retain the internal ABI.
  // CHECK-LABEL: define linkonce_odr void @small(
  // CHECK: %[[FRAME:.*]] = alloca [48 x i8], align 16
  // CHECK: %[[STEP:.*]] = call { ptr, ptr } @small.start(ptr nonnull %[[FRAME]], ptr %{{.*}})
  // CHECK: %[[RESUME:.*]] = extractvalue { ptr, ptr } %[[STEP]], 0
  // CHECK: icmp eq ptr %[[RESUME]], null
  // CHECK: br i1
  // CHECK: %[[CHILD:.*]] = extractvalue { ptr, ptr } %[[STEP]], 1
  // CHECK: call void @__reussir_finish_dtor(ptr nonnull %[[FRAME]], ptr nonnull %[[RESUME]], ptr %[[CHILD]])
  // CHECK-NOT: call ptr @__reussir_create_dtor_executor
  // CHECK: ret void
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
