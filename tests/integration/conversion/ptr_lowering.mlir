// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// RUN: %reussir-opt %s --reussir-attach-native-target --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --mlir-to-llvmir | %FileCheck %s

!P = !ptr.ptr<#ptr.generic_space>

// CHECK-LABEL: define ptr @arithmetic(ptr %0, i64 %1)
// CHECK: %[[STEP:.*]] = getelementptr inbounds i8, ptr %0, i64 %1
// CHECK: getelementptr i8, ptr %[[STEP]], i64
func.func @arithmetic(%p: !P, %n: i64) -> !P {
  %a = ptr.ptr_add inbounds %p, %n : !P, i64
  %size = ptr.type_offset !reussir.rc<i64> : i64
  %b = ptr.ptr_add %a, %size : !P, i64
  return %b : !P
}

// CHECK-LABEL: define i32 @difference(ptr %0, ptr %1)
// CHECK: %[[LHS:.*]] = ptrtoint ptr %0 to i64
// CHECK: %[[RHS:.*]] = ptrtoint ptr %1 to i64
// CHECK: %[[DIFF:.*]] = sub nsw i64 %[[LHS]], %[[RHS]]
// CHECK: trunc i64 %[[DIFF]] to i32
func.func @difference(%p: !P, %q: !P) -> i32 {
  %d = ptr.ptr_diff nsw %p, %q : !P -> i32
  return %d : i32
}

// CHECK-LABEL: define i32 @memory(ptr %0, i32 %1)
// CHECK: store volatile i32 %1, ptr %0, align 4, !nontemporal
// CHECK: load i32, ptr %0, align 2, !invariant.load
// CHECK: load volatile i32, ptr %0, align 4, !invariant.group
// CHECK: store atomic i32 %1, ptr %0 release, align 4
// CHECK: load atomic i32, ptr %0 seq_cst, align 4
func.func @memory(%p: !P, %v: i32) -> i32 {
  ptr.store volatile %v, %p nontemporal alignment = 4 : i32, !P
  %a = ptr.load %p invariant alignment = 2 : !P -> i32
  %b = ptr.load volatile %p invariant_group : !P -> i32
  ptr.store %v, %p atomic release alignment = 4 : i32, !P
  %c = ptr.load %p atomic seq_cst alignment = 4 : !P -> i32
  return %c : i32
}

// CHECK-LABEL: define ptr @counted(ptr %0, ptr %1)
// CHECK: %[[OLD:.*]] = load ptr, ptr %0
// CHECK: store ptr %1, ptr %0
// CHECK: ret ptr %[[OLD]]
func.func @counted(%p: !P, %v: !reussir.rc<i64>) -> !reussir.rc<i64> {
  %old = ptr.load %p : !P -> !reussir.rc<i64>
  ptr.store %v, %p : !reussir.rc<i64>, !P
  return %old : !reussir.rc<i64>
}

// CHECK-LABEL: define ptr @addresses(ptr %0, i32 %1)
// CHECK: %[[ADDR:.*]] = ptrtoaddr ptr %0 to i64
// CHECK: %[[MASK:.*]] = zext i32 %1 to i64
// CHECK: %[[MASKED:.*]] = call ptr @llvm.ptrmask.p0.i64(ptr %0, i64 %[[MASK]])
// CHECK: inttoptr i64 %[[ADDR]] to ptr
func.func @addresses(%p: !P, %mask: i32) -> !P {
  %addr = reussir.ptr.addr %p : !P -> i64
  %masked = reussir.ptr.mask %p, %mask : !P, i32
  %q = reussir.ptr.from_addr %addr : i64 -> !P
  %r = ptr.ptr_add %masked, %addr : !P, i64
  %d = ptr.ptr_diff %q, %r : !P -> i64
  %s = ptr.ptr_add %q, %d : !P, i64
  return %s : !P
}

// CHECK-LABEL: define ptr @null()
// CHECK: ret ptr null
func.func @null() -> !P {
  %p = ptr.constant #ptr.null : !P
  return %p : !P
}
