// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// RUN: %reussir-opt %s --reussir-attach-native-target --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// REQUIRES: pointer64

!P = !ptr.ptr<#ptr.generic_space>

// CHECK-LABEL: define ptr @arithmetic(
// CHECK-SAME: ptr %[[P:.*]], i64 %[[N:.*]])
// CHECK: %[[STEP:.*]] = getelementptr inbounds i8, ptr %[[P]], i64 %[[N]]
// CHECK: getelementptr i8, ptr %[[STEP]], i64
func.func @arithmetic(%p: !P, %n: i64) -> !P {
  %a = ptr.ptr_add inbounds %p, %n : !P, i64
  %size = ptr.type_offset !reussir.rc<i64> : i64
  %b = ptr.ptr_add %a, %size : !P, i64
  return %b : !P
}

// CHECK-LABEL: define i32 @difference(
// CHECK-SAME: ptr %[[P:.*]], ptr %[[Q:.*]])
// CHECK: %[[LHS:.*]] = ptrtoaddr ptr %[[P]] to i64
// CHECK: %[[RHS:.*]] = ptrtoaddr ptr %[[Q]] to i64
// CHECK: %[[DIFF:.*]] = sub nsw i64 %[[LHS]], %[[RHS]]
// CHECK: trunc i64 %[[DIFF]] to i32
func.func @difference(%p: !P, %q: !P) -> i32 {
  %d = ptr.ptr_diff nsw %p, %q : !P -> i32
  return %d : i32
}

// CHECK-LABEL: define i128 @wide_difference(
// CHECK: %[[DIFF:.*]] = sub nuw i64
// CHECK: %[[BOTH:.*]] = sub nuw nsw i64
// CHECK: %[[PLAIN:.*]] = sub i64
// CHECK: sext i64 %[[PLAIN]] to i128
func.func @wide_difference(%p: !P, %q: !P) -> i128 {
  %a = ptr.ptr_diff nuw %p, %q : !P -> i64
  %b = ptr.ptr_diff nuw | nsw %p, %q : !P -> i64
  %c = ptr.ptr_diff %p, %q : !P -> i128
  return %c : i128
}

// CHECK-LABEL: define i32 @memory(
// CHECK-SAME: ptr %[[P:.*]], i32 %[[V:.*]])
// CHECK: store volatile i32 %[[V]], ptr %[[P]], align 4, !nontemporal
// CHECK: load i32, ptr %[[P]], align 2, !invariant.load
// CHECK: load volatile i32, ptr %[[P]], align 4, !invariant.group
// CHECK: load i32, ptr %[[P]], align 4, !nontemporal
// CHECK: store i32 %[[V]], ptr %[[P]], align 4, !invariant.group
// CHECK: store atomic i32 %[[V]], ptr %[[P]] release, align 4
// CHECK: load atomic i32, ptr %[[P]] syncscope("singlethread") seq_cst, align 4
func.func @memory(%p: !P, %v: i32) -> i32 {
  ptr.store volatile %v, %p nontemporal alignment = 4 : i32, !P
  %a = ptr.load %p invariant alignment = 2 : !P -> i32
  %b = ptr.load volatile %p invariant_group : !P -> i32
  %n = ptr.load %p nontemporal : !P -> i32
  ptr.store %v, %p invariant_group : i32, !P
  ptr.store %v, %p atomic release alignment = 4 : i32, !P
  %c = ptr.load %p atomic syncscope("singlethread") seq_cst alignment = 4 : !P -> i32
  return %c : i32
}

// CHECK-LABEL: define ptr @addresses(
// CHECK-SAME: ptr %[[P:.*]], i32 %[[M:.*]])
// CHECK: %[[ADDR:.*]] = ptrtoaddr ptr %[[P]] to i64
// CHECK: %[[MASK:.*]] = zext i32 %[[M]] to i64
// CHECK: %[[MASKED:.*]] = call ptr @llvm.ptrmask.p0.i64(ptr %[[P]], i64 %[[MASK]])
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

// CHECK-LABEL: define ptr @resized_addresses(
// CHECK-SAME: ptr %[[P:.*]], i128 %[[M:.*]])
// CHECK: %[[ADDR:.*]] = ptrtoaddr ptr %[[P]] to i64
// CHECK: %[[LOW:.*]] = trunc i64 %[[ADDR]] to i32
// CHECK: %[[MASK:.*]] = trunc i128 %[[M]] to i64
// CHECK: call ptr @llvm.ptrmask.p0.i64(ptr %[[P]], i64 %[[MASK]])
// CHECK: inttoptr i32 %[[LOW]] to ptr
func.func @resized_addresses(%p: !P, %mask: i128) -> !P {
  %addr = reussir.ptr.addr %p : !P -> i32
  %index = reussir.ptr.addr %p : !P -> index
  %masked = reussir.ptr.mask %p, %mask : !P, i128
  %q = reussir.ptr.from_addr %addr : i32 -> !P
  %r = ptr.ptr_add %masked, %index : !P, index
  %d = ptr.ptr_diff %q, %r : !P -> i64
  %s = ptr.ptr_add %q, %d : !P, i64
  return %s : !P
}

// CHECK-LABEL: define ptr @provenance(
// CHECK-SAME: ptr %[[P:.*]])
// CHECK: %[[ADDR:.*]] = ptrtoint ptr %[[P]] to i32
// CHECK: %[[Q:.*]] = inttoptr i32 %[[ADDR]] to ptr
// CHECK: %[[FRESH:.*]] = call ptr @llvm.launder.invariant.group.p0(ptr %[[Q]])
// CHECK: call ptr @llvm.strip.invariant.group.p0(ptr %[[FRESH]])
func.func @provenance(%p: !P) -> !P {
  %addr = reussir.ptr.expose_addr %p : !P -> i32
  %q = reussir.ptr.from_addr %addr : i32 -> !P
  %fresh = reussir.ptr.launder_invariant_group %q : !P
  %plain = reussir.ptr.strip_invariant_group %fresh : !P
  return %plain : !P
}

// CHECK-LABEL: define ptr @str_data(
// CHECK-SAME: { ptr, i64 } %[[S:.*]])
// CHECK: extractvalue { ptr, i64 } %[[S]], 0
func.func @str_data(%s: !reussir.str<global>) -> !P {
  %p = reussir.str.data_ptr (%s : !reussir.str<global>) : !P
  return %p : !P
}

// CHECK-LABEL: define ptr @view_data(
// CHECK: ret ptr
func.func @view_data(%m: memref<4xf64>) -> !P {
  %c = memref.memory_space_cast %m : memref<4xf64> to memref<4xf64, #ptr.generic_space>
  %p = ptr.to_ptr %c : memref<4xf64, #ptr.generic_space> -> !P
  return %p : !P
}

// A masked access without an alignment is byte-aligned.
// CHECK-LABEL: define <4 x float> @masked(
// CHECK-SAME: ptr %[[P:.*]], <4 x i1> %[[M:.*]], <4 x float> %[[V:.*]])
// CHECK: %[[L:.*]] = call <4 x float> @llvm.masked.load.v4f32.p0(ptr align 4 %[[P]], <4 x i1> %[[M]], <4 x float> %[[V]])
// CHECK: call void @llvm.masked.store.v4f32.p0(<4 x float> %[[L]], ptr align 1 %[[P]], <4 x i1> %[[M]])
func.func @masked(%p: !P, %m: vector<4xi1>, %v: vector<4xf32>) -> vector<4xf32> {
  %l = ptr.masked_load %p, %m, %v alignment = 4 : !P -> vector<4xf32>
  ptr.masked_store %l, %p, %m : vector<4xf32>, !P
  return %l : vector<4xf32>
}

// CHECK-LABEL: define ptr @null()
// CHECK: ret ptr null
func.func @null() -> !P {
  %p = ptr.constant #ptr.null : !P
  return %p : !P
}
