// RUN: %reussir-opt %s --reussir-lowering-basic-ops | \
// RUN: %reussir-translate --mlir-to-llvmir | %FileCheck %s

!arr = !reussir.array<2 x i64>

module {
  func.func @array_roundtrip(%lhs : i64, %rhs : i64) -> i64 {
    %arr = reussir.array.create(%lhs, %rhs : i64, i64) : !arr
    %updated = reussir.array.insert(%arr : !arr)[1](%lhs : i64) : !arr
    %elt = reussir.array.extract(%updated : !arr)[1] : i64
    return %elt : i64
  }

  func.func @array_project(%arr : !arr, %index : index) -> i64 {
    %ref = reussir.ref.spilled (%arr : !arr) : !reussir.ref<!arr>
    %elt_ref = reussir.array.project (%ref : !reussir.ref<!arr>, %index : index) : !reussir.ref<i64>
    %elt = reussir.ref.load (%elt_ref : !reussir.ref<i64>) : i64
    return %elt : i64
  }

  func.func @array_project_store(%arr : !reussir.rc<!arr flex>, %rhs : i64, %index : index) {
    %ref = reussir.rc.borrow (%arr : !reussir.rc<!arr flex>) : !reussir.ref<!arr flex>
    %elt_ref = reussir.array.project (%ref : !reussir.ref<!arr flex>, %index : index) : !reussir.ref<i64 field>
    reussir.ref.store (%elt_ref : !reussir.ref<i64 field>) (%rhs : i64)
    return
  }
}

// CHECK-LABEL: define i64 @array_roundtrip(i64 %0, i64 %1)
// CHECK: %[[ARR0:.+]] = insertvalue [2 x i64] undef, i64 %0, 0
// CHECK: %[[ARR1:.+]] = insertvalue [2 x i64] %[[ARR0]], i64 %1, 1
// CHECK: %[[UPDATED:.+]] = insertvalue [2 x i64] %[[ARR1]], i64 %0, 1
// CHECK: %[[ELT:.+]] = extractvalue [2 x i64] %[[UPDATED]], 1
// CHECK: ret i64 %[[ELT]]
// CHECK-LABEL: define i64 @array_project([2 x i64] %0, i{{[0-9]+}} %1)
// CHECK: %[[REF:.+]] = alloca [2 x i64]
// CHECK: store [2 x i64] %0, ptr %[[REF]]
// CHECK: %[[PTR:.+]] = getelementptr [2 x i64], ptr %[[REF]], i32 0, i{{[0-9]+}} %1
// CHECK: %[[LOAD:.+]] = load i64, ptr %[[PTR]]
// CHECK: ret i64 %[[LOAD]]
// CHECK-LABEL: define void @array_project_store(ptr %0, i64 %1, i{{[0-9]+}} %2)
// CHECK: %[[DATA:.+]] = getelementptr { ptr, ptr, ptr, [2 x i64] }, ptr %0, i32 0, i32 3
// CHECK: %[[ELT:.+]] = getelementptr [2 x i64], ptr %[[DATA]], i32 0, i{{[0-9]+}} %2
// CHECK: store i64 %1, ptr %[[ELT]]
// CHECK: ret void
