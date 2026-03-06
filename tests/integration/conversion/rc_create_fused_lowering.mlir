// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-rc-create-fusion --reussir-lowering-basic-ops | \
// RUN: %reussir-translate --mlir-to-llvmir | %FileCheck %s

!cons = !reussir.record<compound "List::Cons" [value] { i32, i64 }>
!nil = !reussir.record<compound "List::Nil" [value] {}>
!list = !reussir.record<variant "List" {!cons, !nil}>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  func.func @mk_compound(%a: i32, %b: i64) -> !reussir.rc<!cons> {
    %c = reussir.record.compound(%a, %b : i32, i64) : !cons
    %rc = reussir.rc.create value(%c : !cons) : !reussir.rc<!cons>
    return %rc : !reussir.rc<!cons>
  }

  func.func @mk_variant(%a: i32, %b: i64) -> !reussir.rc<!list> {
    %c = reussir.record.compound(%a, %b : i32, i64) : !cons
    %v = reussir.record.variant [0] (%c : !cons) : !list
    %rc = reussir.rc.create value(%v : !list) : !reussir.rc<!list>
    return %rc : !reussir.rc<!list>
  }
}

// CHECK-LABEL: define ptr @mk_compound
// CHECK-NOT: alloca %"List::Cons"
// CHECK: %[[ALLOC:.*]] = call ptr @__reussir_allocate(i64 8, i64 24)
// CHECK: %[[COUNT:.*]] = getelementptr { i64, %"List::Cons" }, ptr %[[ALLOC]], i32 0, i32 0
// CHECK: store i64 1, ptr %[[COUNT]], align 8
// CHECK: %[[PAYLOAD:.*]] = getelementptr { i64, %"List::Cons" }, ptr %[[ALLOC]], i32 0, i32 1
// CHECK: %[[FIELD0:.*]] = getelementptr %"List::Cons", ptr %[[PAYLOAD]], i32 0, i32 0
// CHECK: store i32 %0, ptr %[[FIELD0]], align 4, !invariant.group
// CHECK: %[[FIELD1:.*]] = getelementptr %"List::Cons", ptr %[[PAYLOAD]], i32 0, i32 1
// CHECK: store i64 %1, ptr %[[FIELD1]], align 8, !invariant.group

// CHECK-LABEL: define ptr @mk_variant
// CHECK-NOT: alloca %List
// CHECK-NOT: alloca %"List::Cons"
// CHECK: %[[ALLOC:.*]] = call ptr @__reussir_allocate(i64 8, i64 32)
// CHECK: %[[COUNT:.*]] = getelementptr { i64, %List }, ptr %[[ALLOC]], i32 0, i32 0
// CHECK: store i64 1, ptr %[[COUNT]], align 8
// CHECK: %[[VARIANT:.*]] = getelementptr { i64, %List }, ptr %[[ALLOC]], i32 0, i32 1
// CHECK: %[[TAGPTR:.*]] = getelementptr %List, ptr %[[VARIANT]], i32 0, i32 0
// CHECK: store i64 0, ptr %[[TAGPTR]], align 8
// CHECK: %[[PAYLOAD:.*]] = getelementptr %List, ptr %[[VARIANT]], i32 0, i32 1
// CHECK: %[[FIELD0:.*]] = getelementptr %"List::Cons", ptr %[[PAYLOAD]], i32 0, i32 0
// CHECK: store i32 %0, ptr %[[FIELD0]], align 4, !invariant.group
// CHECK: %[[FIELD1:.*]] = getelementptr %"List::Cons", ptr %[[PAYLOAD]], i32 0, i32 1
// CHECK: store i64 %1, ptr %[[FIELD1]], align 8, !invariant.group
