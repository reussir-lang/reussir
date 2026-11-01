// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s --check-prefix=ROUNDTRIP
// RUN: %reussir-opt %s --reussir-convert-to-llvm --verify-each | %FileCheck %s

// `rc.defer` and `drop.run` become calls of the runtime's drop stack, the
// callee passed by address (the runtime calls it back as `void(ptr)`). A
// box with 8 bytes of header whose upper 16 bits are free (a compound
// record whose payload starts at offset 8, a fused variant) is deferred
// through `__reussir_drop_defer_wide`, which may link it through them; a
// compound record whose payload starts at offset 4 through
// `__reussir_drop_defer`.

!node_ = !reussir.record<compound "DeferNode" incomplete>
!node = !reussir.record<compound "DeferNode" {i64, !node_}>
!small = !reussir.record<compound "DeferSmall" {i32, i32}>
!tree_ = !reussir.record<variant "DeferTree" incomplete>
!leaf = !reussir.record<compound "DeferLeaf" [value] {}>
!fork = !reussir.record<compound "DeferFork" [value] {!tree_, !tree_}>
!tree = !reussir.record<variant "DeferTree" {!leaf, !fork}>

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  // CHECK-DAG: llvm.func @__reussir_drop_defer(!llvm.ptr, !llvm.ptr) attributes {passthrough = ["nounwind"]}
  // CHECK-DAG: llvm.func @__reussir_drop_defer_wide(!llvm.ptr, !llvm.ptr) attributes {passthrough = ["nounwind"]}
  // CHECK-DAG: llvm.func @__reussir_drop_run(!llvm.ptr, !llvm.ptr) attributes {passthrough = ["nounwind"]}
  func.func private @node_release(!reussir.rc<!node>)
  func.func private @node_walk(!reussir.ref<!node>)
  func.func private @small_release(!reussir.rc<!small>)
  func.func private @tree_release(!reussir.rc<!tree>)

  // ROUNDTRIP-LABEL: func.func @defer
  // ROUNDTRIP: reussir.rc.defer @node_release(%{{.*}} : !reussir.rc<
  // CHECK-LABEL: llvm.func @defer
  // CHECK: %[[FN:.*]] = llvm.mlir.addressof @node_release : !llvm.ptr
  // CHECK: llvm.call @__reussir_drop_defer_wide(%{{.*}}, %[[FN]]) : (!llvm.ptr, !llvm.ptr) -> ()
  func.func @defer(%box: !reussir.rc<!node>) {
    reussir.rc.defer @node_release(%box : !reussir.rc<!node>)
    return
  }

  // CHECK-LABEL: llvm.func @defer_narrow
  // CHECK: %[[FN:.*]] = llvm.mlir.addressof @small_release : !llvm.ptr
  // CHECK: llvm.call @__reussir_drop_defer(%{{.*}}, %[[FN]]) : (!llvm.ptr, !llvm.ptr) -> ()
  func.func @defer_narrow(%box: !reussir.rc<!small>) {
    reussir.rc.defer @small_release(%box : !reussir.rc<!small>)
    return
  }

  // CHECK-LABEL: llvm.func @defer_variant
  // CHECK: %[[FN:.*]] = llvm.mlir.addressof @tree_release : !llvm.ptr
  // CHECK: llvm.call @__reussir_drop_defer_wide(%{{.*}}, %[[FN]]) : (!llvm.ptr, !llvm.ptr) -> ()
  func.func @defer_variant(%box: !reussir.rc<!tree>) {
    reussir.rc.defer @tree_release(%box : !reussir.rc<!tree>)
    return
  }

  // ROUNDTRIP-LABEL: func.func @run
  // ROUNDTRIP: reussir.drop.run @node_walk(%{{.*}} : !reussir.ref<
  // CHECK-LABEL: llvm.func @run
  // CHECK: %[[FN:.*]] = llvm.mlir.addressof @node_walk : !llvm.ptr
  // CHECK: llvm.call @__reussir_drop_run(%{{.*}}, %[[FN]]) : (!llvm.ptr, !llvm.ptr) -> ()
  func.func @run(%ref: !reussir.ref<!node>) {
    reussir.drop.run @node_walk(%ref : !reussir.ref<!node>)
    return
  }
}
