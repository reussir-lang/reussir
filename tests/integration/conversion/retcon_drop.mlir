// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true retcon-drops=true' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true retcon-drops=false' --verify-each | %FileCheck %s --check-prefix=DIRECT --implicit-check-not=.retcon --implicit-check-not=__reussir_locked_drop

!tree_ = !reussir.record<compound "Tree" incomplete>
!tree = !reussir.record<compound "Tree" { !reussir.rc<!reussir.cell<!reussir.rc<!tree_ shared> mutex> atomic>, !tree_, !tree_ }>
module attributes {llvm.target_triple = "x86_64-unknown-linux-gnu"} {
  // CHECK-LABEL: func.func private @__reussir_locked_drop(
  // CHECK-SAME: reussir.locked_drop
  // CHECK-NOT: reussir.retcon_drop
  // CHECK: func.call @{{.*}}TreeE.retcon
  // CHECK-LABEL: func.func private @{{.*}}TreeE.retcon(
  // CHECK-SAME: reussir.retcon_drop
  // CHECK: sync.mutex.critical_section
  // CHECK: func.call @__reussir_locked_drop
  // CHECK: reussir.dtor.suspend @{{.*}}TreeE.retcon
  // CHECK: reussir.dtor.suspend @{{.*}}TreeE.retcon
  // CHECK-LABEL: func.func @drop(
  // CHECK: call @{{.*}}TreeE.retcon
  // DIRECT-LABEL: func.func @drop(
  // DIRECT: call @{{.*}}TreeE(
  func.func @drop(%ref: !reussir.ref<!tree>) {
    reussir.ref.drop (%ref : !reussir.ref<!tree>)
    return
  }
}

// -----

!tree_ = !reussir.record<compound "WindowsTree" incomplete>
!tree = !reussir.record<compound "WindowsTree" { !tree_, !tree_ }>
module attributes {llvm.target_triple = "x86_64-pc-windows-msvc"} {
  // CHECK-LABEL: module attributes
  // CHECK: func.func private @{{.*}}WindowsTreeE.retcon
  // CHECK: func.func @windows_drop(
  // CHECK: call @{{.*}}WindowsTreeE.retcon(
  // DIRECT-LABEL: func.func @windows_drop(
  func.func @windows_drop(%ref: !reussir.ref<!tree>) {
    reussir.ref.drop (%ref : !reussir.ref<!tree>)
    return
  }
}
