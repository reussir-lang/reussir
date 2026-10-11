// RUN: %reussir-opt %s --reussir-convert-to-std --reussir-acquire-drop-expansion='outline-record=true' | %FileCheck %s --implicit-check-not=' field' --implicit-check-not='_field'
// The Cell slot uses ref<Value>, matching the existing ownership helper's
// argument. Atomic refcount context stays independent of regional capability.
!box = !reussir.record<compound "Box" {i64}>
!value = !reussir.record<compound "Value" [value] {!box}>
!cell = !reussir.rc<!reussir.cell<!value>>
!mutex = !reussir.rc<!reussir.cell<!value mutex> atomic>
module {
  // CHECK-LABEL: func.func @plain
  // CHECK: reussir.ref.project
  // CHECK: call {{.*}}acquire_in_place
  // CHECK: reussir.ref.load
  // CHECK: call {{.*}}drop_in_place
  // CHECK: memref.store
  func.func @plain(%cell: !cell, %replacement: !value) -> !value {
    %old = reussir.cell.get(%cell : !cell) : !value
    reussir.cell.set(%replacement : !value, %cell : !cell)
    return %old : !value
  }
  // CHECK-LABEL: func.func @locked
  // CHECK: sync.mutex
  // CHECK: reussir.ref.from_memref{{.*}} atomic>
  // CHECK: call {{.*}}acquire_in_place_atomic
  // CHECK: call {{.*}}drop_in_place_atomic
  // CHECK: memref.store
  func.func @locked(%cell: !mutex, %replacement: !value) -> !value {
    %old = reussir.cell.get(%cell : !mutex) : !value
    reussir.cell.set(%replacement : !value, %cell : !mutex)
    return %old : !value
  }
}
