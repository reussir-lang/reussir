// RUN: %reussir-opt %s --reussir-symbol-dce | %FileCheck %s --implicit-check-not=@dead --implicit-check-not=reussir.ssa_type_symbols

!handle = !reussir.rc<!reussir.ffi_object<"Handle", @drop>>
!nested_handle = !reussir.rc<!reussir.ffi_object<"Nested", @nested_drop>>
!node_ = !reussir.record<compound "Node" incomplete>
!node = !reussir.record<compound "Node" {!nested_handle, !reussir.nullable<!reussir.rc<!node_>>}>
!dead = !reussir.rc<!reussir.ffi_object<"Dead", @dead_drop>>

module {
  // CHECK: func.func private @drop
  func.func private @drop(!handle)
  // CHECK: func.func private @nested_drop
  func.func private @nested_drop(!nested_handle)
  func.func private @dead_drop(!dead)

  // CHECK-LABEL: func.func @local
  // CHECK: reussir.rc.dec
  func.func @local(%raw: i64) {
    %handle = builtin.unrealized_conversion_cast %raw : i64 to !handle
    reussir.rc.dec(%handle : !handle)
    return
  }

  // CHECK-LABEL: func.func @nested
  // CHECK: reussir.rc.dec
  func.func @nested(%raw: i64) {
    %node = builtin.unrealized_conversion_cast %raw : i64 to !reussir.rc<!node>
    reussir.rc.dec(%node : !reussir.rc<!node>)
    return
  }

  func.func private @dead_recursive(%raw: i64) {
    %handle = builtin.unrealized_conversion_cast %raw : i64 to !dead
    reussir.rc.dec(%handle : !dead)
    func.call @dead_recursive(%raw) : (i64) -> ()
    return
  }
}
