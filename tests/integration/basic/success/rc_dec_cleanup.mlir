// RUN: %reussir-opt %s --symbol-dce | %FileCheck %s
// RUN: %reussir-opt %s --mlir-print-op-generic | %reussir-opt --symbol-dce | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc --symbol-dce | %FileCheck %s

!handle = !reussir.rc<!reussir.ffi_object<"T", @drop>>
module {
  // CHECK: func.func private @drop(
  func.func private @drop(!handle)
  // CHECK-NOT: @unused
  func.func private @unused(!handle)
  // CHECK-LABEL: func.func @use(
  func.func @use(%raw: i64) {
    %x = builtin.unrealized_conversion_cast %raw : i64 to !handle
    // CHECK: reussir.rc.dec{{.*}}cleanup(@drop)
    reussir.rc.dec(%x : !handle) cleanup(@drop)
    return
  }
}
