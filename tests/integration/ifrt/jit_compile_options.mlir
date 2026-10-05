// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-opt %t.mlir --reussir-ifrt-just-in-time-transform --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc | %FileCheck %s

// Every call shares bytecode, but keeps its own assignment and execution mode.
// CHECK-LABEL: func.func @single
// CHECK: reussir.pjrt.jit_call @[[CODE:[A-Za-z0-9_]+]]
// CHECK-SAME: on devices [0]
// CHECK-SAME: compile_options = "{{.+}}"
// CHECK-LABEL: func.func @partitioned
// CHECK: reussir.pjrt.jit_call @[[CODE]]
// CHECK-SAME: on devices [3, 1]
// CHECK-SAME: compile_options = "{{.+}}"
// CHECK-LABEL: func.func @reordered
// CHECK: reussir.pjrt.jit_call @[[CODE]]
// CHECK-SAME: on devices [1, 3]
// CHECK-SAME: compile_options = "{{.+}}"
// CHECK-LABEL: func.func @local
// CHECK: reussir.pjrt.jit_call @[[CODE]]
// CHECK-SAME: on devices [3, 1]
// CHECK-SAME: compile_options = "{{.+}}"
// CHECK-SAME: ifrt.local_view
// CHECK: reussir.ifrt.bytecode "private" @[[CODE]]
// CHECK-NOT: reussir.ifrt.bytecode

#s = #ifrt.sharding_param<1 to [0] on 1>
#r = #ifrt.sharding_param<1 to [0] on 2>
!S = !ifrt.array<tensor<8xf32>, #s, [0]>
!A = !ifrt.array<tensor<8xf32>, #r, [3, 1]>
!B = !ifrt.array<tensor<8xf32>, #r, [1, 3]>
module {
  func.func @single(%x: !S) -> !S attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%x) on devices [0] : (!S) -> !S
    return %out : !S
  }
  func.func @partitioned(%x: !A) -> !A attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%x) on devices [3, 1] : (!A) -> !A
    return %out : !A
  }
  func.func @reordered(%x: !B) -> !B attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%x) on devices [1, 3] : (!B) -> !B
    return %out : !B
  }
  func.func @local(%x: !A) -> !A attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%x) on devices [3, 1] {ifrt.local_view} : (!A) -> !A
    return %out : !A
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      return %arg : tensor<8xf32>
    }
  }
}
