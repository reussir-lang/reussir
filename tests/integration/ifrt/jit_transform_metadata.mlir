// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce | %FileCheck %s --implicit-check-not=ifrt.function --implicit-check-not=ifrt.donated

#s = #ifrt.sharding_param<2 to [0] on 2>
!I = !ifrt.array<tensor<8xf32>, #s, [3, 1], memory_kind = "device", layout = "{0}">
module {
  // CHECK-LABEL: func.func @main
  // CHECK: %[[A:.*]], %[[READY:.*]] = reussir.pjrt.jit_call @[[CODE:[A-Za-z0-9_]+]]
  // CHECK-SAME: on devices [3, 1]
  // CHECK-SAME: arg_attrs = [{test.arg}]
  // CHECK-SAME: io_aliases = [array<i32: 0, 0>]
  // CHECK-SAME: res_attrs = [{test.result}]
  // CHECK-SAME: test.option = "kept"
  // CHECK: reussir.pjrt.jit_call @[[CODE]](%[[A]]) after(%[[READY]]) on devices [3, 1]
  // CHECK-SAME: donated_input_indices = array<i32: 0>
  func.func @main(%input: !I {ifrt.donated}) -> !I attributes {ifrt.function} {
    %a, %ready = ifrt.Call @kernel::@main(%input) on devices [3, 1] {io_aliases = [array<i32: 0, 0>], arg_attrs = [{test.arg}], res_attrs = [{test.result}], test.option = "kept"} : (!I) -> !I
    %b, %done = ifrt.Call @kernel::@main(%a) after %ready on devices [3, 1] {donated_input_indices = array<i32: 0>} : (!I) -> !I
    return %b : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      return %arg : tensor<8xf32> loc("kernel.rr":12:5)
    }
  }
  // CHECK-NOT: module @kernel
  // CHECK: reussir.ifrt.bytecode "private" @[[CODE]]
  // CHECK-SAME: bytecode "{{.*}}kernel.rr{{.*}}"
  // CHECK-NOT: reussir.ifrt.bytecode
}
