// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation | %FileCheck %s

module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // CHECK-LABEL: func.func @mixed
  // CHECK-DAG: %[[FOUR:.+]] = arith.constant 4 : index
  // CHECK-DAG: %[[HEADER:.+]] = arith.constant 48 : index
  // CHECK: %[[COUNT:.+]] = arith.muli %arg0, %[[FOUR]]
  // CHECK: %[[PAYLOAD:.+]] = arith.muli %[[COUNT]], %[[FOUR]]
  // CHECK: %[[BYTES:.+]] = arith.addi %[[PAYLOAD]], %[[HEADER]]
  // CHECK: %[[TOKEN:.+]] = reussir.token.alloc(%[[BYTES]] : index)
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%[[TOKEN]]
  func.func @mixed(%n: index) -> !reussir.rc<!reussir.array<? x 4 x i32>> {
    %value = arith.constant 7 : i32
    %rc = reussir.array.create init(%value : i32) extents(%n) : !reussir.rc<!reussir.array<? x 4 x i32>>
    return %rc : !reussir.rc<!reussir.array<? x 4 x i32>>
  }

  // CHECK-LABEL: func.func @provided_token
  // CHECK-NOT: reussir.token.alloc
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%arg2
  func.func @provided_token(%n: index, %init: i32, %token: !reussir.token<align: 8, size: ?>) -> !reussir.rc<!reussir.array<? x i32>> {
    %rc = reussir.array.create init(%init : i32) extents(%n) token(%token : !reussir.token<align: 8, size: ?>) : !reussir.rc<!reussir.array<? x i32>>
    return %rc : !reussir.rc<!reussir.array<? x i32>>
  }

}
