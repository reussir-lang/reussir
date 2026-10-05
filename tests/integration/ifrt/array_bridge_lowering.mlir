// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s --implicit-check-not=reussir.array.to_ifrt --implicit-check-not=reussir.array.from_ifrt --implicit-check-not=__reussir_pjrt_array_allocate --implicit-check-not=__reussir_pjrt_array_deallocate --implicit-check-not=__reussir_pjrt_array_wait_ready --implicit-check-not=__reussir_pjrt_array_dimensions

#s = #ifrt.sharding_param<1 to [0] on 1>
#m = #ifrt.sharding_param<2 to [0] on 2>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!M = !ifrt.array<tensor<8xf32>, #m, [3, 1]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
!RM = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [3, 1], sharding = #m>> atomic>

module {
  // Borrow the payload without allocating or changing its refcount.
  // CHECK-LABEL: llvm.func @single(
  // CHECK-SAME: %[[INPUT:.*]]: !llvm.ptr, %[[BOX:.*]]: !llvm.ptr
  // CHECK: llvm.getelementptr %[[INPUT]][0, 1, 1]
  // CHECK: llvm.load
  // CHECK: llvm.icmp "eq"
  // CHECK: %[[VIEW:.*]] = llvm.getelementptr %[[INPUT]][0, 1]
  // CHECK: llvm.call @__reussir_pjrt_executable_execute
  // CHECK: %[[SLOT:.*]] = llvm.getelementptr %[[RESULT:.*]][0]
  // CHECK: %[[HANDLE:.*]] = llvm.load %[[SLOT]]
  // CHECK: %[[ONE:.*]] = llvm.mlir.constant(1 : i32)
  // CHECK: llvm.store %[[ONE]], %[[BOX]]
  // CHECK: %[[DST:.*]] = llvm.getelementptr %[[BOX]][0, 1, 0]
  // CHECK: llvm.store %[[HANDLE]], %[[DST]]
  // CHECK: %[[ZERO:.*]] = llvm.mlir.constant(0 : i64)
  // CHECK: %[[OFFSET:.*]] = llvm.getelementptr %[[BOX]][0, 1, 1]
  // CHECK: llvm.store %[[ZERO]], %[[OFFSET]]
  // CHECK: llvm.return %[[BOX]]
  func.func @single(%input: !R, %token: !reussir.token<align : 8, size : 24>) -> !R attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 24>) : !I -> !R
    return %result : !R
  }

  // Adopt both shards in their original order, including atomic RC boxes.
  // CHECK-LABEL: llvm.func @sharded(
  // CHECK-SAME: %[[INPUT:.*]]: !llvm.ptr, %[[BOX:.*]]: !llvm.ptr
  // CHECK: llvm.getelementptr %[[INPUT]][0, 1]
  // CHECK: llvm.call @__reussir_pjrt_executable_execute
  // CHECK: %[[SLOT0:.*]] = llvm.getelementptr %[[RESULT:.*]][0]
  // CHECK: %[[HANDLE0:.*]] = llvm.load %[[SLOT0]]
  // CHECK: %[[SLOT1:.*]] = llvm.getelementptr %[[RESULT]][1]
  // CHECK: %[[HANDLE1:.*]] = llvm.load %[[SLOT1]]
  // CHECK: %[[ONE:.*]] = llvm.mlir.constant(1 : i32)
  // CHECK: llvm.store %[[ONE]], %[[BOX]]
  // CHECK: llvm.insertvalue %[[HANDLE0]], {{.*}}[0]
  // CHECK: %[[HANDLES:.*]] = llvm.insertvalue %[[HANDLE1]], {{.*}}[1]
  // CHECK: %[[DST:.*]] = llvm.getelementptr %[[BOX]][0, 1, 0]
  // CHECK: llvm.store %[[HANDLES]], %[[DST]]
  // CHECK: llvm.return %[[BOX]]
  func.func @sharded(%input: !RM, %token: !reussir.token<align : 8, size : 32>) -> !RM attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !RM -> !M
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [3, 1] : (!M) -> !M
    %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 32>) : !M -> !RM
    return %result : !RM
  }

  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      return %arg : tensor<8xf32>
    }
  }
}
