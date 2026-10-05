// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s --implicit-check-not=reussir.array.to_ifrt --implicit-check-not=reussir.array.from_ifrt --implicit-check-not=__reussir_pjrt_array_allocate --implicit-check-not=__reussir_pjrt_array_deallocate

#s = #ifrt.sharding_param<1x1x1 to [0] on 1>
#m = #ifrt.sharding_param<2x1x1 to [0] on 4>
!I = !ifrt.array<tensor<?x4x?xf32>, #s, [0]>
!M = !ifrt.array<tensor<?x4x?xf32>, #m, [3, 1, 2, 0]>
!R = !reussir.rc<!reussir.array<? x 4 x ? x f32, #reussir.target<devices = [0], sharding = #s>>>
!RM = !reussir.rc<!reussir.array<? x 4 x ? x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #m>> atomic>

module {
  // Borrowing needs no query. Adoption reads the result's actual shape and
  // stores only its dynamic dimensions, in dimension order.
  // CHECK-LABEL: llvm.func @single(
  // CHECK-SAME: %[[INPUT:.*]]: !llvm.ptr, %[[BOX:.*]]: !llvm.ptr
  // CHECK: llvm.alloca {{.*}} x i64
  // CHECK: llvm.getelementptr %[[INPUT]][0, 1]
  // CHECK-NOT: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: reussir.pjrt.jit_call
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: %[[LOCAL0:.*]] = llvm.load
  // CHECK: llvm.load
  // CHECK: %[[LOCAL2:.*]] = llvm.load
  // CHECK: %[[PRODUCT0:.*]] = "llvm.intr.smul.with.overflow"(%[[LOCAL0]],
  // CHECK: %[[EXTENT0:.*]] = llvm.extractvalue %[[PRODUCT0]][0]
  // CHECK: llvm.mlir.constant(4 : i64)
  // CHECK: llvm.icmp "eq"
  // CHECK: %[[PRODUCT2:.*]] = "llvm.intr.smul.with.overflow"(%[[LOCAL2]],
  // CHECK: %[[EXTENT2:.*]] = llvm.extractvalue %[[PRODUCT2]][0]
  // CHECK: %[[DST0:.*]] = llvm.getelementptr %[[BOX]][0, 1, 2]
  // CHECK: llvm.store %[[EXTENT0]], %[[DST0]]
  // CHECK: %[[DST2:.*]] = llvm.getelementptr %[[BOX]][0, 1, 3]
  // CHECK: llvm.store %[[EXTENT2]], %[[DST2]]
  // CHECK: llvm.return %[[BOX]]
  func.func @single(%input: !R, %token: !reussir.token<align : 8, size : 40>) -> !R attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 40>) : !I -> !R
    return %result : !R
  }

  // Four devices hold two shards with replication; extents scale by two.
  // CHECK-LABEL: llvm.func @sharded(
  // CHECK: reussir.pjrt.jit_call
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: %[[LOCAL0:.*]] = llvm.load
  // CHECK: %[[LOCAL1:.*]] = llvm.load
  // CHECK: %[[LOCAL2:.*]] = llvm.load
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: %[[OTHER0:.*]] = llvm.load
  // CHECK: llvm.icmp "eq" %[[OTHER0]], %[[LOCAL0]]
  // CHECK: %[[OTHER1:.*]] = llvm.load
  // CHECK: llvm.icmp "eq" %[[OTHER1]], %[[LOCAL1]]
  // CHECK: %[[OTHER2:.*]] = llvm.load
  // CHECK: llvm.icmp "eq" %[[OTHER2]], %[[LOCAL2]]
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions
  // CHECK: %[[TWO:.*]] = llvm.mlir.constant(2 : i64)
  // CHECK: "llvm.intr.smul.with.overflow"(%[[LOCAL0]], %[[TWO]]
  // CHECK: %[[ONE:.*]] = llvm.mlir.constant(1 : i64)
  // CHECK: "llvm.intr.smul.with.overflow"(%[[LOCAL1]], %[[ONE]]
  // CHECK: llvm.return
  func.func @sharded(%input: !RM, %token: !reussir.token<align : 8, size : 64>) -> !RM attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !RM -> !M
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [3, 1, 2, 0] : (!M) -> !M
    %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 64>) : !M -> !RM
    return %result : !RM
  }

  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<?x4x?xf32>) -> tensor<?x4x?xf32> {
      return %arg : tensor<?x4x?xf32>
    }
  }
}
