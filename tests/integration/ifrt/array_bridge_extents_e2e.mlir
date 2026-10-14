// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_bridge_extents_e2e.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %not --crash %t.exe negative 2>&1 | %FileCheck %s --check-prefix=NEGATIVE
// RUN: %not --crash %t.exe dynamic 2>&1 | %FileCheck %s --check-prefix=SHAPE
// RUN: %not --crash %t.exe static 2>&1 | %FileCheck %s --check-prefix=SHAPE
// RUN: %not --crash %t.exe unequal 2>&1 | %FileCheck %s --check-prefix=UNEQUAL
// NEGATIVE: IFRT result extent must be nonnegative
// SHAPE: IFRT result physical shard shape mismatch
// UNEQUAL: IFRT result requires equal physical shard shapes

#s = #ifrt.sharding_param<2x2x1 to [0] on 4>
!I = !ifrt.array<tensor<?x3x?xf32>, #s, [3, 1, 2, 0]>
!R = !reussir.rc<!reussir.array<? x 3 x ? x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #s>>>
module {
  func.func @adopt(%rows: index, %columns: index) -> !R attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main() on devices [3, 1, 2, 0] : () -> !I
    %result = reussir.array.from_ifrt %out extents(%rows, %columns) : !I -> !R
    return %result : !R
  }
  func.func @release(%array: !R) {
    reussir.rc.dec(%array : !R)
    return
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main() -> tensor<?x3x?xf32> {
      %out = stablehlo.custom_call @test_shape() : () -> tensor<?x3x?xf32>
      return %out : tensor<?x3x?xf32>
    }
  }
}
