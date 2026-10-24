// REQUIRES: openxla, linux, pjrt-runtime
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_transfer_uneven_kernel.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env XLA_FLAGS="--xla_force_host_platform_device_count=4" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" REUSSIR_PJRT_CACHE_CONFIG="%S/../runtime/Inputs/pjrt_cache.toml" %t.exe

#s = #ifrt.sharding_param<2x2 to [1,0] on 2x2>
!I = !ifrt.array<tensor<3x5xf32>, #s, [3, 1, 2, 0]>
!R = !reussir.rc<!reussir.array<3 x 5 x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #s>>>
!SI = !ifrt.array<tensor<1x1xf32>, #s, [3, 1, 2, 0]>
!SR = !reussir.rc<!reussir.array<1 x 1 x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #s>>>
module {
  func.func @reverse(%source: memref<3x5xf32>, %destination: memref<3x5xf32>) attributes {llvm.emit_c_interface, ifrt.function} {
    %input = reussir.array.to_device %source : memref<3x5xf32> -> !R
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [3, 1, 2, 0] : (!I) -> !I
    %result = reussir.array.from_ifrt %out : !I -> !R
    reussir.array.to_host %result into %destination : !R, memref<3x5xf32>
    reussir.rc.dec(%input : !R)
    reussir.rc.dec(%result : !R)
    return
  }
  func.func @small(%source: memref<1x1xf32>, %destination: memref<1x1xf32>) attributes {llvm.emit_c_interface, ifrt.function} {
    %input = reussir.array.to_device %source : memref<1x1xf32> -> !SR
    %view = reussir.array.to_ifrt %input : !SR -> !SI
    %out, %done = ifrt.Call @tiny::@main(%view) on devices [3, 1, 2, 0] : (!SI) -> !SI
    %result = reussir.array.from_ifrt %out : !SI -> !SR
    reussir.array.to_host %result into %destination : !SR, memref<1x1xf32>
    reussir.rc.dec(%input : !SR)
    reussir.rc.dec(%result : !SR)
    return
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<3x5xf32> {mhlo.sharding = "{devices=[2,2]0,2,1,3}"}) -> (tensor<3x5xf32> {mhlo.sharding = "{devices=[2,2]0,2,1,3}"}) {
      %out = stablehlo.reverse %x, dims = [0, 1] : tensor<3x5xf32>
      return %out : tensor<3x5xf32>
    }
  }
  module @tiny attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<1x1xf32> {mhlo.sharding = "{devices=[2,2]0,2,1,3}"}) -> (tensor<1x1xf32> {mhlo.sharding = "{devices=[2,2]0,2,1,3}"}) {
      %three = stablehlo.constant dense<3.0> : tensor<1x1xf32>
      %out = stablehlo.add %x, %three : tensor<1x1xf32>
      return %out : tensor<1x1xf32>
    }
  }
}
