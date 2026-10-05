// REQUIRES: ifrt, linux, pjrt-runtime
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-token-instantiation --reussir-token-reuse --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/pjrt_sharded_e2e.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env XLA_FLAGS="--xla_force_host_platform_device_count=2" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" REUSSIR_PJRT_CACHE_CONFIG="%S/../runtime/Inputs/pjrt_cache.toml" %t.exe

#s = #ifrt.sharding_param<2 to [0] on 2>
!I = !ifrt.array<tensor<8xf32>, #s, [1, 0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [1, 0], sharding = #s>>>
module {
  func.func @sharded(%input: !R) -> !R attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [1, 0] : (!I) -> !I
    %result = reussir.array.from_ifrt %out : !I -> !R
    return %result : !R
  }
  func.func @release(%array: !R) {
    reussir.rc.dec(%array : !R)
    return
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"}) -> (tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"}) {
      %out = stablehlo.reverse %x, dims = [0] : tensor<8xf32>
      return %out : tensor<8xf32>
    }
  }
}
