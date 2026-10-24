// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_transfer_sharded.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

// Nontrivial mesh permutation AND physical device order. Both global extents
// are dynamic, and each column shard occupies noncontiguous rows on the host.
!D = !reussir.rc<!reussir.array<? x ? x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #ifrt.sharding_param<2x2 to [1,0] on 2x2>>>>
!R = !reussir.rc<!reussir.array<? x ? x f32, #reussir.target<devices = [3, 1, 2, 0], sharding = #ifrt.sharding_param<2x1 to [1,0] on 2x2>>>>
!V = memref<?x?xf32, strided<[?, ?], offset: ?>>
func.func @tiled(%source: !V, %destination: !V) attributes {llvm.emit_c_interface} {
  %d = reussir.array.to_device %source : !V -> !D
  reussir.array.to_host %d into %destination : !D, !V
  reussir.rc.dec(%d : !D)
  return
}
func.func @replicated(%source: !V, %destination: !V) attributes {llvm.emit_c_interface} {
  %d = reussir.array.to_device %source : !V -> !R
  reussir.array.to_host %d into %destination : !R, !V
  reussir.rc.dec(%d : !R)
  return
}
