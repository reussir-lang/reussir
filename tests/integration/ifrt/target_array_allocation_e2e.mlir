// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/target_array_allocation_e2e.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %not --crash %t.exe bad

// Two shards, replicated across four devices in a nontrivial mesh order.
#target = #reussir.target<devices = [3, 1, 2, 0], sharding = #ifrt.sharding_param<2x1 to [1, 0] on 2x2>, memory_kind = "device", layout = "{0,1:T(8,128)(2,1)}">
!array = !reussir.rc<!reussir.array<? x 4 x f32, #target>>
func.func @create(%n: index) -> !array {
  %a = reussir.array.create extents(%n) : !array
  return %a : !array
}
func.func @retain(%a: !array) {
  reussir.rc.inc(%a : !array)
  return
}
func.func @release(%a: !array) {
  reussir.rc.dec(%a : !array)
  return
}
