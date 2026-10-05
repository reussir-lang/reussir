// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_bridge_borrow_e2e.c -o %t.exe %extra_sys_libs
// RUN: %t.exe
// RUN: %not --crash %t.exe bad

#s = #ifrt.sharding_param<2x1x1 to [0] on 2>
!I = !ifrt.array<tensor<?x4x?xf32>, #s, [3, 1]>
!R = !reussir.rc<!reussir.array<? x 4 x ? x f32, #reussir.target<devices = [3, 1], sharding = #s>> atomic>
func.func @borrow(%input: !R) {
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
