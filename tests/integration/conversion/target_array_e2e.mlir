// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-closure-outlining,reussir-lowering-region-patterns,func.func(reussir-inc-dec-cancellation),reussir-rc-decrement-expansion,func.func(reussir-infer-variant-tag),reussir-acquire-drop-expansion,reussir-convert-to-std,func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,func.func(reussir-rc-create-sink),func.func(reussir-rc-create-fusion),reussir-trmc-recursion-analysis,reussir-compile-polymorphic-ffi,canonicalize,cse,control-flow-sink,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/target_array_e2e.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

#device = #reussir.target<devices = [0]>
!array = !reussir.array<? x 4 x f32, #device>
!rc = !reussir.rc<!array>
!fixed = !reussir.rc<!reussir.array<2 x 4 x f32, #device> atomic>
!outer = !reussir.rc<!reussir.array<2 x !rc>>

func.func @create(%n: index) -> !rc {
  %a = reussir.array.create extents(%n) : !rc
  return %a : !rc
}
func.func @retain(%a: !rc) {
  reussir.rc.inc(%a : !rc)
  return
}
func.func @release(%a: !rc) {
  reussir.rc.dec(%a : !rc)
  return
}
func.func @create_atomic() -> !fixed {
  %a = reussir.array.create extents() : !fixed
  return %a : !fixed
}
func.func @retain_atomic(%a: !fixed) {
  reussir.rc.inc(%a : !fixed)
  return
}
func.func @release_atomic(%a: !fixed) {
  reussir.rc.dec(%a : !fixed)
  return
}
func.func @nested(%a: !rc) {
  %outer = reussir.array.create extents() : !outer body {
  ^bb0(%i: index):
    reussir.rc.inc(%a : !rc)
    reussir.scf.yield %a : !rc
  }
  reussir.rc.dec(%outer : !outer)
  return
}
