// REQUIRES: linux, pjrt-runtime
// RUN: %reussir-opt %S/array_transfer_e2e.mlir --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -DREAL_PJRT %S/array_transfer_e2e.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe
