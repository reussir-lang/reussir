// REQUIRES: openxla, linux, pjrt-runtime
// RUN: %reussir-opt %S/array_transfer_sharded.mlir --reussir-attach-native-target --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc -DREAL_PJRT %t.o %S/array_transfer_sharded.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env XLA_FLAGS="--xla_force_host_platform_device_count=4" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe
