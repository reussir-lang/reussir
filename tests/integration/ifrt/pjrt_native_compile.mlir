// REQUIRES: ifrt
// RUN: %rrc %S/pjrt_call_e2e.mlir -O none --emit llvm-ir -o %t.none.ll
// RUN: %FileCheck %s < %t.none.ll
// RUN: %rrc %S/pjrt_call_e2e.mlir -O aggressive --emit llvm-ir -o %t.opt.ll
// RUN: %FileCheck %s < %t.opt.ll

// Exercise native compiler dialect registration and kernel preparation even
// when no external PJRT runtime or plugin is configured. The shared source
// includes StableHLO arithmetic, repeated calls, multiple results, and control
// dependencies; its separate E2E test checks execution when a plugin is present.
// CHECK-LABEL: define {{.*}}@examples(
// CHECK: call {{.*}}@__reussir_pjrt_compile
// CHECK: call {{.*}}@__reussir_pjrt_executable_execute
// CHECK: call {{.*}}@__reussir_pjrt_array_to_host
