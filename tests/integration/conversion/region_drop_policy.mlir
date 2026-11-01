// RUN: %reussir-opt %s --reussir-lowering-region-patterns | %FileCheck %s
// RUN: %reussir-opt %s --reussir-lowering-region-patterns="drop-order=backward" | %FileCheck %s
// RUN: not %reussir-opt %s --reussir-lowering-region-patterns="drop-order=forward" 2>&1 | %FileCheck %s --check-prefix=CONFLICT
// RUN: not %reussir-opt %s --reussir-lowering-region-patterns="drop-order=invalid" 2>&1 | %FileCheck %s --check-prefix=INVALID

// CHECK: module attributes {reussir.drop_order = "backward"}
// CONFLICT: drop-order conflicts with saved module policy
// INVALID: drop-order must be forward or backward
module attributes {reussir.drop_order = "backward"} {}
