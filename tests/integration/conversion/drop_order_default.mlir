// RUN: %reussir-opt %s --reussir-lowering-region-patterns | %FileCheck %s
// RUN: %reussir-opt %s --reussir-rc-decrement-expansion | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion="drop-order=forward" | %FileCheck %s --check-prefix=FORWARD

// CHECK: module attributes {reussir.drop_order = "backward"}
// FORWARD: module attributes {reussir.drop_order = "forward"}
module {}
