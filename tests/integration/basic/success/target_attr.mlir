// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t
// RUN: %reussir-opt %t | %FileCheck %s

// Optional fields stay absent; logical device order is preserved.
// CHECK: reussir.default_target = #reussir.target<devices = [0]>
// CHECK: reussir.ordered_target = #reussir.target<devices = [3, 1, 2, 0]>
module attributes {
  reussir.default_target = #reussir.target<devices = [0]>,
  reussir.ordered_target = #reussir.target<devices = [3, 1, 2, 0]>
} {}

// Memory kinds and layouts are strings, including backend-specific spellings.
// CHECK: reussir.auto_layout = #reussir.target<devices = [0], layout = "auto">
// CHECK: reussir.default_layout = #reussir.target<devices = [0], layout = "default">
// CHECK: reussir.device_memory = #reussir.target<devices = [0], memory_kind = "device">
// CHECK: reussir.explicit_layout = #reussir.target<devices = [0], layout = "{1,0:T(8,128)}">
// CHECK: reussir.host_memory = #reussir.target<devices = [0], memory_kind = "pinned_host", layout = "{0}">
module attributes {
  reussir.auto_layout = #reussir.target<devices = [0], layout = "auto">,
  reussir.default_layout = #reussir.target<devices = [0], layout = "default">,
  reussir.device_memory = #reussir.target<devices = [0], memory_kind = "device">,
  reussir.explicit_layout = #reussir.target<devices = [0], layout = "{1,0:T(8,128)}">,
  reussir.host_memory = #reussir.target<layout = "{0}", devices = [0], memory_kind = "pinned_host">
} {}
