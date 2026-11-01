// RUN: %reussir-opt %s | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc | %FileCheck %s
// CHECK: #reussir.worklist_role<walk, forward>
// CHECK: #reussir.worklist_role<release, backward>
func.func private @walk() attributes {
  reussir.worklist_role = #reussir.worklist_role<walk, forward>
} {
  return
}
func.func private @release() attributes {
  reussir.worklist_role = #reussir.worklist_role<release, backward>
} {
  return
}
