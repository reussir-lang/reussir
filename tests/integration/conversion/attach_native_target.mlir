// RUN: %reussir-opt %s -reussir-attach-native-target | %FileCheck %s

module {
  // CHECK: module attributes {
  // CHECK-DAG: dlti.dl_spec = #dlti.dl_spec<
  // CHECK-DAG: llvm.data_layout = "
  // CHECK-DAG: llvm.target_triple = "
}
