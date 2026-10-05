// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --reussir-convert-to-llvm | %FileCheck %s --implicit-check-not=ifrt.function --implicit-check-not=ifrt.donated

// Even a host with no kernel calls must be ready for function conversion.
// CHECK-LABEL: llvm.func @main
// CHECK-SAME: test.arg
// CHECK-SAME: test.function
// CHECK: llvm.return
module {
  func.func @main(%arg: i32 {ifrt.donated, test.arg}) attributes {ifrt.function, test.function} {
    return
  }
}
