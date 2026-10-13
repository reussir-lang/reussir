// RUN: %reussir-opt %s --reussir-prepare-pjrt --reussir-prepare-pjrt | %FileCheck %s --check-prefix=MLIR
// RUN: %rrc %s -O none --emit llvm-ir -o - | %FileCheck %s --check-prefix=LLVM
// RUN: %rrc %s -O aggressive --emit llvm-ir -o - | %FileCheck %s --check-prefix=LLVM

// The preparation pass is a no-op for ordinary modules in either build mode.
// MLIR-LABEL: func.func @identity
// MLIR-SAME: test.arg
// MLIR-SAME: test.function
// MLIR: return
// LLVM-LABEL: define {{.*}}@identity(
// LLVM: ret i32
func.func @identity(%value: i32 {test.arg}) -> i32 attributes {test.function} {
  return %value : i32
}
