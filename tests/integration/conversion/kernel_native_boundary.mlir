// RUN: %not %reussir-opt --reussir-prepare-pjrt %s 2>&1 | %FileCheck %s
// Even a kernel with no StableHLO instructions cannot become a host function.
// CHECK: kernel launching is not available; an unlaunched kernel cannot enter native host lowering
module {
  func.func @identity(%x: tensor<i32>) -> tensor<i32> attributes {reussir.kernel_function} {
    return %x : tensor<i32>
  }
}
