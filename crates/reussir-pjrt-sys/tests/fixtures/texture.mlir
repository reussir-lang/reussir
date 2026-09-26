// Handwritten checkerboard texture; no host inputs or custom runtime functions.
module @texture {
  func.func @main() -> tensor<8x8xf32> {
    %x = stablehlo.iota dim = 0 : tensor<8x8xi32>
    %y = stablehlo.iota dim = 1 : tensor<8x8xi32>
    %one = stablehlo.constant dense<1> : tensor<8x8xi32>
    %sum = stablehlo.add %x, %y : tensor<8x8xi32>
    %checker = stablehlo.and %sum, %one : tensor<8x8xi32>
    %pixels = stablehlo.convert %checker : (tensor<8x8xi32>) -> tensor<8x8xf32>
    return %pixels : tensor<8x8xf32>
  }
}
