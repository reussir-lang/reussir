module {
  func.func @main(
      %lhs: tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"},
      %rhs: tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"})
      -> (tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"},
          tensor<8xf32> {mhlo.sharding = "{devices=[2]0,1}"}) {
    %sum = stablehlo.add %lhs, %rhs : tensor<8xf32>
    %difference = stablehlo.subtract %lhs, %rhs : tensor<8xf32>
    // Reversing the global array requires communication between the shards.
    %reverse = stablehlo.reverse %difference, dims = [0] : tensor<8xf32>
    return %sum, %reverse : tensor<8xf32>, tensor<8xf32>
  }
}
