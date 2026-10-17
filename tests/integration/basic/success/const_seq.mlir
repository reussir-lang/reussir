// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

module @test {
  // CHECK: reussir.const_seq.global @ints = dense<[0, -1, 7]> : tensor<3xi64>
  reussir.const_seq.global @ints = dense<[0, -1, 7]> : tensor<3xi64>
  // CHECK: reussir.const_seq.global @floats = dense<[1.500000e+00, -2.000000e+00]> : tensor<2xf32>
  reussir.const_seq.global @floats = dense<[1.5, -2.0]> : tensor<2xf32>
  // CHECK: reussir.const_seq.global @empty = dense<> : tensor<0xi1>
  reussir.const_seq.global @empty = dense<> : tensor<0xi1>

  // CHECK-LABEL: func.func @read
  func.func @read(%i: index) -> (index, i64, f32, index) {
    // CHECK: reussir.const_seq.literal @ints : <i64>
    %ints = reussir.const_seq.literal @ints : !reussir.const_seq<i64>
    // CHECK: reussir.const_seq.len
    %len = reussir.const_seq.len (%ints : !reussir.const_seq<i64>) : index
    // CHECK: reussir.const_seq.get
    %x = reussir.const_seq.get (%ints : !reussir.const_seq<i64>) [%i] : i64
    %floats = reussir.const_seq.literal @floats : !reussir.const_seq<f32>
    // CHECK: reussir.const_seq.unsafe_get
    %y = reussir.const_seq.unsafe_get (%floats : !reussir.const_seq<f32>) [%i] : f32
    %empty = reussir.const_seq.literal @empty : !reussir.const_seq<i1>
    %zero = reussir.const_seq.len (%empty : !reussir.const_seq<i1>) : index
    return %len, %x, %y, %zero : index, i64, f32, index
  }
}
