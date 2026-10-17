// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func @not_found() -> !reussir.const_seq<i64> {
  // expected-error @+1 {{referenced symbol is not a reussir.const_seq.global: @missing}}
  %seq = reussir.const_seq.literal @missing : !reussir.const_seq<i64>
  return %seq : !reussir.const_seq<i64>
}

// -----

reussir.const_seq.global @ints = dense<[1, 2]> : tensor<2xi64>
func.func @element_mismatch() -> !reussir.const_seq<i32> {
  // expected-error @+1 {{element type does not match the global payload: 'i64'}}
  %seq = reussir.const_seq.literal @ints : !reussir.const_seq<i32>
  return %seq : !reussir.const_seq<i32>
}

// -----

// expected-error @+1 {{payload must be a rank-1 dense elements attribute}}
reussir.const_seq.global @matrix = dense<[[1, 2], [3, 4]]> : tensor<2x2xi64>

// -----

// expected-error @+1 {{const_seq element type must be an integer or floating-point type, got 'index'}}
func.func private @bad_element(!reussir.const_seq<index>)
