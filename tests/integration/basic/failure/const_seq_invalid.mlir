// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func @not_found() -> !reussir.const_seq<i64> {
  // expected-error @+1 {{referenced symbol is not a reussir.const_seq.global: @missing}}
  %seq = reussir.const_seq.literal @missing : !reussir.const_seq<i64>
  return %seq : !reussir.const_seq<i64>
}

// -----

reussir.const_seq.global @ints = dense<[1, 2]> : tensor<2xi64>
func.func @element_mismatch() -> !reussir.const_seq<i32> {
  // expected-error @+1 {{type does not match the global's '!reussir.const_seq<i64>'}}
  %seq = reussir.const_seq.literal @ints : !reussir.const_seq<i32>
  return %seq : !reussir.const_seq<i32>
}

// -----

// expected-error @+1 {{dense payload must have rank 1}}
reussir.const_seq.global @matrix = dense<[[1, 2], [3, 4]]> : tensor<2x2xi64>

// -----

// expected-error @+1 {{const_seq element type must be an integer, a floating-point type, a const_seq, or a value record, got 'index'}}
func.func private @bad_element(!reussir.const_seq<index>)

// -----

!shared = !reussir.record<compound "Shared" [shared] {i64}>
// expected-error @+1 {{const_seq element type must be an integer, a floating-point type, a const_seq, or a value record, got '!reussir.record<compound "Shared" {i64}>'}}
func.func private @shared_element(!reussir.const_seq<!shared>)

// -----

// expected-error @+1 {{an array payload requires the sequence type}}
reussir.const_seq.global @untyped = [@ints]

// -----

// expected-error @+1 {{dense payload element type 'i64' does not match '!reussir.const_seq<i32>'}}
reussir.const_seq.global @mismatch : !reussir.const_seq<i32> = dense<[1]> : tensor<1xi64>

// -----

reussir.const_seq.global @ints = dense<[1, 2]> : tensor<2xi64>
// expected-error @+1 {{nested sequence @ints has type '!reussir.const_seq<i64>', expected '!reussir.const_seq<i32>'}}
reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i32>> = [@ints]

// -----

// expected-error @+1 {{referenced symbol is not a reussir.const_seq.global: @missing}}
reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i64>> = [@missing]

// -----

// expected-error @+1 {{expected the symbol of a '!reussir.const_seq<i64>' global, got 1 : i64}}
reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i64>> = [1 : i64]

// -----

!tile = !reussir.record<compound "Tile" [value] {i64, i1}>
// expected-error @+1 {{expected 2 member value(s) for '!reussir.record<compound "Tile" [value] {i64, i1}>', got 1}}
reussir.const_seq.global @tiles : !reussir.const_seq<!tile> = [[1 : i64]]

// -----

!tile = !reussir.record<compound "Tile" [value] {i64, i1}>
// expected-error @+1 {{expected a 'i1' constant, got 1 : i64}}
reussir.const_seq.global @tiles : !reussir.const_seq<!tile> = [[1 : i64, 1 : i64]]

// -----

!auto = !reussir.record<compound "Layout.Auto" [value] {}>
!layout = !reussir.record<variant "Layout" [value] {!auto}>
// expected-error @+1 {{variant tag 1 : i64 is out of 1 arms}}
reussir.const_seq.global @layouts : !reussir.const_seq<!layout> = [[1, []]]

// -----

!auto = !reussir.record<compound "Layout.Auto" [value] {}>
!layout = !reussir.record<variant "Layout" [value] {!auto}>
// expected-error @+1 {{expected a variant value as [tag, arm], got [0]}}
reussir.const_seq.global @layouts : !reussir.const_seq<!layout> = [[0]]

// -----

reussir.const_seq.global @ints = dense<[1, 2]> : tensor<2xi64>
func.func @type_mismatch() -> !reussir.const_seq<!reussir.const_seq<i64>> {
  // expected-error @+1 {{type does not match the global's '!reussir.const_seq<i64>'}}
  %seq = reussir.const_seq.literal @ints : !reussir.const_seq<!reussir.const_seq<i64>>
  return %seq : !reussir.const_seq<!reussir.const_seq<i64>>
}
