// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

// A constant sequence of aggregates: nested sequences reference the global
// holding their elements, compound values list their members, and variant
// values pair a tag with the arm's compound value.

!tile = !reussir.record<compound "Tile" [value] {!reussir.const_seq<i64>, i1}>
!auto = !reussir.record<compound "Layout.Auto" [value] {}>
!tiled = !reussir.record<compound "Layout.Tiled" [value] {!tile}>
!layout = !reussir.record<variant "Layout" [value] {!auto, !tiled}>

module @test {
  // CHECK: reussir.const_seq.global @dims = dense<[8, 128]> : tensor<2xi64>
  reussir.const_seq.global @dims = dense<[8, 128]> : tensor<2xi64>
  // CHECK: reussir.const_seq.global @none = dense<> : tensor<0xi64>
  reussir.const_seq.global @none = dense<> : tensor<0xi64>
  // A scalar sequence may also name its type.
  // CHECK: reussir.const_seq.global @typed : !reussir.const_seq<i64> = dense<7> : tensor<1xi64>
  reussir.const_seq.global @typed : !reussir.const_seq<i64> = dense<[7]> : tensor<1xi64>

  // CHECK: reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i64>> = [@dims, @none, @dims]
  reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i64>> = [@dims, @none, @dims]
  // CHECK: reussir.const_seq.global @empty_rows : !reussir.const_seq<!reussir.const_seq<i64>> = []
  reussir.const_seq.global @empty_rows : !reussir.const_seq<!reussir.const_seq<i64>> = []

  // CHECK: reussir.const_seq.global @tiles : !reussir.const_seq<!reussir.record<compound "Tile" [value] {!reussir.const_seq<i64>, i1}>> = {{\[}}[@dims, true], [@none, false]]
  reussir.const_seq.global @tiles : !reussir.const_seq<!tile> = [[@dims, true], [@none, false]]

  // CHECK: reussir.const_seq.global @layouts : !reussir.const_seq<!reussir.record<variant "Layout" [value] {!reussir.record<compound "Layout.Auto" [value] {}>, !reussir.record<compound "Layout.Tiled" [value] {!reussir.record<compound "Tile" [value] {!reussir.const_seq<i64>, i1}>}>}>> = {{\[}}[0, []], [1, {{\[}}[@dims, true]]]]
  reussir.const_seq.global @layouts : !reussir.const_seq<!layout> = [[0, []], [1, [[@dims, true]]]]

  // CHECK-LABEL: func.func @read
  func.func @read(%i: index) -> (index, !reussir.const_seq<i64>, !tile, !layout) {
    // CHECK: reussir.const_seq.literal @rows : <!reussir.const_seq<i64>>
    %rows = reussir.const_seq.literal @rows : !reussir.const_seq<!reussir.const_seq<i64>>
    %len = reussir.const_seq.len (%rows : !reussir.const_seq<!reussir.const_seq<i64>>) : index
    // CHECK: reussir.const_seq.get(%{{.*}} : <!reussir.const_seq<i64>>) [%{{.*}}] : !reussir.const_seq<i64>
    %row = reussir.const_seq.get (%rows : !reussir.const_seq<!reussir.const_seq<i64>>) [%i] : !reussir.const_seq<i64>
    %tiles = reussir.const_seq.literal @tiles : !reussir.const_seq<!tile>
    // CHECK: reussir.const_seq.get(%{{.*}} : <!reussir.record<compound "Tile" [value] {!reussir.const_seq<i64>, i1}>>) [%{{.*}}] : !reussir.record<compound "Tile" [value] {!reussir.const_seq<i64>, i1}>
    %tile = reussir.const_seq.get (%tiles : !reussir.const_seq<!tile>) [%i] : !tile
    %layouts = reussir.const_seq.literal @layouts : !reussir.const_seq<!layout>
    %layout = reussir.const_seq.get (%layouts : !reussir.const_seq<!layout>) [%i] : !layout
    return %len, %row, %tile, %layout : index, !reussir.const_seq<i64>, !tile, !layout
  }
}
