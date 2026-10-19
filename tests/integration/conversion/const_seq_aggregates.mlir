// RUN: %reussir-opt %s --reussir-convert-to-std --convert-to-llvm -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.mlir -o - | %FileCheck %s --check-prefix=LLVM

// An aggregate constant sequence lowers to a literal struct with one field
// per element, built in the global's initializer. Each element is laid out
// like the converted element type — members at their offsets, explicit
// padding between and after — so a `get` through that type reads it back.

!tile = !reussir.record<compound "Tile" [value] {i1, !reussir.const_seq<i64>}>
!auto = !reussir.record<compound "Layout.Auto" [value] {}>
!tiled = !reussir.record<compound "Layout.Tiled" [value] {!tile}>
!layout = !reussir.record<variant "Layout" [value] {!auto, !tiled}>

module {
  // CHECK: llvm.mlir.global internal constant @dims(dense<[8, 128]> : tensor<2xi64>) {addr_space = 0 : i32} : !llvm.array<2 x i64>
  reussir.const_seq.global @dims = dense<[8, 128]> : tensor<2xi64>

  // Nested sequences are views of their globals: a global declared later
  // in the module is addressed by symbol, and the length is the element
  // count of either form.
  // CHECK: llvm.mlir.global internal constant @rows() {addr_space = 0 : i32, alignment = 8 : i64} : !llvm.struct<(struct<(ptr, i64)>, struct<(ptr, i64)>)> {
  // CHECK:   %[[DIMS:.*]] = llvm.mlir.addressof @dims : !llvm.ptr
  // CHECK:   %[[TWO:.*]] = llvm.mlir.constant(2 : i64) : i64
  // CHECK:   %[[ZERO:.*]] = llvm.mlir.zero : !llvm.struct<(ptr, i64)>
  // CHECK:   %[[V1:.*]] = llvm.insertvalue %[[DIMS]], %[[ZERO]][0]
  // CHECK:   %[[ROW:.*]] = llvm.insertvalue %[[TWO]], %[[V1]][1]
  // CHECK:   llvm.mlir.addressof @none : !llvm.ptr
  // CHECK:   llvm.mlir.constant(0 : i64) : i64
  // CHECK:   %[[ROWS:.*]] = llvm.mlir.zero : !llvm.struct<(struct<(ptr, i64)>, struct<(ptr, i64)>)>
  // CHECK:   %[[R1:.*]] = llvm.insertvalue %[[ROW]], %[[ROWS]][0]
  // CHECK:   %[[R2:.*]] = llvm.insertvalue %{{.*}}, %[[R1]][1]
  // CHECK:   llvm.return %[[R2]]
  reussir.const_seq.global @rows : !reussir.const_seq<!reussir.const_seq<i64>> = [@dims, @none]
  reussir.const_seq.global @none = dense<> : tensor<0xi64>

  // A compound: the members in packed order (the view at 0, the i1 at 16),
  // then padding to the record's 24 bytes.
  // CHECK: llvm.mlir.global internal constant @tiles() {addr_space = 0 : i32, alignment = 8 : i64} : !llvm.struct<(struct<(struct<(ptr, i64)>, i1, array<7 x i8>)>)> {
  // CHECK:   %[[TRUE:.*]] = llvm.mlir.constant(true) : i1
  // CHECK:   %[[TILE0:.*]] = llvm.mlir.zero : !llvm.struct<(struct<(ptr, i64)>, i1, array<7 x i8>)>
  // CHECK:   %[[TILE1:.*]] = llvm.insertvalue %{{.*}}, %[[TILE0]][0]
  // CHECK:   llvm.insertvalue %[[TRUE]], %[[TILE1]][1]
  reussir.const_seq.global @tiles : !reussir.const_seq<!tile> = [[true, @dims]]

  // A variant: the i8 tag at 0, the arm at the arms' alignment (8), the
  // whole padded to 32 bytes; an empty arm takes no field.
  // CHECK: llvm.mlir.global internal constant @layouts() {addr_space = 0 : i32, alignment = 8 : i64} : !llvm.struct<(struct<(i8, array<31 x i8>)>, struct<(i8, array<7 x i8>, struct<(struct<(struct<(ptr, i64)>, i1, array<7 x i8>)>)>)>)> {
  // CHECK:   %[[TAG0:.*]] = llvm.mlir.constant(0 : i8) : i8
  // CHECK:   %[[AUTO:.*]] = llvm.mlir.zero : !llvm.struct<(i8, array<31 x i8>)>
  // CHECK:   llvm.insertvalue %[[TAG0]], %[[AUTO]][0]
  // CHECK:   %[[TAG1:.*]] = llvm.mlir.constant(1 : i8) : i8
  // CHECK:   %[[TILED:.*]] = llvm.mlir.zero : !llvm.struct<(i8, array<7 x i8>, struct<(struct<(struct<(ptr, i64)>, i1, array<7 x i8>)>)>)>
  // CHECK:   %[[TILED1:.*]] = llvm.insertvalue %[[TAG1]], %[[TILED]][0]
  // CHECK:   llvm.insertvalue %{{.*}}, %[[TILED1]][2]
  reussir.const_seq.global @layouts : !reussir.const_seq<!layout> = [[0, []], [1, [[false, @dims]]]]

  // A read goes through the converted element type.
  // CHECK-LABEL: @read
  // CHECK: llvm.getelementptr %{{.*}}[%{{.*}}] : (!llvm.ptr, i64) -> !llvm.ptr, !llvm.struct<"Tile", (struct<(ptr, i64)>, i1, array<7 x i8>)>
  // CHECK: llvm.load %{{.*}} : !llvm.ptr -> !llvm.struct<"Tile", (struct<(ptr, i64)>, i1, array<7 x i8>)>
  func.func @read(%i: index) -> !tile {
    %tiles = reussir.const_seq.literal @tiles : !reussir.const_seq<!tile>
    %tile = reussir.const_seq.get unsafe (%tiles : !reussir.const_seq<!tile>) [%i] : !tile
    return %tile : !tile
  }
}

// The initializers fold to LLVM constants, padding zero-filled.
// LLVM-DAG: @rows = internal constant { { ptr, i64 }, { ptr, i64 } } { { ptr, i64 } { ptr @dims, i64 2 }, { ptr, i64 } { ptr @none, i64 0 } }, align 8
// LLVM-DAG: @tiles = internal constant { { { ptr, i64 }, i1, [7 x i8] } } { { { ptr, i64 }, i1, [7 x i8] } { { ptr, i64 } { ptr @dims, i64 2 }, i1 true, [7 x i8] zeroinitializer } }, align 8
// LLVM-DAG: @layouts = internal constant { { i8, [31 x i8] }, { i8, [7 x i8], { { { ptr, i64 }, i1, [7 x i8] } } } } { { i8, [31 x i8] } zeroinitializer, { i8, [7 x i8], { { { ptr, i64 }, i1, [7 x i8] } } } { i8 1, [7 x i8] zeroinitializer, { { { ptr, i64 }, i1, [7 x i8] } } { { { ptr, i64 }, i1, [7 x i8] } { { ptr, i64 } { ptr @dims, i64 2 }, i1 false, [7 x i8] zeroinitializer } } } }, align 8
// LLVM: getelementptr %Tile, ptr @tiles, i64 %{{.*}}
