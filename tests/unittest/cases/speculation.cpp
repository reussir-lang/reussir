#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Interfaces/SideEffectInterfaces.h>

import reussir.test;

namespace reussir {

// Speculation and memory effects answer separate questions. In particular,
// unchecked immutable string reads are effect-free, but still need bounds
// proofs before they can be speculated.
TEST_F(ReussirTest, SpeculationAndMemoryEffects) {
  withModule(R"mlir(
    func.func @test(%array: !reussir.ref<!reussir.array<4 x i32>>,
                   %dynamic: !reussir.ref<!reussir.array<? x i32>>,
                   %index: index, %str: !reussir.str<local>,
                   %rc: !reussir.rc<i32 shared atomic>) {
      %c0 = arith.constant 0 : index
      %c4 = arith.constant 4 : index
      %mem = reussir.array.view(%array : !reussir.ref<!reussir.array<4 x i32>>) : memref<4xi32> {test.speculate = true, test.effect_free = true}
      %tensor = reussir.array.view(%array : !reussir.ref<!reussir.array<4 x i32>>) : tensor<4xi32> {test.speculate = false, test.effect_free = false}
      %dyn = reussir.array.view(%dynamic : !reussir.ref<!reussir.array<? x i32>>) : memref<?xi32, strided<[?], offset: ?>> {test.speculate = false, test.effect_free = false}
      %good = reussir.array.project(%mem : memref<4xi32>)[%c0 : index] : !reussir.ref<i32 field> {test.speculate = true, test.effect_free = true}
      %bad = reussir.array.project(%mem : memref<4xi32>)[%c4 : index] : !reussir.ref<i32 field> {test.speculate = false, test.effect_free = false}
      %unknown = reussir.array.project(%mem : memref<4xi32>)[%index : index] : !reussir.ref<i32 field> {test.speculate = false, test.effect_free = false}
      %byte = reussir.str.byte_at(%str : !reussir.str<local>)[%index : index] : i8 {test.speculate = true, test.effect_free = true}
      %unchecked = reussir.str.unsafe_byte_at(%str : !reussir.str<local>)[%index : index] : i8 {test.speculate = false, test.effect_free = true}
      %prefix = reussir.str.startswith(%str : !reussir.str<local>) "hi" : i1 {test.speculate = true, test.effect_free = true}
      %unsafe_prefix = reussir.str.unsafe_startswith(%str : !reussir.str<local>) "hi" : i1 {test.speculate = false, test.effect_free = true}
      %selected, %found = reussir.str.select(%str) ["hi"] : (!reussir.str<local>) -> (index, i1) {test.speculate = true, test.effect_free = true}
      %equal = reussir.str.equal(%str : !reussir.str<local>, %str : !reussir.str<local>) : i1 {test.speculate = true, test.effect_free = true}
      %cmp = reussir.str.compare(%str : !reussir.str<local>, %str : !reussir.str<local>) : i32 {test.speculate = true, test.effect_free = true}
      %count = reussir.rc.fetch(%rc : !reussir.rc<i32 shared atomic>) : index {test.speculate = false, test.effect_free = false}
      %unique = reussir.rc.is_unique(%rc : !reussir.rc<i32 shared atomic>) : i1 {test.speculate = false, test.effect_free = false}
      return
    }
  )mlir",
             [](mlir::ModuleOp module) {
               unsigned checked = 0;
               module.walk([&](mlir::Operation *op) {
                 if (auto expected =
                         op->getAttrOfType<mlir::BoolAttr>("test.speculate")) {
                   EXPECT_EQ(mlir::isSpeculatable(op), expected.getValue())
                       << op->getName().getStringRef().str();
                   EXPECT_EQ(
                       mlir::isMemoryEffectFree(op),
                       op->getAttrOfType<mlir::BoolAttr>("test.effect_free")
                           .getValue())
                       << op->getName().getStringRef().str();
                   ++checked;
                 }
               });
               EXPECT_EQ(checked, 15u);
             });
}

TEST_F(ReussirTest, NullableDispatchSpeculationIncludesNestedOperations) {
  withModule(
      R"mlir(
    func.func @test(%nullable: !reussir.nullable<!reussir.ref<i32>>) {
      reussir.nullable.dispatch(%nullable : !reussir.nullable<!reussir.ref<i32>>) {
        nonnull -> {
        ^bb0(%ref: !reussir.ref<i32>):
          reussir.scf.yield
        }
        null -> { reussir.scf.yield }
      } {test.speculate = true}
      reussir.nullable.dispatch(%nullable : !reussir.nullable<!reussir.ref<i32>>) {
        nonnull -> {
        ^bb0(%ref: !reussir.ref<i32>):
          %value = reussir.ref.load(%ref : !reussir.ref<i32>) : i32
          reussir.scf.yield
        }
        null -> { reussir.scf.yield }
      } {test.speculate = false}
      return
    }
  )mlir",
      [](mlir::ModuleOp module) {
        unsigned checked = 0;
        module.walk([&](ReussirNullableDispatchOp op) {
          EXPECT_EQ(
              mlir::isSpeculatable(op),
              op->getAttrOfType<mlir::BoolAttr>("test.speculate").getValue());
          ++checked;
        });
        EXPECT_EQ(checked, 2u);
      });
}

TEST_F(ReussirTest, ProvenStringBoundsPermitSpeculation) {
  withModule(R"mlir(
    reussir.str.global @text = "hello"
    func.func @test(%unknown: !reussir.str<local>) {
      %c0 = arith.constant 0 : index
      %c2 = arith.constant 2 : index
      %c3 = arith.constant 3 : index
      %c5 = arith.constant 5 : index
      %global = reussir.str.literal @text : !reussir.str<global>
      %local = reussir.str.cast(%global : !reussir.str<global>) : !reussir.str<local>
      %tail = reussir.str.slice(%local : !reussir.str<local>)[%c2] : !reussir.str<local>
      %last = reussir.str.unsafe_byte_at(%tail : !reussir.str<local>)[%c2 : index] : i8 {test.speculate = true}
      %past = reussir.str.unsafe_byte_at(%tail : !reussir.str<local>)[%c3 : index] : i8 {test.speculate = false}
      %prefix = reussir.str.unsafe_startswith(%tail : !reussir.str<local>) "llo" : i1 {test.speculate = true}
      %long = reussir.str.unsafe_startswith(%tail : !reussir.str<local>) "hello" : i1 {test.speculate = false}
      %empty = reussir.str.unsafe_startswith(%unknown : !reussir.str<local>) "" : i1 {test.speculate = true}
      %cmp = reussir.str.unsafe_memcmp(%local : !reussir.str<local>, %tail : !reussir.str<local>)[%c3] : i32 {test.speculate = true}
      %overread = reussir.str.unsafe_memcmp(%local : !reussir.str<local>, %tail : !reussir.str<local>)[%c5] : i32 {test.speculate = false}
      %no_read = reussir.str.unsafe_memcmp(%unknown : !reussir.str<local>, %unknown : !reussir.str<local>)[%c0] : i32 {test.speculate = true}
      return
    }
  )mlir",
             [](mlir::ModuleOp module) {
               unsigned checked = 0;
               module.walk([&](mlir::Operation *op) {
                 if (auto expected =
                         op->getAttrOfType<mlir::BoolAttr>("test.speculate")) {
                   EXPECT_EQ(mlir::isSpeculatable(op), expected.getValue())
                       << op->getName().getStringRef().str();
                   ++checked;
                 }
               });
               EXPECT_EQ(checked, 8u);
             });
}

} // namespace reussir
