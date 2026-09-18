#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Interfaces/SideEffectInterfaces.h>

import reussir.test;

namespace reussir {

// Speculation and memory effects answer separate questions. In particular,
// projecting an array is effect-free, but its in-bounds assumption prevents
// unconditional speculation.
TEST_F(ReussirTest, SpeculationAndMemoryEffects) {
  withModule(R"mlir(
    func.func @test(%array: !reussir.ref<!reussir.array<4 x i32>>,
                   %dynamic: !reussir.ref<!reussir.array<? x i32>>,
                   %index: index,
                   %rc: !reussir.rc<i32 shared atomic>) {
      %c0 = arith.constant 0 : index
      %c4 = arith.constant 4 : index
      %mem = reussir.array.view(%array : !reussir.ref<!reussir.array<4 x i32>>) : memref<4xi32> {test.speculate = true, test.effect_free = true}
      %tensor = reussir.array.view(%array : !reussir.ref<!reussir.array<4 x i32>>) : tensor<4xi32> {test.speculate = false, test.effect_free = false}
      %dyn = reussir.array.view(%dynamic : !reussir.ref<!reussir.array<? x i32>>) : memref<?xi32, strided<[?], offset: ?>> {test.speculate = false, test.effect_free = false}
      %good = reussir.array.project(%mem : memref<4xi32>)[%c0 : index] : !reussir.ref<i32 field> {test.speculate = false, test.effect_free = true}
      %bad = reussir.array.project(%mem : memref<4xi32>)[%c4 : index] : !reussir.ref<i32 field> {test.speculate = false, test.effect_free = true}
      %unknown = reussir.array.project(%mem : memref<4xi32>)[%index : index] : !reussir.ref<i32 field> {test.speculate = false, test.effect_free = true}
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
               EXPECT_EQ(checked, 8u);
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

} // namespace reussir
