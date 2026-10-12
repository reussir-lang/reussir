//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <iterator>
#include <mlir/IR/Verifier.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, GlueSymbolCollectionTracksInsertionsAndAtomicKinds) {
  withType<RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "Glue" [value] {
          !reussir.record<compound "Inner" [shared] {i64}>}>)",
      [&](mlir::ModuleOp module, RecordType type) {
        mlir::OpBuilder builder(context.get());
        // Exercise a pre-existing symbol as well as symbols created after
        // the collection has cached the module.
        auto existing = createDtorIfNotExists(module, type, builder);
        mlir::SymbolTableCollection symbols;
        auto &table = symbols.getSymbolTable(module);
        for (AtomicKind kind : {AtomicKind::normal, AtomicKind::atomic}) {
          auto dtor = createDtorIfNotExists(module, type, builder, kind, &symbols);
          auto acquire = emitOwnershipAcquisitionFuncIfNotExists(
              module, type, builder, kind, &symbols);
          EXPECT_EQ(table.lookup(type.getDtorName(kind).getValue()),
                    dtor.getOperation());
          EXPECT_EQ(table.lookup(type.getAcquireName(kind).getValue()),
                    acquire.getOperation());
          for (unsigned i = 0; i < 8; ++i) {
            EXPECT_EQ(createDtorIfNotExists(module, type, builder, kind,
                                           &symbols), dtor);
            EXPECT_EQ(emitOwnershipAcquisitionFuncIfNotExists(
                          module, type, builder, kind, &symbols), acquire);
          }
          // The legacy uncached entry points must find the same functions.
          EXPECT_EQ(createDtorIfNotExists(module, type, builder, kind), dtor);
          EXPECT_EQ(emitOwnershipAcquisitionFuncIfNotExists(
                        module, type, builder, kind), acquire);
          if (kind == AtomicKind::normal)
            EXPECT_EQ(existing, dtor);
        }
        EXPECT_EQ(std::distance(module.getOps<mlir::func::FuncOp>().begin(),
                                module.getOps<mlir::func::FuncOp>().end()), 5);
        EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
      });
}

TEST_F(ReussirTest, GlueSymbolCollectionSeparatesModules) {
  withType<RecordType>(
      SIMPLE_LAYOUT, R"(!reussir.record<compound "Local" [value] {i64}>)",
      [&](mlir::ModuleOp first, RecordType type) {
        mlir::OpBuilder builder(context.get());
        mlir::OwningOpRef<mlir::ModuleOp> second =
            mlir::ModuleOp::create(builder.getUnknownLoc());
        mlir::SymbolTableCollection symbols;
        auto a = emitOwnershipAcquisitionFuncIfNotExists(
            first, type, builder, AtomicKind::normal, &symbols);
        auto b = emitOwnershipAcquisitionFuncIfNotExists(
            *second, type, builder, AtomicKind::normal, &symbols);
        EXPECT_NE(a, b);
        EXPECT_EQ(a.getSymName(), b.getSymName());
        EXPECT_EQ(a->getParentOp(), first.getOperation());
        EXPECT_EQ(b->getParentOp(), second->getOperation());
        EXPECT_EQ(emitOwnershipAcquisitionFuncIfNotExists(
                      first, type, builder, AtomicKind::normal, &symbols), a);
        EXPECT_TRUE(mlir::succeeded(mlir::verify(first)));
        EXPECT_TRUE(mlir::succeeded(mlir::verify(*second)));
      });
}
} // namespace reussir
