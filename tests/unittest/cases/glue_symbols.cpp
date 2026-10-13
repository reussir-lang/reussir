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
// A shared aggregate DAG must produce one body per named value record, even
// when no caller supplies a symbol collection. This also checks that the normal
// and atomic graphs stay separate and that nested calls forward the delta.
TEST_F(ReussirTest, NestedAcquireGlueSharesBodiesWithoutExternalSymbolTable) {
  withType<RecordType>(
      SIMPLE_LAYOUT,
      R"(!reussir.record<compound "DagLeaf" [value] {
          !reussir.record<compound "DagPayload" [shared] {i64}>}>)",
      [&](mlir::ModuleOp module, RecordType leaf) {
        mlir::OpBuilder builder(context.get());
        llvm::SmallVector<RecordType> levels{leaf};
        for (unsigned i = 1; i <= 6; ++i) {
          auto next = RecordType::get(
              context.get(), builder.getStringAttr("Dag" + std::to_string(i)),
              RecordKind::compound);
          next.complete({levels.back(), levels.back()}, {false, false},
                        Capability::value, false);
          levels.push_back(next);
        }
        for (AtomicKind kind : {AtomicKind::normal, AtomicKind::atomic}) {
          auto root = emitOwnershipAcquisitionFuncIfNotExists(
              module, levels.back(), builder, kind);
          EXPECT_EQ(root, emitOwnershipAcquisitionFuncIfNotExists(
                              module, levels.back(), builder, kind));
          for (auto [i, type] : llvm::enumerate(levels)) {
            auto glue = module.lookupSymbol<mlir::func::FuncOp>(
                type.getAcquireName(kind).getValue());
            ASSERT_TRUE(glue);
            unsigned calls = 0, retains = 0;
            glue.walk([&](mlir::func::CallOp call) {
              ++calls;
              ASSERT_GT(i, 0u);
              EXPECT_EQ(call.getCallee(),
                        levels[i - 1].getAcquireName(kind).getValue());
              EXPECT_EQ(call.getOperand(1), glue.getArgument(1));
              EXPECT_EQ(llvm::cast<RefType>(call.getOperand(0).getType())
                            .getAtomicKind(),
                        kind);
            });
            glue.walk([&](ReussirRcIncOp inc) {
              ++retains;
              EXPECT_EQ(inc.getDelta(), glue.getArgument(1));
            });
            EXPECT_EQ(calls, i ? 2u : 0u);
            EXPECT_EQ(retains, i ? 0u : 1u);
          }
        }
        EXPECT_EQ(std::distance(module.getOps<mlir::func::FuncOp>().begin(),
                                module.getOps<mlir::func::FuncOp>().end()),
                  1 + 2 * levels.size());
        EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
      });
}
} // namespace reussir
