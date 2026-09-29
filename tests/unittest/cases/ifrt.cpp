//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file tests IFRT bridge metadata tokens and memory effects.
///
//===----------------------------------------------------------------------===//

#ifdef REUSSIR_ENABLE_OPENXLA
#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <mlir/IR/Verifier.h>

import reussir.test;

void registerIfrtDialectsAndPasses(mlir::DialectRegistry &registry);

namespace reussir {
TEST_F(ReussirTest, IfrtBridgeMetadataTokensAndEffects) {
  mlir::DialectRegistry registry;
  registerIfrtDialectsAndPasses(registry);
  context->appendDialectRegistry(registry);
  context->loadAllAvailableDialects();
  withModule(
      R"mlir(
    #s = #ifrt.sharding_param<1 to [0] on 1>
    #m = #ifrt.sharding_param<1x2 to [0] on 2>
    !I = !ifrt.array<tensor<8xf32>, #s, [0]>
    !D = !ifrt.array<tensor<?x8xf32>, #m, [3, 1]>
    !R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
    !RD = !reussir.rc<!reussir.array<? x 8 x f32, #reussir.target<devices = [3, 1], sharding = #m>> atomic>
    module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
      ifrt.LoadedExecutable @kernel on devices [0, 1, 3] : (!I) -> (!I, !D)
      func.func @bridge(%input: !R) -> (!R, !RD) attributes {ifrt.function} {
        %view = reussir.array.to_ifrt %input : !R -> !I
        %a, %b, %done = ifrt.CallLoadedExecutable @kernel(%view) : (!I) -> (!I, !D)
        %ra = reussir.array.from_ifrt %a : !I -> !R
        %rb = reussir.array.from_ifrt %b : !D -> !RD
        return %ra, %rb : !R, !RD
      }
    }
  )mlir",
      [](mlir::ModuleOp module) {
        llvm::SmallVector<uint64_t> sizes;
        module.walk([&](ReussirArrayFromIfrtOp op) {
          auto acceptor = llvm::cast<TokenAcceptor>(op.getOperation());
          EXPECT_TRUE(acceptor.shouldAcceptToken());
          EXPECT_FALSE(acceptor.hasToken());
          auto type = acceptor.getTokenType();
          EXPECT_FALSE(type.isDynamicSize());
          EXPECT_EQ(type.getAlign(), 8u);
          sizes.push_back(type.getSize());
          mlir::OpBuilder builder(op);
          auto size = acceptor.buildTokenSize(builder)
                          .getDefiningOp<mlir::arith::ConstantIndexOp>();
          ASSERT_TRUE(size);
          EXPECT_EQ(size.value(), type.getSize());
          auto token = ReussirTokenAllocOp::create(builder, op.getLoc(), type,
                                                   mlir::Value{});
          acceptor.assignToken(token);
          EXPECT_TRUE(acceptor.hasToken());
          EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
          EXPECT_EQ(acceptor.removeToken(), token.getResult());
          EXPECT_FALSE(acceptor.hasToken());
          token.erase();
        });
        EXPECT_EQ(sizes, (llvm::SmallVector<uint64_t>{24, 40}));
        unsigned bridges = 0;
        module.walk([&](mlir::Operation *op) {
          if (!llvm::isa<ReussirArrayToIfrtOp, ReussirArrayFromIfrtOp>(op))
            return;
          EXPECT_FALSE(mlir::isSpeculatable(op));
          EXPECT_FALSE(mlir::isMemoryEffectFree(op));
          ++bridges;
        });
        EXPECT_EQ(bridges, 3u);
        EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
      });
}
} // namespace reussir
#endif
