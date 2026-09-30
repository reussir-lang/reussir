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
/// This file tests IFRT bridges and JIT kernel packaging.
///
//===----------------------------------------------------------------------===//

#ifdef REUSSIR_ENABLE_OPENXLA
#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/BLAKE3.h>
#include <mlir/IR/Diagnostics.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>

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

namespace {
constexpr llvm::StringLiteral jitSource = R"mlir(
  #s = #ifrt.sharding_param<1 to [0] on 1>
  !I = !ifrt.array<tensor<8xf32>, #s, [0]>
  module {
    func.func @host(%input: !I) -> !I attributes {ifrt.function} {
      %a, %ready = ifrt.Call @first::@main(%input) on devices [0] : (!I) -> !I
      %b, %done = ifrt.Call @second::@main(%a) after %ready on devices [0] : (!I) -> !I
      return %b : !I
    }
    module @first attributes {sym_visibility = "private"} {
      func.func @main(%arg: tensor<8xf32> loc("kernel.rr":2:10)) -> tensor<8xf32> {
        return %arg : tensor<8xf32> loc("kernel.rr":3:5)
      } loc("kernel.rr":2:1)
    } loc("kernel.rr":1:1)
    module @second attributes {sym_visibility = "private"} {
      func.func @main(%arg: tensor<8xf32> loc("kernel.rr":2:10)) -> tensor<8xf32> {
        return %arg : tensor<8xf32> loc("kernel.rr":3:5)
      } loc("kernel.rr":2:1)
    } loc("kernel.rr":1:1)
  }
)mlir";

mlir::LogicalResult packageKernels(mlir::ModuleOp module) {
  mlir::PassManager pm(module.getContext());
  pm.addPass(createReussirIFRTJustInTimeTransformPass());
  pm.addPass(mlir::createSymbolDCEPass());
  return pm.run(module);
}
} // namespace

TEST_F(ReussirTest, IfrtJitBytecodeRoundTripAndDeduplication) {
  mlir::DialectRegistry registry;
  registerIfrtDialectsAndPasses(registry);
  context->appendDialectRegistry(registry);
  context->loadAllAvailableDialects();
  auto module = parse(jitSource);
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(packageKernels(*module)));
  EXPECT_TRUE(module->getOps<mlir::ModuleOp>().empty());
  auto codes = module->getOps<ReussirIFRTBytecodeOp>();
  ASSERT_EQ(llvm::range_size(codes), 1u);
  auto code = *codes.begin();
  llvm::BLAKE3 hash;
  hash.update(code.getBytecode());
  auto digest = hash.final();
  EXPECT_EQ(code.getChecksum(), llvm::toHex(digest, /*LowerCase=*/true));
  code.setChecksumAttr(mlir::StringAttr::get(context.get(), llvm::toHex(digest)));
  EXPECT_TRUE(mlir::succeeded(mlir::verify(*module)));
  code.setChecksumAttr(mlir::StringAttr::get(
      context.get(), llvm::toHex(digest, /*LowerCase=*/true)));
  auto kernel = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                        context.get());
  ASSERT_TRUE(kernel);
  auto main = kernel->lookupSymbol<mlir::func::FuncOp>("main");
  ASSERT_TRUE(main);
  EXPECT_TRUE(main.isPublic());
  EXPECT_FALSE(kernel->getSymName());
  EXPECT_EQ(kernel->getLoc(),
            mlir::FileLineColLoc::get(context.get(), "kernel.rr", 1, 1));
  EXPECT_EQ(main.getLoc(),
            mlir::FileLineColLoc::get(context.get(), "kernel.rr", 2, 1));
  EXPECT_EQ(main.getArgument(0).getLoc(),
            mlir::FileLineColLoc::get(context.get(), "kernel.rr", 2, 10));
  EXPECT_EQ(main.getBody().front().getTerminator()->getLoc(),
            mlir::FileLineColLoc::get(context.get(), "kernel.rr", 3, 5));
  auto host = module->lookupSymbol<mlir::func::FuncOp>("host");
  auto calls = llvm::to_vector(host.getOps<ReussirPJRTJitCallOp>());
  ASSERT_EQ(calls.size(), 2u);
  EXPECT_EQ(calls[0].getCalleeAttr(), calls[1].getCalleeAttr());
  ASSERT_EQ(calls[1].getControlInputs().size(), 1u);
  EXPECT_EQ(calls[1].getControlInputs()[0], calls[0].getControlOutput());
  EXPECT_FALSE(mlir::isMemoryEffectFree(calls[0]));
  std::string printed;
  llvm::raw_string_ostream stream(printed);
  module->print(stream);
  ASSERT_TRUE(mlir::parseSourceString<mlir::ModuleOp>(printed, context.get()));
  EXPECT_TRUE(mlir::succeeded(packageKernels(*module)));
  EXPECT_EQ(llvm::range_size(module->getOps<ReussirIFRTBytecodeOp>()), 1u);

  // A conflicting user symbol must be uniqued once, even for identical kernels.
  auto collision = parse(jitSource);
  ASSERT_TRUE(collision);
  mlir::OpBuilder builder(context.get());
  builder.setInsertionPointToEnd(collision->getBody());
  mlir::func::FuncOp::create(builder, collision->getLoc(), code.getSymName(),
                             builder.getFunctionType({}, {}))
      .setPrivate();
  ASSERT_TRUE(mlir::succeeded(packageKernels(*collision)));
  auto uniqueCodes = collision->getOps<ReussirIFRTBytecodeOp>();
  ASSERT_EQ(llvm::range_size(uniqueCodes), 1u);
  EXPECT_NE((*uniqueCodes.begin()).getSymName(), code.getSymName());
}

TEST_F(ReussirTest, IfrtJitFailureDoesNotPartiallyRewriteCalls) {
  mlir::DialectRegistry registry;
  registerIfrtDialectsAndPasses(registry);
  context->appendDialectRegistry(registry);
  context->loadAllAvailableDialects();
  std::string source = jitSource.str();
  source.insert(source.rfind("return %arg"),
                "%bad = arith.addf %arg, %arg : tensor<8xf32>\n");
  auto module = parse(source);
  ASSERT_TRUE(module);
  std::string before, after;
  llvm::raw_string_ostream beforeStream(before), afterStream(after);
  module->print(beforeStream);
  mlir::ScopedDiagnosticHandler handler(
      context.get(), [](mlir::Diagnostic &) { return mlir::success(); });
  EXPECT_TRUE(mlir::failed(packageKernels(*module)));
  module->print(afterStream);
  EXPECT_EQ(before, after);
}

TEST_F(ReussirTest, IfrtJitRejectsCorruptBytecodeAndMismatchedSignature) {
  mlir::DialectRegistry registry;
  registerIfrtDialectsAndPasses(registry);
  context->appendDialectRegistry(registry);
  context->loadAllAvailableDialects();
  auto module = parse(jitSource);
  ASSERT_TRUE(module);
  ASSERT_TRUE(mlir::succeeded(packageKernels(*module)));
  auto code = *module->getOps<ReussirIFRTBytecodeOp>().begin();
  auto checksum = code.getChecksumAttr();
  std::string corrupt = checksum.getValue().str();
  corrupt[0] = corrupt[0] == '0' ? '1' : '0';
  std::string diagnostic;
  mlir::ScopedDiagnosticHandler handler(context.get(),
                                        [&](mlir::Diagnostic &diag) {
                                          diagnostic = diag.str();
                                          return mlir::success();
                                        });
  code.setChecksumAttr(mlir::StringAttr::get(context.get(), corrupt));
  EXPECT_TRUE(mlir::failed(mlir::verify(*module)));
  EXPECT_NE(diagnostic.find("checksum does not match"), std::string::npos);
  code.setChecksumAttr(checksum);

  // Valid bytecode and a correct checksum are insufficient if @main disagrees.
  std::string source = jitSource.str();
  for (size_t pos = 0; (pos = source.find("8xf32", pos)) != std::string::npos;
       ++pos)
    source[pos] = '4';
  auto other = parse(source);
  ASSERT_TRUE(other);
  ASSERT_TRUE(mlir::succeeded(packageKernels(*other)));
  auto otherCode = *other->getOps<ReussirIFRTBytecodeOp>().begin();
  code.setBytecodeAttr(otherCode.getBytecodeAttr());
  code.setChecksumAttr(otherCode.getChecksumAttr());
  EXPECT_TRUE(mlir::failed(mlir::verify(*module)));
  EXPECT_NE(diagnostic.find("must match the kernel signature"),
            std::string::npos);
}
} // namespace reussir
#endif
