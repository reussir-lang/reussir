#include "Reussir-c/Types.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>
#include <mlir/CAPI/IR.h>
#include <mlir/IR/Verifier.h>
#include <mlir/AsmParser/AsmParser.h>

import reussir.test;

namespace reussir {
TEST_F(ReussirTest, StablehloAsyncTypesAreCheckedAndDistinct) {
  auto ctx = wrap(context.get());
  auto token = reussirStablehloTokenTypeGet(ctx);
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 0, nullptr).ptr, nullptr);
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, -1, nullptr).ptr, nullptr);
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, nullptr).ptr, nullptr);
  MlirType nullType{nullptr};
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, &nullType).ptr, nullptr);
  auto scalar = wrap(mlir::IntegerType::get(context.get(), 32));
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, &scalar).ptr, nullptr);
  mlir::MLIRContext other;
  auto foreign =
      wrap(mlir::RankedTensorType::get({}, mlir::Float32Type::get(&other)));
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, &foreign).ptr, nullptr);
#ifdef REUSSIR_ENABLE_OPENXLA
  ASSERT_NE(token.ptr, nullptr);
  EXPECT_NE(token.ptr, reussirTokenTypeGet(ctx, 8, 16).ptr);
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, &token).ptr, nullptr);
  auto tensor = wrap(mlir::RankedTensorType::get(
      {mlir::ShapedType::kDynamic, 0}, mlir::Float32Type::get(context.get())));
  MlirType payloads[] = {tensor, tensor};
  auto future = reussirStablehloFutureTypeGet(ctx, 2, payloads);
  ASSERT_NE(future.ptr, nullptr);
  EXPECT_EQ(reussirStablehloFutureTypeGet(ctx, 1, &future).ptr, nullptr);
#else
  EXPECT_EQ(token.ptr, nullptr);
#endif
}

TEST_F(ReussirTest, DeviceArrayIfrtViewsPreserveMetadata) {
  MlirType nullType{nullptr};
  EXPECT_EQ(reussirArrayTypeGetIfrtType(nullType).ptr, nullptr);
  EXPECT_EQ(reussirIfrtArrayGetDevices(nullType).ptr, nullptr);
  EXPECT_EQ(reussirIfrtArrayGetHloSharding(nullType).ptr, nullptr);
  auto scalar = wrap(mlir::IntegerType::get(context.get(), 32));
  EXPECT_EQ(reussirArrayTypeGetIfrtType(scalar).ptr, nullptr);
  auto target = mlir::parseAttribute("#reussir.target<devices = [3]>", context.get());
  ASSERT_TRUE(target);
  int64_t shape[] = {0, mlir::ShapedType::kDynamic};
  auto descriptor = reussirArrayTypeGetWithTarget(2, shape, scalar, wrap(target));
  ASSERT_NE(descriptor.ptr, nullptr);
  auto view = reussirArrayTypeGetIfrtType(descriptor);
#ifdef REUSSIR_ENABLE_OPENXLA
  ASSERT_NE(view.ptr, nullptr);
  auto expected = mlir::parseType("!ifrt.array<tensor<0x?xi32>, #ifrt.sharding_unspecified, [3]>", context.get());
  ASSERT_TRUE(expected);
  EXPECT_EQ(unwrap(view), expected);
  EXPECT_NE(reussirIfrtArrayGetDevices(view).ptr, nullptr);
  EXPECT_EQ(reussirIfrtArrayGetHloSharding(view).ptr, nullptr);
#else
  EXPECT_EQ(view.ptr, nullptr);
#endif
}

TEST_F(ReussirTest, BasicContextTest) {
  ASSERT_NE(context, nullptr);
  // Basic test that the context is properly initialized
  EXPECT_TRUE(context->isMultithreadingEnabled() ||
              !context->isMultithreadingEnabled());
}

// Test parsing RecordType from string
TEST_F(ReussirTest, ParseRecordTypeTest) {
  withType<reussir::RecordType>(
      SIMPLE_LAYOUT, R"(!reussir.record<compound "Test" {}>)",
      [](mlir::ModuleOp module, reussir::RecordType type) {

      });
}

TEST_F(ReussirTest, TokenAcceptorBuildsFixedSize) {
  withModule(R"(
module {
  func.func @create(%init: i32) -> !reussir.rc<i32> {
    %rc = reussir.rc.create value(%init : i32) : !reussir.rc<i32>
    return %rc : !reussir.rc<i32>
  }
}
)", [](mlir::ModuleOp module) {
    module.walk([](ReussirRcCreateOp op) {
      mlir::OpBuilder builder(op);
      auto acceptor = llvm::cast<TokenAcceptor>(op.getOperation());
      auto size = acceptor.buildTokenSize(builder);
      ASSERT_TRUE(size.getType().isIndex());
      auto constant = size.getDefiningOp<mlir::arith::ConstantIndexOp>();
      ASSERT_TRUE(constant);
      EXPECT_EQ(constant.value(), 8);
    });
    EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
  });
}
} // namespace reussir
