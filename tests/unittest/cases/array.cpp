#include "Reussir/Conversion/TypeConverter.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <gtest/gtest.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Verifier.h>

import reussir.test;
import reussir.test.value;

namespace reussir {

// Ownership acquisition over an array has two shapes, chosen by
// `kArrayOwnershipUnrollThreshold` in lib/IR/ReussirOps.cpp: at or below the
// threshold the traversal is unrolled into straight-line code, above it the
// traversal becomes an `scf.for` nest with one loop per dimension.
//
// Both shapes have to be counted over the whole function rather than its
// entry block. The loop form puts every project and inc inside the loop body,
// so an entry-block scan reports zero of each and reads as "nothing was
// emitted" — which is how these counts silently stopped describing the
// compiler when the loop form landed.
struct AcquisitionShape {
  size_t views = 0;
  size_t projects = 0;
  size_t incs = 0;
  llvm::SmallVector<int64_t> tripCounts;
};

// The walk is pre-order so that nested loops are seen outermost first and
// `tripCounts` lines up with the array's dimension order; the default
// post-order would report the innermost extent first.
static AcquisitionShape inspectAcquisition(mlir::func::FuncOp funcOp) {
  AcquisitionShape shape;
  funcOp.walk<mlir::WalkOrder::PreOrder>([&](mlir::Operation *op) {
    if (llvm::isa<ReussirArrayViewOp>(op))
      ++shape.views;
    if (llvm::isa<ReussirArrayProjectOp>(op))
      ++shape.projects;
    if (llvm::isa<ReussirRcIncOp>(op))
      ++shape.incs;
    // Trip counts, not just loop count: the point of the loop form is that it
    // visits every element, which only the bounds can attest to.
    if (auto forOp = llvm::dyn_cast<mlir::scf::ForOp>(op)) {
      if (auto bound = forOp.getUpperBound()
                           .getDefiningOp<mlir::arith::ConstantIndexOp>())
        shape.tripCounts.push_back(bound.value());
    }
  });
  return shape;
}

TEST_F(ReussirTest, ParseArrayTypeTest) {
  withType<reussir::ArrayType>(
      SIMPLE_LAYOUT, R"(!reussir.array<4 x 8 x !reussir.rc<i64>>)",
      [](mlir::ModuleOp module, reussir::ArrayType type) {
        EXPECT_EQ(type.getShape().size(), 2u);
        EXPECT_EQ(type.getShape()[0], 4);
        EXPECT_EQ(type.getShape()[1], 8);
        EXPECT_TRUE(llvm::isa<RcType>(type.getElementType()));
      });
}

TEST_F(ReussirTest, ArrayRejectsRankZero) {
  auto loc = mlir::UnknownLoc::get(context.get());
  mlir::ScopedDiagnosticHandler handler(
      context.get(), [](mlir::Diagnostic &) { return mlir::success(); });
  auto i32Type = mlir::IntegerType::get(context.get(), 32);
  EXPECT_FALSE(ArrayType::getChecked([&] { return mlir::emitError(loc); },
                                     context.get(), llvm::ArrayRef<int64_t>{},
                                     mlir::Type(i32Type)));
}

TEST_F(ReussirTest, ValueRecordsAreMemRefElements) {
  for (std::string_view spelling :
       {R"(!reussir.record<compound "MemRefPair" [value] {i32, i64}>)",
        R"(!reussir.record<variant "MemRefChoice" [value] {i32, i64}>)"}) {
    withType<RecordType>(
        SIMPLE_LAYOUT, spelling, [](mlir::ModuleOp module, RecordType type) {
          auto loc = module.getLoc();
          EXPECT_TRUE(mlir::BaseMemRefType::isValidElementType(type));
          auto ranked = mlir::MemRefType::getChecked(
              loc, llvm::ArrayRef<int64_t>{2, 3}, type);
          ASSERT_TRUE(ranked);
          EXPECT_EQ(ranked.getElementType(), type);
          auto unranked = mlir::UnrankedMemRefType::getChecked(
              loc, type, mlir::Attribute{});
          ASSERT_TRUE(unranked);
          EXPECT_EQ(unranked.getElementType(), type);
        });
  }
}

TEST_F(ReussirTest, ArrayProjectionPreservesStridesAndMemorySpace) {
  withModule(R"mlir(
    func.func private @static(memref<3x4x5xf32>) -> memref<4x5xf32, strided<[5, 1], offset: ?>>
    func.func private @dynamic(memref<?x4x5xf32>) -> memref<4x5xf32, strided<[5, 1], offset: ?>>
    func.func private @unit_dimensions(memref<2x1x1xf32, strided<[30, 7, 2], offset: 9>>) -> memref<1x1xf32, strided<[7, 2], offset: ?>>
    func.func private @reversed(memref<3x4xf32, strided<[20, -2], offset: 8>, 3>) -> memref<4xf32, strided<[-2], offset: ?>, 3>
    func.func private @scalar(memref<4xf32, strided<[2], offset: 3>>) -> memref<f32, strided<[], offset: ?>>
  )mlir",
             [](mlir::ModuleOp module) {
               for (auto func : module.getOps<mlir::func::FuncOp>()) {
                 auto input = llvm::cast<mlir::MemRefType>(
                     func.getArgumentTypes().front());
                 EXPECT_EQ(getProjectedArrayViewType(input),
                           func.getResultTypes().front())
                     << func.getName().str();
               }
             });
}

TEST_F(ReussirTest, TokenAcceptorBuildsDynamicArraySize) {
  withModule(R"(
module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @create(%init: i32) {
    %three = arith.constant 3 : index
    %two = arith.constant 2 : index
    %zero = arith.constant 0 : index
    %full = reussir.array.create extents(%three, %two) : !reussir.rc<!reussir.array<? x 4 x ? x i32>> body {
      ^bb0(%array_i0: index, %array_i1: index, %array_i2: index):
        reussir.scf.yield %init : i32
    }
    %empty = reussir.array.create extents(%three, %zero) : !reussir.rc<!reussir.array<? x 4 x ? x i32>> body {
      ^bb0(%array_i0: index, %array_i1: index, %array_i2: index):
        reussir.scf.yield %init : i32
    }
    %uninitialized = reussir.array.create extents(%three, %two) : !reussir.rc<!reussir.array<? x 4 x ? x i32>>
    return
  }
}
)",
             [](mlir::ModuleOp module) {
               llvm::SmallVector<int64_t> sizes;
               module.walk([&](ReussirArrayCreateOp op) {
                 mlir::OpBuilder builder(op);
                 auto acceptor = llvm::cast<TokenAcceptor>(op.getOperation());
                 auto size = acceptor.buildTokenSize(builder);
                 ASSERT_TRUE(size.getType().isIndex());
                 auto constant =
                     size.getDefiningOp<mlir::arith::ConstantIndexOp>();
                 ASSERT_TRUE(constant);
                 sizes.push_back(constant.value());
               });
               EXPECT_EQ(sizes, (llvm::SmallVector<int64_t>{160, 64, 160}));
               EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
             });
}

TEST_F(ReussirTest, ArrayWithoutInitializerHasNoLoop) {
  withModule(R"(
module {
  func.func @create(%n: index, %value: i32) {
    %fixed = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i32>>
    %dynamic = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x i32>>
    %explicit_empty = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i32>> body {}
    %initialized = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i32>> body {
      ^bb0(%i: index):
        reussir.scf.yield %value : i32
    }
    return
  }
}
)",
             [](mlir::ModuleOp module) {
               size_t uninitialized = 0;
               module.walk([&](ReussirArrayCreateOp op) {
                 auto loop = llvm::cast<mlir::LoopLikeOpInterface>(
                     op.getOperation());
                 ASSERT_TRUE(loop.getLoopInductionVars());
                 ASSERT_TRUE(loop.getLoopLowerBounds());
                 ASSERT_TRUE(loop.getLoopUpperBounds());
                 ASSERT_TRUE(loop.getLoopSteps());
                 if (op.getBody().empty()) {
                   ++uninitialized;
                   EXPECT_TRUE(loop.getLoopRegions().empty());
                   EXPECT_TRUE(loop.getLoopInductionVars()->empty());
                   EXPECT_TRUE(loop.getLoopLowerBounds()->empty());
                   EXPECT_TRUE(loop.getLoopUpperBounds()->empty());
                   EXPECT_TRUE(loop.getLoopSteps()->empty());
                 } else {
                   EXPECT_EQ(loop.getLoopRegions().size(), 1u);
                   EXPECT_EQ(loop.getLoopInductionVars()->size(), 1u);
                   EXPECT_EQ(loop.getLoopLowerBounds()->size(), 1u);
                   EXPECT_EQ(loop.getLoopUpperBounds()->size(), 1u);
                   EXPECT_EQ(loop.getLoopSteps()->size(), 1u);
                 }
               });
               EXPECT_EQ(uninitialized, 3u);
               EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
             });
}

TEST_F(ReussirTest, DynamicArraysRequireSharedStorage) {
  auto i32Type = mlir::IntegerType::get(context.get(), 32);
  mlir::Type dynamicArray = ArrayType::get(
      context.get(), {mlir::ShapedType::kDynamic, 2}, i32Type);
  mlir::Type staticArray = ArrayType::get(context.get(), {4, 2}, i32Type);
  auto loc = mlir::UnknownLoc::get(context.get());
  mlir::ScopedDiagnosticHandler handler(context.get(), [](mlir::Diagnostic &) {
    return mlir::success();
  });

  // Shared dynamic boxes and their payload references remain valid.
  EXPECT_TRUE(RcType::getChecked(loc, context.get(), dynamicArray,
                                 Capability::shared, AtomicKind::normal));
  EXPECT_TRUE(RcBoxType::getChecked(loc, context.get(), dynamicArray, false));
  EXPECT_TRUE(RefType::getChecked(loc, context.get(), dynamicArray,
                                  Capability::unspecified, AtomicKind::normal));

  // Both region-local and frozen references would recover the wrong header.
  // Static arrays retain their existing capability support.
  for (Capability capability : {Capability::flex, Capability::rigid}) {
    EXPECT_FALSE(RcType::getChecked(loc, context.get(), dynamicArray,
                                    capability, AtomicKind::normal));
    EXPECT_FALSE(RefType::getChecked(loc, context.get(), dynamicArray,
                                     capability, AtomicKind::normal));
    EXPECT_TRUE(RcType::getChecked(loc, context.get(), staticArray,
                                   capability, AtomicKind::normal));
    EXPECT_TRUE(RefType::getChecked(loc, context.get(), staticArray,
                                    capability, AtomicKind::normal));
  }
  EXPECT_FALSE(RcBoxType::getChecked(loc, context.get(), dynamicArray, true));
  EXPECT_TRUE(RcBoxType::getChecked(loc, context.get(), staticArray, true));
}

TEST_F(ReussirTest, ArrayValueLoweringRequiresStaticShape) {
  mlir::LLVMTypeConverter converter(context.get());
  populateReussirToLLVMTypeConversions(converter);
  auto i32Type = mlir::IntegerType::get(context.get(), 32);

  auto dynamicArray = ArrayType::get(
      context.get(), {mlir::ShapedType::kDynamic}, i32Type);
  auto mixedArray = ArrayType::get(
      context.get(), {4, mlir::ShapedType::kDynamic}, i32Type);
  EXPECT_FALSE(converter.convertType(dynamicArray));
  EXPECT_FALSE(converter.convertType(mixedArray));

  // A statically empty array is still a valid concrete value.
  auto emptyArray = ArrayType::get(context.get(), {0}, i32Type);
  EXPECT_EQ(converter.convertType(emptyArray),
            mlir::LLVM::LLVMArrayType::get(i32Type, 0));
  auto staticArray = ArrayType::get(context.get(), {4, 2}, i32Type);
  EXPECT_EQ(converter.convertType(staticArray),
            mlir::LLVM::LLVMArrayType::get(
                mlir::LLVM::LLVMArrayType::get(i32Type, 2), 4));

  // Rejecting concrete values must not prevent accessing boxed storage.
  auto boxType = RcBoxType::get(context.get(), dynamicArray);
  auto loweredBox = llvm::dyn_cast_if_present<mlir::LLVM::LLVMStructType>(
      converter.convertType(boxType));
  ASSERT_TRUE(loweredBox);
  EXPECT_EQ(loweredBox.getBody().size(), 5u);
  EXPECT_EQ(loweredBox.getBody().back(),
            mlir::LLVM::LLVMArrayType::get(i32Type, 0));
  EXPECT_FALSE(converter.convertType(dynamicArray));
}

TEST_F(ReussirTest, RcTypeIsValidMemRefElementType) {
  auto i64Type = mlir::IntegerType::get(context.get(), 64);
  auto rcType = reussir::RcType::get(context.get(), i64Type);

  EXPECT_TRUE(llvm::isa<mlir::MemRefElementTypeInterface>(rcType));
  EXPECT_TRUE(mlir::BaseMemRefType::isValidElementType(rcType));

  withType<mlir::MemRefType>(
      SIMPLE_LAYOUT, R"(memref<2x!reussir.rc<i64>>)",
      [](mlir::ModuleOp module, mlir::MemRefType type) {
        EXPECT_TRUE(type.hasStaticShape());
        EXPECT_EQ(type.getShape().size(), 1u);
        EXPECT_EQ(type.getShape()[0], 2);
        EXPECT_TRUE(llvm::isa<reussir::RcType>(type.getElementType()));
      });
}

// Two elements, under the unroll threshold: straight-line code, one project
// and one inc per element, no loop.
TEST_F(ReussirValueTransformTest, RefToArrayOfRcAcquisition) {
  testValueAcquisition("!reussir.ref<!reussir.array<2 x !reussir.rc<i32>>>",
                       [](mlir::func::FuncOp funcOp) {
                         auto shape = inspectAcquisition(funcOp);
                         EXPECT_EQ(shape.views, 1u);
                         EXPECT_TRUE(shape.tripCounts.empty());
                         EXPECT_EQ(shape.projects, 2u);
                         EXPECT_EQ(shape.incs, 2u);
                       });
}

// Four elements is exactly the threshold, and the threshold is inclusive, so
// this is the largest nested shape that still unrolls: each row projected
// once and each of its elements once, an inc per element, no loop.
TEST_F(ReussirValueTransformTest, RefToNestedArrayOfRcAcquisition) {
  testValueAcquisition("!reussir.ref<!reussir.array<2 x 2 x !reussir.rc<i32>>>",
                       [](mlir::func::FuncOp funcOp) {
                         auto shape = inspectAcquisition(funcOp);
                         EXPECT_EQ(shape.views, 1u);
                         EXPECT_TRUE(shape.tripCounts.empty());
                         EXPECT_EQ(shape.projects, 6u);
                         EXPECT_EQ(shape.incs, 4u);
                       });
}

// Six elements is over the threshold, so this takes the loop form: a loop per
// dimension over that dimension's extent, and a single project-per-dimension
// and inc in the innermost body — emitted once each, executed once per
// element. The trip counts are what carry "every element is visited" here,
// which is why they are asserted and not just the loop count.
TEST_F(ReussirValueTransformTest, RefToLargeNestedArrayOfRcAcquisition) {
  testValueAcquisition("!reussir.ref<!reussir.array<2 x 3 x !reussir.rc<i32>>>",
                       [](mlir::func::FuncOp funcOp) {
                         auto shape = inspectAcquisition(funcOp);
                         EXPECT_EQ(shape.views, 1u);
                         EXPECT_EQ(shape.tripCounts.size(), 2u);
                         if (shape.tripCounts.size() == 2u) {
                           EXPECT_EQ(shape.tripCounts[0], 2);
                           EXPECT_EQ(shape.tripCounts[1], 3);
                         }
                         EXPECT_EQ(shape.projects, 2u);
                         EXPECT_EQ(shape.incs, 1u);
                       });
}

TEST_F(ReussirTest, ArrayViewAllowsTensorResult) {
  withModule(R"(
!arr4 = !reussir.array<4 x i8>

module {
  func.func @borrow_tensor(%xs : !reussir.ref<!arr4>) -> tensor<4xi8> {
    %view = reussir.array.view(%xs : !reussir.ref<!arr4>) : tensor<4xi8>
    return %view : tensor<4xi8>
  }
}
)",
             [](mlir::ModuleOp module) {
               EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
             });
}

TEST_F(ReussirTest, ArrayWithUniqueViewAllowsTensorBodyArgument) {
  withModule(R"(
!arr4 = !reussir.array<4 x i8>
!rc_arr4 = !reussir.rc<!arr4>

module {
  func.func @borrow_tensor(%xs : !rc_arr4) -> !rc_arr4 {
    %updated = reussir.array.with_unique_view (%xs : !rc_arr4) -> !rc_arr4 {
      ^bb0(%view: tensor<4xi8>):
        reussir.scf.yield
    }
    return %updated : !rc_arr4
  }
}
)",
             [](mlir::ModuleOp module) {
               EXPECT_TRUE(mlir::succeeded(mlir::verify(module)));
             });
}
} // namespace reussir
