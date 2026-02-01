//===-- SCFOpsLowering.cpp - Reussir SCF ops lowering impl -----*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/SCFOpsLowering.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include <algorithm>
#include <format>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/BLAKE3.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/Math/IR/Math.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Pass/Pass.h>
#include <utility>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRSCFOPSLOWERINGPASS
#include "Reussir/Conversion/Passes.h.inc"
//===----------------------------------------------------------------------===//
// String Pattern Trie
//===----------------------------------------------------------------------===//
namespace {

/// =======================
/// Hashing / naming
/// =======================

std::string b62encode(llvm::APInt value) {
  const char *alphabet =
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  if (value.isZero())
    return std::string(1, alphabet[0]);

  std::string result;
  while (!value.isZero()) {
    uint64_t rem;
    llvm::APInt::udivrem(value, 62, value, rem);
    result.push_back(alphabet[rem]);
  }
  std::reverse(result.begin(), result.end());
  return result;
}

llvm::APInt blakeHashedPattern(mlir::ArrayAttr pattern) {
  llvm::BLAKE3 blake3;
  for (auto attr : pattern.getValue()) {
    if (auto s = llvm::dyn_cast<mlir::StringAttr>(attr))
      blake3.update(s.getValue());
  }
  auto result = blake3.final();
  auto data = std::bit_cast<std::array<uint64_t, 4>>(result);
  return llvm::APInt(256, data);
}

std::string decisionFunctionName(mlir::ArrayAttr patterns) {
  std::string hash = b62encode(blakeHashedPattern(patterns));
  const char *sep = (hash[0] >= '0' && hash[0] <= '9') ? "_" : "";
  return std::format("_RNvC25REUSSIR_STRING_DISPATCHER{}{}{}", hash.size(), sep,
                     hash);
}

struct PatternInfo {
  size_t originalIdx;
  llvm::StringRef pattern;
};

static std::pair<mlir::Value, mlir::Value>
buildDecisionTree(mlir::Location loc, mlir::OpBuilder &builder,
                  mlir::Type indexType, mlir::Type i1Type,
                  mlir::Value currentSlice,
                  llvm::ArrayRef<PatternInfo> patterns) {
  if (patterns.empty()) {
    auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
    auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
    return {poison.getResult(), falseVal.getResult()};
  }

  if (patterns.size() == 1) {
    const auto &p = patterns[0];
    mlir::Value condition;
    if (p.pattern.empty()) {
      auto len = builder.create<ReussirStrLenOp>(loc, builder.getIndexType(),
                                                 currentSlice);
      auto zero = builder.create<mlir::arith::ConstantIndexOp>(loc, 0);
      condition = builder.create<mlir::arith::CmpIOp>(
          loc, mlir::arith::CmpIPredicate::eq, len.getResult(),
          zero.getResult());
    } else {
      auto startswith = builder.create<ReussirStrUnsafeStartWithOp>(
          loc, i1Type, currentSlice, builder.getStringAttr(p.pattern));
      auto len = builder.create<ReussirStrLenOp>(loc, builder.getIndexType(),
                                                 currentSlice);
      auto expectedLen =
          builder.create<mlir::arith::ConstantIndexOp>(loc, p.pattern.size());
      auto lenOk = builder.create<mlir::arith::CmpIOp>(
          loc, mlir::arith::CmpIPredicate::eq, len.getResult(),
          expectedLen.getResult());
      condition = builder.create<mlir::arith::AndIOp>(
          loc, startswith.getResult(), lenOk.getResult());
    }

    auto ifOp = builder.create<mlir::scf::IfOp>(
        loc, mlir::TypeRange{indexType, i1Type}, condition,
        /*addThenRegion=*/true, /*addElseRegion=*/true);

    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto idx =
          builder.create<mlir::arith::ConstantIndexOp>(loc, p.originalIdx);
      auto trueVal = builder.create<mlir::arith::ConstantIntOp>(loc, 1, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{idx.getResult(), trueVal.getResult()});
    }

    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
      auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
    }
    return {ifOp.getResult(0), ifOp.getResult(1)};
  }

  // Find LCP
  llvm::StringRef first = patterns[0].pattern;
  size_t lcpLen = first.size();
  for (size_t i = 1; i < patterns.size(); ++i) {
    size_t j = 0;
    while (j < lcpLen && j < patterns[i].pattern.size() &&
           first[j] == patterns[i].pattern[j]) {
      j++;
    }
    lcpLen = j;
  }

  if (lcpLen > 0) {
    auto lcp = first.substr(0, lcpLen);
    auto len = builder.create<ReussirStrLenOp>(loc, builder.getIndexType(),
                                               currentSlice);
    auto minLen = builder.create<mlir::arith::ConstantIndexOp>(loc, lcpLen);
    auto lenOk = builder.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::uge, len.getResult(),
        minLen.getResult());

    auto ifLen = builder.create<mlir::scf::IfOp>(
        loc, mlir::TypeRange{indexType, i1Type}, lenOk.getResult(),
        /*addThenRegion=*/true, /*addElseRegion=*/true);

    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(&ifLen.getThenRegion().front());

      auto startswith = builder.create<ReussirStrUnsafeStartWithOp>(
          loc, i1Type, currentSlice, builder.getStringAttr(lcp));

      auto ifMatch = builder.create<mlir::scf::IfOp>(
          loc, mlir::TypeRange{indexType, i1Type}, startswith.getResult(),
          /*addThenRegion=*/true, /*addElseRegion=*/true);

      {
        mlir::OpBuilder::InsertionGuard thenGuard(builder);
        builder.setInsertionPointToStart(&ifMatch.getThenRegion().front());
        auto offset = builder.create<mlir::arith::ConstantIndexOp>(loc, lcpLen);
        auto nextSlice = builder.create<ReussirStrSliceOp>(
            loc, currentSlice.getType(), currentSlice, offset.getResult());

        llvm::SmallVector<PatternInfo> nextPatterns;
        for (const auto &p : patterns) {
          nextPatterns.push_back({p.originalIdx, p.pattern.substr(lcpLen)});
        }
        auto res = buildDecisionTree(loc, builder, indexType, i1Type,
                                     nextSlice.getResult(), nextPatterns);
        builder.create<mlir::scf::YieldOp>(
            loc, mlir::ValueRange{res.first, res.second});
      }

      {
        mlir::OpBuilder::InsertionGuard elseGuard(builder);
        builder.setInsertionPointToStart(&ifMatch.getElseRegion().front());
        auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
        auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
        builder.create<mlir::scf::YieldOp>(
            loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
      }
      builder.create<mlir::scf::YieldOp>(loc, ifMatch.getResults());
    }

    {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(&ifLen.getElseRegion().front());
      auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
      auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
    }

    return {ifLen.getResult(0), ifLen.getResult(1)};
  }

  // LCP is 0, dispatch on byte
  llvm::SmallVector<PatternInfo> emptyPatterns;
  llvm::SmallVector<PatternInfo> nonEmptyPatterns;
  for (const auto &p : patterns) {
    if (p.pattern.empty()) {
      emptyPatterns.push_back(p);
    } else {
      nonEmptyPatterns.push_back(p);
    }
  }

  auto len = builder.create<ReussirStrLenOp>(loc, builder.getIndexType(),
                                             currentSlice);
  auto zero = builder.create<mlir::arith::ConstantIndexOp>(loc, 0);
  auto isZero = builder.create<mlir::arith::CmpIOp>(
      loc, mlir::arith::CmpIPredicate::eq, len.getResult(), zero.getResult());

  auto ifZero = builder.create<mlir::scf::IfOp>(
      loc, mlir::TypeRange{indexType, i1Type}, isZero.getResult(),
      /*addThenRegion=*/true, /*addElseRegion=*/true);

  // Then: len == 0
  {
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(&ifZero.getThenRegion().front());
    if (!emptyPatterns.empty()) {
      auto idx = builder.create<mlir::arith::ConstantIndexOp>(
          loc, emptyPatterns[0].originalIdx);
      auto trueVal = builder.create<mlir::arith::ConstantIntOp>(loc, 1, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{idx.getResult(), trueVal.getResult()});
    } else {
      auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
      auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
    }
  }

  // Else: len > 0, dispatch on byte
  {
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(&ifZero.getElseRegion().front());

    if (nonEmptyPatterns.empty()) {
      auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
      auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
      builder.create<mlir::scf::YieldOp>(
          loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
    } else {
      auto byteAtZero = builder.create<ReussirStrUnsafeByteAtOp>(
          loc, builder.getI8Type(), currentSlice, zero.getResult());
      auto byteIndex = builder.create<mlir::arith::IndexCastOp>(
          loc, builder.getIndexType(), byteAtZero.getResult());

      llvm::SmallVector<int64_t> cases;
      llvm::MapVector<uint8_t, llvm::SmallVector<PatternInfo>> groups;

      for (const auto &p : nonEmptyPatterns) {
        uint8_t b = static_cast<uint8_t>(p.pattern[0]);
        if (groups.find(b) == groups.end()) {
          cases.push_back(b);
        }
        groups[b].push_back({p.originalIdx, p.pattern.substr(1)});
      }

      auto one = builder.create<mlir::arith::ConstantIndexOp>(loc, 1);
      auto nextSlice = builder.create<ReussirStrSliceOp>(
          loc, currentSlice.getType(), currentSlice, one.getResult());

      auto switchOp = builder.create<mlir::scf::IndexSwitchOp>(
          loc, mlir::TypeRange{indexType, i1Type}, byteIndex.getResult(), cases,
          cases.size());

      for (auto [idx, b] : llvm::enumerate(cases)) {
        auto &region = switchOp.getCaseRegions()[idx];
        region.emplaceBlock();
        mlir::OpBuilder::InsertionGuard caseGuard(builder);
        builder.setInsertionPointToStart(&region.front());
        auto res = buildDecisionTree(loc, builder, indexType, i1Type,
                                     nextSlice.getResult(), groups[b]);
        builder.create<mlir::scf::YieldOp>(
            loc, mlir::ValueRange{res.first, res.second});
      }

      // Default region
      {
        auto &region = switchOp.getDefaultRegion();
        region.emplaceBlock();
        mlir::OpBuilder::InsertionGuard defaultGuard(builder);
        builder.setInsertionPointToStart(&region.front());
        auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);
        auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
        builder.create<mlir::scf::YieldOp>(
            loc, mlir::ValueRange{poison.getResult(), falseVal.getResult()});
      }
      builder.create<mlir::scf::YieldOp>(loc, switchOp.getResults());
    }
  }

  return {ifZero.getResult(0), ifZero.getResult(1)};
}

std::string emitDecisionFunction(mlir::ModuleOp module,
                                 mlir::OpBuilder &builder,
                                 mlir::ArrayAttr patterns) {
  std::string funcName = decisionFunctionName(patterns);
  if (module.lookupSymbol<mlir::func::FuncOp>(funcName)) {
    return funcName;
  }

  mlir::OpBuilder::InsertionGuard guard(builder);
  builder.setInsertionPointToStart(module.getBody());

  auto indexType = builder.getIndexType();
  auto i1Type = builder.getI1Type();

  auto strType =
      reussir::StrType::get(builder.getContext(), reussir::LifeScope::local);
  auto funcType = builder.getFunctionType({strType}, {indexType, i1Type});

  auto func =
      builder.create<mlir::func::FuncOp>(module.getLoc(), funcName, funcType);
  func.setVisibility(mlir::SymbolTable::Visibility::Private);
  func->setAttr("llvm.linkage",
                mlir::LLVM::LinkageAttr::get(builder.getContext(),
                                             mlir::LLVM::Linkage::Internal));
  mlir::Block *entry = func.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  llvm::SmallVector<PatternInfo> patternInfos;
  for (auto [idx, attr] : llvm::enumerate(patterns.getValue())) {
    patternInfos.push_back(
        {idx, llvm::cast<mlir::StringAttr>(attr).getValue()});
  }

  auto res = buildDecisionTree(module.getLoc(), builder, indexType, i1Type,
                               entry->getArgument(0), patternInfos);

  builder.create<mlir::func::ReturnOp>(module.getLoc(),
                                       mlir::ValueRange{res.first, res.second});

  return funcName;
}

} // namespace

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

struct ReussirNullableDispatchOpRewritePattern
    : public mlir::OpRewritePattern<ReussirNullableDispatchOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirNullableDispatchOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // First, create a check operation to get the null flag from the input.
    auto flag = rewriter.create<reussir::ReussirNullableCheckOp>(
        op.getLoc(), op.getNullable());
    auto scfIfOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), flag, /*addThenRegion=*/true,
        /*addElseRegion=*/true);
    // first, do the easy part, for else region, we can just inline the
    // operation
    rewriter.inlineBlockBefore(&*op.getNullRegion().begin(),
                               &*scfIfOp.getElseRegion().begin(),
                               scfIfOp.getElseRegion().begin()->begin());

    // Now, for the then region, we first create the coerced value
    rewriter.setInsertionPointToStart(&scfIfOp.getThenRegion().front());
    auto coerced = rewriter.create<reussir::ReussirNullableCoerceOp>(
        op.getLoc(), op.getNullable().getType().getPtrTy(), op.getNullable());
    // Then we inline the region, supplying coerced value as the argument
    rewriter.inlineBlockBefore(
        &*op.getNonNullRegion().begin(), &*scfIfOp.getThenRegion().begin(),
        scfIfOp.getThenRegion().begin()->end(), mlir::ValueRange{coerced});
    rewriter.replaceOp(op, scfIfOp);
    return mlir::success();
  }
};

struct ReussirRecordDispatchOpRewritePattern
    : public mlir::OpRewritePattern<ReussirRecordDispatchOp> {
  using OpRewritePattern::OpRewritePattern;

private:
  static mlir::DenseI64ArrayAttr
  getAllTagsAsSingletons(ReussirRecordDispatchOp op) {
    llvm::SmallVector<int64_t> allTags;
    for (auto tagSet : op.getTagSets()) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      if (tagArray.size() != 1)
        return {};
      allTags.push_back(tagArray[0]);
    }
    return mlir::DenseI64ArrayAttr::get(op.getContext(), allTags);
  }
  static mlir::Value buildPreDispatcher(ReussirRecordTagOp tag,
                                        ReussirRecordDispatchOp op,
                                        mlir::PatternRewriter &rewriter) {
    mlir::OpBuilder::InsertionGuard guard(rewriter);
    llvm::DenseMap<int64_t, int64_t> tagToRegionIdx;
    llvm::SmallVector<int64_t> allTags;
    for (auto [idx, tagSet] : llvm::enumerate(op.getTagSets())) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      for (auto tag : tagArray.asArrayRef()) {
        allTags.push_back(tag);
        tagToRegionIdx[tag] = idx;
      }
    };
    auto indexSwitchOp = rewriter.create<mlir::scf::IndexSwitchOp>(
        op.getLoc(), rewriter.getIndexType(), tag.getResult(), allTags,
        allTags.size());

    for (auto [tag, region] :
         llvm::zip(allTags, indexSwitchOp.getCaseRegions())) {
      mlir::Block *block = rewriter.createBlock(&region, region.begin());
      rewriter.setInsertionPointToStart(block);
      auto constantIdx = rewriter.create<mlir::arith::ConstantIndexOp>(
          op.getLoc(), tagToRegionIdx[tag]);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          constantIdx->getResults());
    }
    {
      mlir::Block *block =
          rewriter.createBlock(&indexSwitchOp.getDefaultRegion(),
                               indexSwitchOp.getDefaultRegion().begin());
      rewriter.setInsertionPointToStart(block);
      auto poison = rewriter.create<mlir::ub::PoisonOp>(
          op.getLoc(), rewriter.getIndexType());
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), poison->getResults());
    }
    return indexSwitchOp.getResult(0);
  }

public:
  mlir::LogicalResult
  matchAndRewrite(ReussirRecordDispatchOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // First, create a RecordTagOps operation to get the tag from the input.
    auto tag = rewriter.create<reussir::ReussirRecordTagOp>(op.getLoc(),
                                                            op.getVariant());

    mlir::Value outerSwitchValue;
    mlir::DenseI64ArrayAttr outerSwitchCases;

    if (auto allTags = getAllTagsAsSingletons(op)) {
      outerSwitchValue = tag.getResult();
      outerSwitchCases = allTags;
    } else {
      outerSwitchValue = buildPreDispatcher(tag, op, rewriter);
      outerSwitchCases = mlir::DenseI64ArrayAttr::get(
          op.getContext(),
          llvm::to_vector(llvm::seq<int64_t>(0, op.getTagSets().size())));
    }

    auto indexSwitchOp = rewriter.create<mlir::scf::IndexSwitchOp>(
        op.getLoc(), op->getResultTypes(), outerSwitchValue, outerSwitchCases,
        outerSwitchCases.size());
    // mark default region as unreachable
    {
      mlir::Block *block =
          rewriter.createBlock(&indexSwitchOp.getDefaultRegion(),
                               indexSwitchOp.getDefaultRegion().begin());
      rewriter.setInsertionPointToStart(block);
      llvm::SmallVector<mlir::Value, 1> poisonValues;
      if (op.getValue()) {
        auto poison = rewriter.create<mlir::ub::PoisonOp>(
            op.getLoc(), op.getValue().getType());
        poisonValues.push_back(poison);
      }
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), poisonValues);
    }
    for (auto [idx, tagSet, region] :
         llvm::enumerate(op.getTagSets(), indexSwitchOp.getCaseRegions())) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      llvm::SmallVector<mlir::Value, 1> args;
      mlir::Block *block = rewriter.createBlock(&region, region.begin());
      rewriter.setInsertionPointToStart(block);
      // if we know exact variant, we need to coerce the variant to the exact
      // type
      if (tagArray.size() == 1) {
        RefType variantRef = op.getVariant().getType();
        RecordType recordType =
            llvm::cast<RecordType>(variantRef.getElementType());
        mlir::Type targetVariantType =
            getProjectedType(recordType.getMembers()[tagArray[0]],
                             recordType.getMemberIsField()[tagArray[0]],
                             variantRef.getCapability());
        RefType coercedType =
            RefType::get(rewriter.getContext(), targetVariantType,
                         variantRef.getCapability());
        auto coerced = rewriter.create<reussir::ReussirRecordCoerceOp>(
            op.getLoc(), coercedType, rewriter.getIndexAttr(tagArray[0]),
            op.getVariant());
        args.push_back(coerced);
      }
      // inline the block, supplying coerced value as the argument
      rewriter.inlineBlockBefore(&op->getRegion(idx).front(), block,
                                 block->end(), args);
    }
    rewriter.replaceOp(op, indexSwitchOp);
    return mlir::success();
  }
};

struct ReussirClosureUniqifyOpRewritePattern
    : public mlir::OpRewritePattern<ReussirClosureUniqifyOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirClosureUniqifyOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // Create a check operation to see if the closure is unique
    auto isUnique = rewriter.create<reussir::ReussirRcIsUniqueOp>(
        op.getLoc(), op.getClosure());

    // Create an SCF if-else operation
    auto scfIfOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), isUnique, /*addThenRegion=*/true,
        /*addElseRegion=*/true);

    // In the then region (closure is unique), just return the original closure
    rewriter.setInsertionPointToStart(&scfIfOp.getThenRegion().front());
    rewriter.create<mlir::scf::YieldOp>(op.getLoc(), op.getClosure());

    // In the else region (closure is not unique), clone the closure, dec the
    // original rc pointer
    rewriter.setInsertionPointToStart(&scfIfOp.getElseRegion().front());
    auto cloned = rewriter.create<reussir::ReussirClosureCloneOp>(
        op.getLoc(), op.getClosure().getType(), op.getClosure());
    rewriter.create<reussir::ReussirRcDecOp>(op.getLoc(), mlir::Type{},
                                             op.getClosure());
    rewriter.create<mlir::scf::YieldOp>(op.getLoc(), cloned.getResult());

    rewriter.replaceOp(op, scfIfOp);
    return mlir::success();
  }
};

struct ReussirScfYieldOpRewritePattern
    : public mlir::OpRewritePattern<ReussirScfYieldOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirScfYieldOp op,
                  mlir::PatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<mlir::scf::YieldOp>(op, op->getOperands());
    return mlir::success();
  }
};

struct ReussirTokenEnsureOpRewritePattern
    : public mlir::OpRewritePattern<ReussirTokenEnsureOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirTokenEnsureOp op,
                  mlir::PatternRewriter &rewriter) const override {
    auto nullableDispatchOp = rewriter.create<ReussirNullableDispatchOp>(
        op.getLoc(), op.getType(), op.getNullableToken());

    {
      mlir::Block *thenBlock =
          rewriter.createBlock(&nullableDispatchOp.getNonNullRegion(), {},
                               op.getType(), {op.getLoc()});
      rewriter.setInsertionPointToStart(thenBlock);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          thenBlock->getArgument(0));
    }
    {
      mlir::Block *elseBlock =
          rewriter.createBlock(&nullableDispatchOp.getNullRegion());
      rewriter.setInsertionPointToStart(elseBlock);
      auto allocatedToken =
          rewriter.create<ReussirTokenAllocOp>(op.getLoc(), op.getType());
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          allocatedToken->getResults());
    }
    rewriter.replaceOp(op, nullableDispatchOp);
    return mlir::success();
  }
};

struct ReussirStrByteAtOpRewritePattern
    : public mlir::OpRewritePattern<ReussirStrByteAtOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirStrByteAtOp op,
                  mlir::PatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();

    // Get string length
    auto lenOp = rewriter.create<reussir::ReussirStrLenOp>(
        loc, rewriter.getIndexType(), op.getStr());

    // Check if index is within bounds (index < len)
    auto inBounds = rewriter.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::ult, op.getIndex(), lenOp.getResult());

    // Create if-else block
    auto ifOp = rewriter.create<mlir::scf::IfOp>(
        loc, op.getResult().getType(), inBounds, /*addThenRegion=*/true,
        /*addElseRegion=*/true);

    // Then region: Unsafe access
    {
      auto &thenBlock = ifOp.getThenRegion().front();
      rewriter.setInsertionPointToStart(&thenBlock);
      auto unsafeByte = rewriter.create<reussir::ReussirStrUnsafeByteAtOp>(
          loc, rewriter.getI8Type(), op.getStr(), op.getIndex());
      rewriter.create<mlir::scf::YieldOp>(loc, unsafeByte.getResult());
    }

    // Else region: Return 0
    {
      auto &elseBlock = ifOp.getElseRegion().front();
      rewriter.setInsertionPointToStart(&elseBlock);
      auto zero = rewriter.create<mlir::arith::ConstantIntOp>(loc, 0, 8);
      rewriter.create<mlir::scf::YieldOp>(loc, zero->getResult(0));
    }

    rewriter.replaceOp(op, ifOp.getResult(0));
    return mlir::success();
  }
};
struct ReussirStrSelectOpRewritePattern
    : public mlir::OpRewritePattern<ReussirStrSelectOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirStrSelectOp op,
                  mlir::PatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto funcName = emitDecisionFunction(module, rewriter, op.getPatterns());
    auto func = module.lookupSymbol<mlir::func::FuncOp>(funcName);
    auto call = rewriter.create<mlir::func::CallOp>(
        op.getLoc(), func, mlir::ValueRange{op.getStr()});

    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};
struct ReussirStrStartWithOpRewritePattern
    : public mlir::OpRewritePattern<ReussirStrStartWithOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirStrStartWithOp op,
                  mlir::PatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();
    auto indexType = rewriter.getIndexType();

    // Get string length
    auto lenOp =
        rewriter.create<reussir::ReussirStrLenOp>(loc, indexType, op.getStr());

    // Get prefix length
    size_t prefixLen = op.getPrefix().size();
    auto prefixLenVal =
        rewriter.create<mlir::arith::ConstantIndexOp>(loc, prefixLen);

    // Check if len >= prefixLen
    auto isSufficientLen = rewriter.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::uge, lenOp.getResult(), prefixLenVal);

    auto resultType = op.getResult().getType();
    auto ifOp = rewriter.create<mlir::scf::IfOp>(
        loc, resultType, isSufficientLen, /*addThenRegion=*/true,
        /*addElseRegion=*/true);

    // Then region: Unsafe check
    {
      auto &thenBlock = ifOp.getThenRegion().front();
      rewriter.setInsertionPointToStart(&thenBlock);
      auto unsafeCheck = rewriter.create<reussir::ReussirStrUnsafeStartWithOp>(
          loc, resultType, op.getStr(), op.getPrefixAttr());
      rewriter.create<mlir::scf::YieldOp>(loc, unsafeCheck.getResult());
    }

    // Else region: Return false
    {
      auto &elseBlock = ifOp.getElseRegion().front();
      rewriter.setInsertionPointToStart(&elseBlock);
      auto falseVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
      rewriter.create<mlir::scf::YieldOp>(loc, falseVal->getResult(0));
    }

    rewriter.replaceOp(op, ifOp.getResult(0));
    return mlir::success();
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// SCFOpsLoweringPass
//===----------------------------------------------------------------------===//

namespace {
struct SCFOpsLoweringPass
    : public impl::ReussirSCFOpsLoweringPassBase<SCFOpsLoweringPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());

    populateSCFOpsLoweringConversionPatterns(patterns);

    // Configure target legality
    target.addLegalDialect<mlir::arith::ArithDialect, mlir::scf::SCFDialect,
                           mlir::math::MathDialect, mlir::func::FuncDialect,
                           mlir::ub::UBDialect, reussir::ReussirDialect>();

    // Illegal operations
    target.addIllegalOp<ReussirNullableDispatchOp, ReussirRecordDispatchOp,
                        ReussirScfYieldOp, ReussirClosureUniqifyOp,
                        ReussirTokenEnsureOp, ReussirStrByteAtOp,
                        ReussirStrSelectOp, ReussirStrStartWithOp>();

    if (failed(applyPartialConversion(getOperation(), target,
                                      std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateSCFOpsLoweringConversionPatterns(
    mlir::RewritePatternSet &patterns) {
  // Add conversion patterns for Reussir SCF operations
  patterns
      .add<ReussirNullableDispatchOpRewritePattern,
           ReussirRecordDispatchOpRewritePattern,
           ReussirClosureUniqifyOpRewritePattern,
           ReussirScfYieldOpRewritePattern, ReussirTokenEnsureOpRewritePattern,
           ReussirStrByteAtOpRewritePattern, ReussirStrSelectOpRewritePattern,
           ReussirStrStartWithOpRewritePattern>(patterns.getContext());
}

} // namespace reussir
