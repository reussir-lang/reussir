//===-- TokenReuse.cpp - Reussir token reuse pass impl ----------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 With LLVM Exceptions OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/TokenReuse.h"
#include "Reussir/Analysis/AliasAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirInterfaces.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "immer/lock/no_lock_policy.hpp"
#include "immer/memory_policy.hpp"
#include "llvm/Support/raw_ostream.h"

#include <bit>
#include <cstddef>
#include <functional>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#include <immer/set.hpp>
#pragma GCC diagnostic pop
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/xxhash.h>
#include <mlir/IR/Dominance.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/IR/Value.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Pass/Pass.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRTOKENREUSEPASS
#include "Reussir/Conversion/Passes.h.inc"

// This Pass implements token reuse optimization for Reussir dialect.
// This is currently a one-shot token assignment pass that does not rely on
// iterative data-flow analysis.
// Assumptions and Current Limitations:
// - IR passed in this pass is still in structured control flow form.
// - Currently, if there is exceptional control flow or loop, we do not proceed
//   the analysis. Before entering such region, we free all pending tokens and
//   give up the optimization.
// - This pass assumes all token producer operations (basically drop operations)
//   are inserted already.
// - IPA/IPO is not performed, so we give up optimization across function.
//   + if target function is marked with "leaf/no_alloc", we consider keeping
//     tokens across function call.
//   + otherwise, we free all pending tokens before function call to avoid
//     non-deterministic heap growth.
// - We do not go into regions that are `IsolatedFromAbove`, e.g., region of
//   `reussir.closure.create` operation. So, for closure to work properly,
//   the closure must the outlined first. We `signalPassFailure` if we encounter
//   closure operation with inlined region.
// This pass works as the follows:
// 1. We start with an empty set of available tokens. We use immer's persistent
//    data structure to reduce the overhead of set maintenance.
// 2. We traverse the IR following the DAG, maintaining the fact that available
//    tokens always dominate the current position. So, on exiting a region (via
//    `Terminator`), we return a set of token that are still available intersect
//    with the token that dominates the region parent.
// 3. Encountering a token producer operation (e.g., `reussir.token.dec`), we
//    add it into the pool if the token has no user. (This makes the analysis
//    compatible with pre-deallocated tokens in special cases.)
// 4. Encountering a token consumer operation (e.g., `reussir.token.inc`), we
//    use heuristic to determine the best token to use from the available pool.
//    Remove the selected token from the pool. Currently, we just iterate all
//    the tokens to find the most suitable one.
// 5. At `CallOpInterface` or `LoopLikeOpInterface`, we free all pending tokens.
//    (TODO: Exceptional/Early-Exit Regions do not exist in MLIR yet but
//    maintainers are actively working on it. We need to monitor the progress.)
// 6. At `RegionBranchInterface`, we continue on each nested branch and
//    intersect the result from each of them as remaining available tokens.
//
// The above process only describes how we maintain the available token set.
// Each time the token set change, we actually need to emit corresponding
// operations.
// 1. If we assign a token to a consumer operation, we need to insert:
//    - a `reussir.token.ensure` operation if token is nullable and the original
//      layout is compatible with target layout.
//    - a `reussir.token.realloc` operation if the original layout is not
//      compatible with target layout. (`realloc` operation also supports
//      nullable token.)
// 2. At terminator, if token befores unusable at parent, either because of
//    dominance or because token is consumed in other branches, we need to
//    insert `reussir.token.free` operation to free the token.
// 3. Similarly, at function call or loop, we need to free all pending tokens
//    with `reussir.token.free` operation.
// The change is applied lazily. During the first traversal, we maintain a
// vector of operations where such changes are needed.

namespace {
static inline constexpr size_t MIN_ALLOC_STEP_SIZE = 2 * sizeof(void *);
static inline constexpr size_t MIN_ALLOC_STEP_BITS =
    std::countr_zero(MIN_ALLOC_STEP_SIZE);
static inline constexpr size_t INTERMEDIATE_BITS = 2;
size_t toExpMand(size_t value) {
  auto oneAtBit = [](size_t bit) { return 1 << bit; };
  constexpr size_t LEADING_BIT =
      oneAtBit(INTERMEDIATE_BITS + MIN_ALLOC_STEP_BITS) >> 1;
  constexpr size_t MANTISSA_MASK = oneAtBit(INTERMEDIATE_BITS) - 1;
  constexpr size_t BITS = sizeof(size_t) * CHAR_BIT;

  value = value - 1;

  size_t e = BITS - INTERMEDIATE_BITS - MIN_ALLOC_STEP_BITS -
             __builtin_clz(value | LEADING_BIT);
  size_t b = (e == 0) ? 0 : 1;
  size_t m = (value >> (MIN_ALLOC_STEP_BITS + e - b)) & MANTISSA_MASK;

  return (e << INTERMEDIATE_BITS) + m;
}
// Guess if two tokens are in the same size class.
// Computation logic is from SnMalloc, may not apply to other allocators.
bool possiblyInplaceReallocable(size_t oldAlign, size_t oldSize,
                                size_t newAlign, size_t newSize) {
  if (oldAlign != newAlign)
    return false;
  auto alignedSize = [](size_t alignment, size_t size) {
    return ((alignment - 1) | (size - 1)) + 1;
  };
  // Do not attempt reuse if data is likely managed via superslab.
  constexpr size_t GB = 1024 * 1024 * 1024;
  auto oldAlignedSize = alignedSize(oldAlign, oldSize);
  auto newAlignedSize = alignedSize(newAlign, newSize);
  if (oldAlignedSize >= GB || newAlignedSize >= GB)
    return false;
  auto oldExpMand = toExpMand(oldAlignedSize >> MIN_ALLOC_STEP_BITS);
  auto newExpMand = toExpMand(newAlignedSize >> MIN_ALLOC_STEP_BITS);
  return newExpMand == oldExpMand;
}
// heuristic < 0 : do not reuse at all
// heuristic == 0: can reuse via realloc
// heuristic > 0 : can reuse via ensure, larger is better
// TODO: consider appreantly non-exclusive cases.
int hueristic(TokenProducer producer, TokenAcceptor consumer,
              mlir::AliasAnalysis &aliasAnalyzer) {
  // Under perfect match, we measure the locality score.
  if (producer.getTokenType() == consumer.getTokenType()) {
    ReussirRcDecOp dec = dyn_cast<ReussirRcDecOp>(producer.getOperation());
    ReussirRcCreateOp create =
        dyn_cast<ReussirRcCreateOp>(consumer.getOperation());
    int localityScore = 1;
    // First, we do a very coarse grained copy avoidance analysis.
    if (dec && create &&
        mlir::isa<RecordType>(create.getRcPtr().getType().getElementType())) {
      // Check if any record field is assembled from a projection whose source
      // is aliased with the producer.
      mlir::ValueRange fields{};
      mlir::Operation *op = create.getRcPtr().getDefiningOp();
      while (auto variant =
                 llvm::dyn_cast_if_present<ReussirRecordVariantOp>(op))
        op = variant.getValue().getDefiningOp();
      if (auto compound = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(
              create.getRcPtr().getDefiningOp()))
        fields = compound.getFields();
      for (mlir::Value field : fields) {
        if (auto loaded = llvm::dyn_cast_if_present<ReussirRefLoadOp>(
                field.getDefiningOp())) {
          mlir::Value root = loaded.getRef();
          while (true) {
            if (auto projection =
                    llvm::dyn_cast<ReussirRefProjectOp>(root.getDefiningOp())) {
              root = projection.getRef();
              continue;
            }
            if (auto blkArg = llvm::dyn_cast<mlir::BlockArgument>(root)) {
              if (auto dispatch =
                      llvm::dyn_cast_if_present<ReussirRecordDispatchOp>(
                          blkArg.getOwner()->getParentOp())) {
                root = dispatch.getValue();
                continue;
              }
            }
            break;
          }
          if (auto borrow = llvm::dyn_cast_if_present<ReussirRcBorrowOp>(
                  root.getDefiningOp()))
            localityScore +=
                aliasAnalyzer.alias(borrow.getRcPtr(), dec.getRcPtr()) ==
                mlir::AliasResult::MustAlias;
        }
      }
    }
    return localityScore;
  }
  TokenType producerType = producer.getTokenType();
  TokenType consumerType = consumer.getTokenType();
  size_t oldSize = producerType.getSize();
  size_t newSize = consumerType.getSize();
  size_t oldAlign = producerType.getAlign();
  size_t newAlign = consumerType.getAlign();
  return possiblyInplaceReallocable(oldAlign, oldSize, newAlign, newSize) ? 0
                                                                          : -1;
}

struct ValueHash {
  uint64_t operator()(mlir::Value v) const {
    void *ptr = v.getAsOpaquePointer();
    auto bytes = std::bit_cast<std::array<uint8_t, sizeof(void *)>>(ptr);
    return llvm::xxHash64(bytes);
  }
};

using UnsyncPolicy =
    immer::memory_policy<immer::default_heap_policy,
                         immer::unsafe_refcount_policy, immer::no_lock_policy>;

using ValueSet = immer::set<mlir::Value, ValueHash, std::equal_to<mlir::Value>,
                            UnsyncPolicy>;

ValueSet intersect(const ValueSet &lhs, const ValueSet &rhs) {
  if (lhs.empty())
    return lhs;
  if (rhs.empty())
    return rhs;

  ValueSet res = lhs;
  for (auto val : lhs)
    if (!rhs.count(val))
      res = res.erase(val);
  return res;
}

struct Reuse {
  mlir::Value token;
  bool realloc;
  TokenAcceptor anchor;
};
struct Free {
  mlir::Value token;
  mlir::Operation *anchor;
};
struct TokenReusePass : public impl::ReussirTokenReusePassBase<TokenReusePass> {
  using Base::Base;
  ValueSet oneShotTokenReuse(mlir::Region &region, ValueSet availableTokens,
                             llvm::SmallVectorImpl<Reuse> &reuses,
                             llvm::SmallVectorImpl<Free> &frees,
                             mlir::AliasAnalysis &aliasAnalyzer,
                             mlir::DominanceInfo &domInfo) {
    if (region.empty())
      return availableTokens;

    if (!region.hasOneBlock()) {
      region.getParentOp()->emitOpError()
          << "Token reuse pass only supports single block regions (SCF)";
      signalPassFailure();
      return {};
    }

    for (auto &op : region.front()) {
      if (isa<mlir::CallOpInterface>(op) ||
          isa<mlir::LoopLikeOpInterface>(op)) {
        for (auto token : availableTokens)
          frees.push_back({token, &op});
        availableTokens = {};
        for (auto &nestedRegion : op.getRegions())
          oneShotTokenReuse(nestedRegion, {}, reuses, frees, aliasAnalyzer,
                            domInfo);
      } else if (auto branchOp = dyn_cast<mlir::RegionBranchOpInterface>(op)) {
        llvm::SmallVector<ValueSet> branchResults;
        for (auto &nestedRegion : op.getRegions())
          branchResults.push_back(
              oneShotTokenReuse(nestedRegion, availableTokens, reuses, frees,
                                aliasAnalyzer, domInfo));
        if (branchResults.empty()) {
          llvm::errs() << "[WARN] RegionBranch with no regions?\n";
        } else {
          // effective intersect with available token at parent
          // this rule out inner-scope created tokens from escaping parent
          // scope.
          ValueSet intersection = availableTokens;
          for (size_t i = 0; i < branchResults.size(); ++i)
            intersection = intersect(intersection, branchResults[i]);

          for (size_t i = 0; i < branchResults.size(); ++i) {
            for (auto val : branchResults[i]) {
              if (!intersection.count(val)) {
                mlir::Block &block = op.getRegion(i).front();
                frees.push_back({val, block.getTerminator()});
              }
            }
          }
          availableTokens = intersection;
        }
      }

      if (auto producer = dyn_cast<TokenProducer>(op)) {
        if (producer.shouldProduceToken()) {
          mlir::Value token = producer.getProducedValue();
          if (!token) {
            producer->emitOpError() << " should have produced a token before "
                                       "passing into reuse analysis";
            signalPassFailure();
            return {};
          }
          if (token.use_empty())
            availableTokens = availableTokens.insert(token);
        }
      }

      if (auto acceptor = dyn_cast<TokenAcceptor>(op)) {
        auto allocOp = llvm::dyn_cast_if_present<ReussirTokenAllocOp>(
            acceptor.getToken().getDefiningOp());
        if (allocOp && allocOp.getToken().hasOneUse()) {
          int bestScore = -1;
          mlir::Value bestToken{};
          bool bestRealloc = false;

          for (auto tokenVal : availableTokens) {
            if (auto producer =
                    dyn_cast_or_null<TokenProducer>(tokenVal.getDefiningOp())) {
              int score = hueristic(producer, acceptor, aliasAnalyzer);
              if (score >= 0 && score > bestScore) {
                bestScore = score;
                bestToken = tokenVal;
                bestRealloc = (score == 0);
              }
            }
          }

          if (bestToken) {
            mlir::Value selectedToken = bestToken;
            availableTokens = availableTokens.erase(bestToken);
            reuses.push_back({selectedToken, bestRealloc, acceptor});
          }
        }
        // ReussirClosureCreateOp is a kind of acceptor.
        if (auto closure = dyn_cast<ReussirClosureCreateOp>(op)) {
          if (closure.isInlined()) {
            closure->emitOpError()
                << " with inlined region found in token reuse pass";
            signalPassFailure();
            return {};
          }
        }
      }
    }

    mlir::Operation *terminator = region.front().getTerminator();
    for (auto token : availableTokens) {
      if (!region.getParentOp() ||
          !domInfo.properlyDominates(token, region.getParentOp())) {
        frees.push_back({token, terminator});
        availableTokens = availableTokens.erase(token);
      }
    }
    return availableTokens;
  }

  void runOnOperation() override {
    llvm::SmallVector<Reuse> reuses;
    llvm::SmallVector<Free> frees;
    mlir::AliasAnalysis aliasAnalyzer(getOperation());
    reussir::registerAliasAnalysisImplementations(aliasAnalyzer);
    mlir::DominanceInfo domInfo(getOperation());

    for (auto &region : getOperation()->getRegions()) {
      oneShotTokenReuse(region, {}, reuses, frees, aliasAnalyzer, domInfo);
    }

    mlir::IRRewriter rewriter(getOperation());

    for (auto &reuse : reuses) {
      rewriter.setInsertionPoint(reuse.anchor);
      TokenType targetType = reuse.anchor.getTokenType();

      mlir::Value newToken;
      mlir::Value oldToken = reuse.anchor.getToken();
      if (reuse.realloc)
        newToken = rewriter.create<ReussirTokenReallocOp>(
            reuse.anchor->getLoc(), targetType, reuse.token);
      else
        newToken = rewriter.create<ReussirTokenEnsureOp>(
            reuse.anchor->getLoc(), targetType, reuse.token);
      reuse.anchor.assignToken(newToken);
      auto allocOp = llvm::cast<ReussirTokenAllocOp>(oldToken.getDefiningOp());
      rewriter.eraseOp(allocOp);
    }

    for (const auto &free : frees) {
      rewriter.setInsertionPoint(free.anchor);
      rewriter.create<ReussirTokenFreeOp>(free.anchor->getLoc(), free.token);
    }
  }
};
} // namespace
} // namespace reussir
