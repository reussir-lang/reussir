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
/// Beta-reduce closure application chains. Runs right after the inliner (so
/// cross-function create→eval pairs are visible) and before token
/// instantiation / closure outlining (so `closure.create` bodies are still
/// inline regions and carry no materialized token yet).
///
/// Three one-step local patterns under the greedy driver; their fixpoint
/// walks each eval's chain without any hand-driven recursion:
///
///  * FOLD: `eval(apply(a, x), pack)` with a single-use apply becomes
///    `eval(x, [a] ++ pack)` — the argument prepends, so the pack stays in
///    application order. Dominance is free (def-use edge), and ownership is
///    untouched: the fused eval consumes closure and pack exactly as the
///    apply chain did.
///
///  * STRIP: a single-use `closure.uniqify` under a fused eval is erased
///    when it is a provable runtime no-op — its operand is a single-use
///    `closure.apply` or `closure.create`, i.e. a value that is unique by
///    construction (a fresh box, or an in-place application to a value the
///    chain's genuine guard already made unique). The bottom-most uniqify,
///    whose operand is anything else, never matches: it stays in the IR as
///    the fused eval's operand — the fused form carries `apply`'s contract
///    (uniqueness is the producer's obligation) and never re-establishes it.
///
///  * INLINE: `eval(create{body}, pack)` over a single-use inlined create is
///    a visible beta redex: the body block is spliced in front of the eval
///    with the pack substituted for its parameters (legal anywhere — the
///    region is IsolatedFromAbove), and the chain, the create, and its dead
///    `token.alloc` are erased. Evals spliced out of the body are re-queued
///    by the driver, which is what makes the reduction recursive.
///
///  * STRUCTURALINLINE: a redex carried into a supported structured region,
///    in its general frontend shape —
///
///      %c = create {body}; (uniqify; apply cap)*     // capture binding
///      structural* {                                 // LoopLike or cell.rmw
///        rc.inc %c; (uniqify; apply x_i)*; eval(...) // per-execution chain
///      }
///      rc.dec %c
///
///    The body is spliced into the innermost region at the eval and the closure
///    box vanishes: its create / per-execution inc+chain / trailing dec traffic
///    is self-contained (a fresh box nothing else can observe), so erasing all
///    of it is count-neutral. The per-execution uniqify is a *genuine* guard —
///    the carried box is shared, so it clones every execution — but the clone
///    is itself a fresh box the chain consumes whole: cloning inc'd the stored
///    captures out
///    and the eval consumed them, a wash the fused form reproduces. Capture
///    counts are preserved, not reasoned about: each rc-typed capture bound
///    *outside* the structure gains a per-execution `rc.inc` at the splice
///    point (the inlined body consumes its capture parameters each execution)
///    and one `rc.dec` where the box's dec stood (the box's drop used to
///    release the ref the apply consumed). Captures bound *inside* the
///    structure were consumed once per execution before and still are — they
///    just take their seat in the substitution pack.
///
///  * SINK and DROP: a fresh chain consumed in the arms of a branching
///    operation moves into the one arm that does more than release it; a
///    release of a fresh chain becomes the release of its captures. Both
///    expose the redexes an inlined `match` leaves behind.
///
///  * SPECIALIZE: after the patterns reach a fixpoint, a closure literal
///    passed to a function that only retains, releases, evaluates, or
///    forwards it to its own recursive calls goes to a copy of the callee
///    that takes the literal's captures instead; the patterns then run again
///    over the spliced bodies.
///
/// Fused evals that bottom out anywhere else survive the pass; the SCF-ops
/// lowering re-expands them into unchecked applies + plain eval, so the net
/// effect of FOLD+STRIP on a chain the inliner merged is the removal of its
/// intermediate uniqueness checks.
///
//===----------------------------------------------------------------------===//

#include "Reussir/IR/Coverage.h"
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Casting.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/IRMapping.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Interfaces/LoopLikeInterface.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/Transformation/Passes.h"
#include "Reussir/Transformation/Specialization.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRCLOSUREBETAREDUCTIONPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

// FOLD: eval(apply(a, x), pack) → eval(x, [a] ++ pack).
struct FoldApplyIntoEvalPattern
    : public mlir::OpRewritePattern<ReussirClosureEvalOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureEvalOp eval,
                  mlir::PatternRewriter &rewriter) const override {
    auto apply = eval.getClosure().getDefiningOp<ReussirClosureApplyOp>();
    if (!apply || !apply.getApplied().hasOneUse())
      return mlir::failure();
    llvm::SmallVector<mlir::Value> args;
    args.push_back(apply.getArg());
    llvm::append_range(args, eval.getArgs());
    auto fused = ReussirClosureEvalOp::create(
        rewriter, eval.getLoc(),
        eval.getNumResults() ? eval.getResult().getType() : mlir::Type(),
        apply.getClosure(), args);
    rewriter.replaceOp(eval, fused->getResults());
    rewriter.eraseOp(apply);
    return mlir::success();
  }
};

// STRIP: eval(uniqify(x), pack) → eval(x, pack) when the uniqify is a
// provable runtime no-op (x is a single-use apply/create link, unique by
// construction). A plain eval paired with its frontend-emitted uniqify over
// anything else is left byte-identical.
struct StripNoopUniqifyPattern
    : public mlir::OpRewritePattern<ReussirClosureEvalOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureEvalOp eval,
                  mlir::PatternRewriter &rewriter) const override {
    auto uniqify = eval.getClosure().getDefiningOp<ReussirClosureUniqifyOp>();
    if (!uniqify || !uniqify.getUniqified().hasOneUse())
      return mlir::failure();
    mlir::Value below = uniqify.getClosure();
    mlir::Operation *belowDef = below.getDefiningOp();
    if (!below.hasOneUse() || !belowDef ||
        !llvm::isa<ReussirClosureApplyOp, ReussirClosureCreateOp>(belowDef))
      return mlir::failure();
    rewriter.replaceOp(uniqify, below);
    return mlir::success();
  }
};

// INLINE: eval(create{body}, pack) → the body, pack substituted for its
// parameters.
struct InlineCreateIntoEvalPattern
    : public mlir::OpRewritePattern<ReussirClosureEvalOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureEvalOp eval,
                  mlir::PatternRewriter &rewriter) const override {
    auto create = eval.getClosure().getDefiningOp<ReussirClosureCreateOp>();
    if (!create || !sameCoverageScope(create, eval) ||
        !create.getClosure().hasOneUse() || !create.isInlined())
      return mlir::failure();
    // A token producer other than a plain `token.alloc` (impossible today —
    // token reuse runs later — but cheap to guard) would leak if orphaned.
    mlir::Value token = create.getToken();
    if (token && !token.getDefiningOp<ReussirTokenAllocOp>())
      return mlir::failure();

    mlir::Block &body = create.getBody().front();
    assert(body.getNumArguments() == eval.getArgs().size() &&
           "eval pack must cover the create's full signature");
    auto yield = llvm::cast<ReussirClosureYieldOp>(body.getTerminator());
    rewriter.inlineBlockBefore(&body, eval, eval.getArgs());
    // Read the operand after inlining, once block arguments have been replaced
    // with the eval pack and before the moved terminator is erased.
    mlir::Value yielded = yield.getValue();
    rewriter.eraseOp(yield);
    if (eval.getNumResults())
      rewriter.replaceOp(eval, yielded);
    else
      rewriter.eraseOp(eval);
    rewriter.eraseOp(create);
    if (token && token.use_empty())
      rewriter.eraseOp(token.getDefiningOp());
    return mlir::success();
  }
};

// A fresh closure chain `create; (uniqify | apply)*` ending at `top`: every
// link below `top` is single-use and in `top`'s block, and the create is
// inlined with at most a plain `token.alloc`. Fills `chain` bottom-up and
// `caps` with the applied arguments in application order.
ReussirClosureCreateOp
matchFreshChain(mlir::Value top,
                llvm::SmallVectorImpl<mlir::Operation *> &chain,
                llvm::SmallVectorImpl<mlir::Value> &caps) {
  mlir::Operation *topDef = top.getDefiningOp();
  if (!topDef)
    return {};
  for (mlir::Value link = top;;) {
    mlir::Operation *def = link.getDefiningOp();
    if (!def || def->getBlock() != topDef->getBlock() ||
        (link != top && !link.hasOneUse()))
      return {};
    chain.push_back(def);
    if (auto apply = llvm::dyn_cast<ReussirClosureApplyOp>(def)) {
      caps.push_back(apply.getArg());
      link = apply.getClosure();
    } else if (auto uniqify = llvm::dyn_cast<ReussirClosureUniqifyOp>(def)) {
      link = uniqify.getClosure();
    } else if (auto create = llvm::dyn_cast<ReussirClosureCreateOp>(def)) {
      mlir::Value token = create.getToken();
      if (!create.isInlined() ||
          (token && !token.getDefiningOp<ReussirTokenAllocOp>()))
        return {};
      std::reverse(chain.begin(), chain.end());
      std::reverse(caps.begin(), caps.end());
      return create;
    } else {
      return {};
    }
  }
}

// Whether dropping the box can be replaced by releasing its captures one by
// one: each is rc-typed or needs no release at all.
bool capturesReleasable(llvm::ArrayRef<mlir::Value> caps) {
  return llvm::all_of(caps, [](mlir::Value cap) {
    return llvm::isa<RcType>(cap.getType()) ||
           isTriviallyCopyable(cap.getType());
  });
}

// Releases the captures in payload order at the insertion point, in place of
// `boxRelease`, as the box's drop would. Each release keeps the box release's
// cleanup hooks, a superset of the hooks reachable through one capture.
void releaseCaptures(mlir::OpBuilder &rewriter, ReussirRcDecOp boxRelease,
                     llvm::ArrayRef<mlir::Value> caps) {
  for (mlir::Value cap : caps)
    if (llvm::isa<RcType>(cap.getType()))
      ReussirRcDecOp::create(rewriter, boxRelease.getLoc(),
                             /*nullableToken=*/NullableType{}, cap,
                             /*destructureTag=*/mlir::IntegerAttr{},
                             /*boundMembers=*/mlir::DenseI64ArrayAttr{},
                             boxRelease.getCleanupAttr());
}

bool isPlainRelease(mlir::Operation *op) {
  auto dec = llvm::dyn_cast<ReussirRcDecOp>(op);
  return dec && !isUnsafeRcOperation(dec) && dec->use_empty() &&
         dec.getNumResults() == 0 && !dec.isDestructuring();
}

// Erases a fresh chain (bottom-up order in `chain`) and the create's dead
// token.
void eraseFreshChain(mlir::PatternRewriter &rewriter,
                     llvm::ArrayRef<mlir::Operation *> chain) {
  auto create = llvm::cast<ReussirClosureCreateOp>(chain.front());
  mlir::Value token = create.getToken();
  for (mlir::Operation *op : llvm::reverse(chain))
    rewriter.eraseOp(op);
  if (token && token.use_empty())
    rewriter.eraseOp(token.getDefiningOp());
}

// DROP: rc.dec(fresh chain) → release the applied captures. The box was
// never evaluated, so its only effect is dropping what the applies stored.
struct DropFreshClosurePattern : public mlir::OpRewritePattern<ReussirRcDecOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcDecOp dec,
                  mlir::PatternRewriter &rewriter) const override {
    mlir::Value top = dec.getRcPtr();
    if (!isPlainRelease(dec) || !top.hasOneUse())
      return mlir::failure();
    llvm::SmallVector<mlir::Operation *> chain;
    llvm::SmallVector<mlir::Value> caps;
    if (!matchFreshChain(top, chain, caps) || !capturesReleasable(caps))
      return mlir::failure();
    rewriter.setInsertionPoint(dec);
    releaseCaptures(rewriter, dec, caps);
    rewriter.eraseOp(dec);
    eraseFreshChain(rewriter, chain);
    return mlir::success();
  }
};

// SINK: a fresh chain consumed inside the arms of one branching operation,
// e.g. the inlined `match self { Pass => next(), Fail(_) => self }`:
//
//   %c = create {body}; apply cap
//   dispatch { [0]: eval(uniqify %c)   [1]: rc.dec %c }
//
// moves into the one arm that does more than release it, and each release
// in the other arms becomes the release of the captures. The arm then holds
// a single-use chain that FOLD/STRIP/INLINE or DROP reduce. The body moves
// rather than being copied, so code size does not grow.
struct SinkFreshClosurePattern
    : public mlir::OpRewritePattern<ReussirClosureCreateOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureCreateOp create,
                  mlir::PatternRewriter &rewriter) const override {
    // Climb to the end of the single-use chain.
    mlir::Value top = create.getClosure();
    while (top.hasOneUse()) {
      mlir::Operation *user = *top.getUsers().begin();
      if (user->getBlock() != create->getBlock() ||
          !llvm::isa<ReussirClosureApplyOp, ReussirClosureUniqifyOp>(user) ||
          user->getOperand(user->getNumOperands() - 1) != top)
        break;
      top = user->getResult(0);
    }
    llvm::SmallVector<mlir::Operation *> chain;
    llvm::SmallVector<mlir::Value> caps;
    if (top.use_empty() || !matchFreshChain(top, chain, caps))
      return mlir::failure();

    // Every use sits strictly inside one branching op of the chain's block
    // whose regions run at most once.
    mlir::Block *block = create->getBlock();
    mlir::Operation *branch =
        block->findAncestorOpInBlock(**top.getUsers().begin());
    auto regionBranch =
        llvm::dyn_cast_or_null<mlir::RegionBranchOpInterface>(branch);
    if (!regionBranch || llvm::isa<mlir::LoopLikeOpInterface>(branch))
      return mlir::failure();
    for (mlir::Region &region : branch->getRegions())
      if (regionBranch.isRepetitiveRegion(region.getRegionNumber()) ||
          !(region.empty() || region.hasOneBlock()))
        return mlir::failure();

    mlir::Region *home = nullptr;
    llvm::SmallVector<mlir::Operation *> releases;
    for (mlir::Operation *user : top.getUsers()) {
      if (user == branch || block->findAncestorOpInBlock(*user) != branch)
        return mlir::failure();
      mlir::Region *region = user->getParentRegion();
      while (region->getParentOp() != branch)
        region = region->getParentOp()->getParentRegion();
      if (isPlainRelease(user) && user->getParentRegion() == region) {
        releases.push_back(user);
        continue;
      }
      if (home && home != region)
        return mlir::failure();
      home = region;
    }
    if (!home)
      home = releases.front()->getParentRegion();
    llvm::erase_if(releases, [&](mlir::Operation *release) {
      return release->getParentRegion() == home;
    });
    if (!releases.empty() && !capturesReleasable(caps))
      return mlir::failure();

    for (mlir::Operation *release : releases) {
      rewriter.setInsertionPoint(release);
      releaseCaptures(rewriter, llvm::cast<ReussirRcDecOp>(release), caps);
      rewriter.eraseOp(release);
    }
    // Insert before the first operation of the home block that uses the
    // chain, possibly through nested regions.
    mlir::Block &homeBlock = home->front();
    mlir::Operation *anchor = nullptr;
    for (mlir::Operation *user : top.getUsers()) {
      mlir::Operation *ancestor = homeBlock.findAncestorOpInBlock(*user);
      if (!anchor || ancestor->isBeforeInBlock(anchor))
        anchor = ancestor;
    }
    mlir::Value token = create.getToken();
    if (token)
      rewriter.moveOpBefore(token.getDefiningOp(), anchor);
    for (mlir::Operation *op : chain)
      rewriter.moveOpBefore(op, anchor);
    return mlir::success();
  }
};

// STRUCTURALINLINE: beta-reduce a redex carried through a nest consisting
// only of LoopLike operations and cell.rmw. See the file comment for the
// ownership accounting. This intentionally does not accept arbitrary
// region-holding operations.
mlir::LogicalResult structuralBetaReduction(ReussirClosureEvalOp eval,
                                            mlir::PatternRewriter &rewriter) {
  // Peel the per-execution links: single-use `uniqify | apply` ops in the
  // eval's own block, down to the value carried across the region boundary. The
  // uniqify here is a *genuine* guard (the carried box is shared, so it
  // clones every execution) — but the clone is a fresh box the fused
  // chain consumes whole, so erasing it is count-neutral; cloning used to
  // inc the stored captures out and the eval consumed them, a wash the
  // fused form reproduces exactly. Per-execution applied args were
  // consumed once per execution before and still are: they need no
  // adjustment, only a seat in the substitution pack.
  llvm::SmallVector<mlir::Operation *> localChain;
  llvm::SmallVector<mlir::Value> localCaps; // collected top-down
  mlir::Value carried = eval.getClosure();
  while (true) {
    mlir::Operation *def = carried.getDefiningOp();
    if (!def)
      return mlir::failure();
    if (def->getBlock() != eval->getBlock())
      break; // `carried` crosses the innermost structural boundary
    if (!carried.hasOneUse())
      return mlir::failure();
    if (auto apply = llvm::dyn_cast<ReussirClosureApplyOp>(def)) {
      localChain.push_back(def);
      localCaps.push_back(apply.getArg());
      carried = apply.getClosure();
    } else if (auto uniqify = llvm::dyn_cast<ReussirClosureUniqifyOp>(def)) {
      localChain.push_back(def);
      carried = uniqify.getClosure();
    } else {
      return mlir::failure();
    }
  }
  mlir::Operation *top = carried.getDefiningOp();
  mlir::Block *defBlock = top->getBlock();

  // Every region between the carried value and the eval must be one of the
  // deliberately supported structural operations: a LoopLike operation or
  // cell.rmw. The chain must dominate the complete nest from outside.
  mlir::Operation *walk = eval;
  while (walk->getBlock() != defBlock) {
    mlir::Operation *parent = walk->getParentOp();
    if (!parent ||
        !llvm::isa<mlir::LoopLikeOpInterface, ReussirCellRmwOp>(parent))
      return mlir::failure();
    walk = parent;
  }
  mlir::Operation *outermostStructure = walk;

  // The chain below the structurally carried value: linear (every local link
  // single-use), one block, bottoming at an inlined create. Every uniqify
  // over such a chain is a runtime no-op — the value is unique by
  // construction, fresh from the create with no use between the links.
  llvm::SmallVector<mlir::Operation *> outerChain;
  llvm::SmallVector<mlir::Value> outerCaps; // collected top-down
  ReussirClosureCreateOp create;
  mlir::Value link = carried;
  while (true) {
    mlir::Operation *def = link.getDefiningOp();
    if (!def || def->getBlock() != defBlock)
      return mlir::failure();
    if (link != carried && !link.hasOneUse())
      return mlir::failure();
    if (auto apply = llvm::dyn_cast<ReussirClosureApplyOp>(def)) {
      outerChain.push_back(def);
      outerCaps.push_back(apply.getArg());
      link = apply.getClosure();
    } else if (auto uniqify = llvm::dyn_cast<ReussirClosureUniqifyOp>(def)) {
      outerChain.push_back(def);
      link = uniqify.getClosure();
    } else if ((create = llvm::dyn_cast<ReussirClosureCreateOp>(def))) {
      outerChain.push_back(def);
      break;
    } else {
      return mlir::failure();
    }
  }
  if (!create.isInlined())
    return mlir::failure();
  mlir::Value token = create.getToken();
  if (token && !token.getDefiningOp<ReussirTokenAllocOp>())
    return mlir::failure();
  // Walking top-down visits the last application first; the body's
  // parameters bind in application order — outer applies first, then the
  // region-local ones, then the eval's pack.
  std::reverse(outerCaps.begin(), outerCaps.end());
  std::reverse(localCaps.begin(), localCaps.end());

  mlir::Block &body = create.getBody().front();
  if (body.getNumArguments() !=
      outerCaps.size() + localCaps.size() + eval.getArgs().size())
    return mlir::failure();

  // The carried value's remaining uses: exactly one per-execution inc
  // beside the eval, and exactly one plain dec after the nest at the
  // chain's level.
  mlir::Operation *firstLink =
      localChain.empty() ? eval.getOperation() : localChain.back();
  llvm::SmallVector<ReussirRcIncOp> incs;
  ReussirRcDecOp dec;
  for (mlir::Operation *user : carried.getUsers()) {
    if (user == firstLink)
      continue;
    if (auto inc = llvm::dyn_cast<ReussirRcIncOp>(user)) {
      if (isUnsafeRcOperation(inc) || !inc.isSingleAcquire() ||
          inc->getBlock() != eval->getBlock() || !inc->isBeforeInBlock(eval))
        return mlir::failure();
      incs.push_back(inc);
      continue;
    }
    if (auto d = llvm::dyn_cast<ReussirRcDecOp>(user)) {
      if (isUnsafeRcOperation(d) || dec || d->getBlock() != defBlock ||
          !outermostStructure->isBeforeInBlock(d))
        return mlir::failure();
      dec = d;
      continue;
    }
    return mlir::failure();
  }
  if (incs.size() != 1 || !dec || dec.getNumResults() != 0)
    return mlir::failure();

  if (!sameCoverageScope(create, eval))
    return mlir::failure();

  // Per-execution ownership for the rc-typed captures the box stored:
  // the inlined body consumes its capture parameters each execution (see
  // the file comment).
  rewriter.setInsertionPoint(eval);
  for (mlir::Value cap : outerCaps)
    if (llvm::isa<RcType>(cap.getType()))
      ReussirRcIncOp::create(rewriter, eval.getLoc(), cap);

  auto yield = llvm::cast<ReussirClosureYieldOp>(body.getTerminator());
  llvm::SmallVector<mlir::Value> args(outerCaps);
  llvm::append_range(args, localCaps);
  llvm::append_range(args, eval.getArgs());
  rewriter.inlineBlockBefore(&body, eval, args);
  // The yield may refer directly to a body argument; fetch it only after
  // inlining has replaced that argument with its value from `args`.
  mlir::Value yielded = yield.getValue();
  rewriter.eraseOp(yield);
  if (eval.getNumResults())
    rewriter.replaceOp(eval, yielded);
  else
    rewriter.eraseOp(eval);

  // The box's dec used to release the captures the outer applies
  // consumed; release them directly where it stood.
  rewriter.setInsertionPoint(dec);
  releaseCaptures(rewriter, dec, outerCaps);
  for (mlir::Operation *op : localChain)
    rewriter.eraseOp(op);
  for (ReussirRcIncOp inc : incs)
    rewriter.eraseOp(inc);
  rewriter.eraseOp(dec);
  for (mlir::Operation *op : outerChain)
    rewriter.eraseOp(op);
  if (token && token.use_empty())
    rewriter.eraseOp(token.getDefiningOp());
  return mlir::success();
}

struct InlineStructurallyCarriedCreatePattern
    : public mlir::OpRewritePattern<ReussirClosureEvalOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirClosureEvalOp eval,
                  mlir::PatternRewriter &rewriter) const override {
    return structuralBetaReduction(eval, rewriter);
  }
};

// SPECIALIZE: a closure literal passed to a function that only retains,
// releases, evaluates, or forwards it to its own recursive calls, e.g.
//
//   fn ins(t: Tree, k: i64, cmp: (i64, i64) -> i64) -> Tree {
//     ... cmp(k, kx) ... ins(l, k, cmp) ...
//   }
//   ins(t, k, |a, b| b - a)
//
// The call goes to a copy of the callee that takes the literal's captures in
// place of the closure. A reference to the box stands for one owned copy of
// every capture: retaining or releasing the parameter retains or releases
// the captures, an eval splices the literal's body with the captures
// prepended to its arguments, and a recursive call forwards the captures.
// The box is never allocated and its evaluations become direct code.

// Whether parameter `index` of `function` is used only as one immutable
// closure value: retained once, released, evaluated (directly or through a
// uniqify), or forwarded at the same position to a call of `function`.
bool isClosureLiteralParameter(mlir::func::FuncOp function, unsigned index) {
  mlir::BlockArgument param = function.getArgument(index);
  if (isBorrowedArgument(param))
    return false;
  return llvm::all_of(param.getUses(), [&](mlir::OpOperand &use) {
    mlir::Operation *user = use.getOwner();
    if (auto inc = llvm::dyn_cast<ReussirRcIncOp>(user))
      return !isUnsafeRcOperation(inc) && inc.isSingleAcquire();
    if (isPlainRelease(user))
      return true;
    if (llvm::isa<ReussirClosureEvalOp>(user))
      return use.getOperandNumber() == 0;
    if (auto uniqify = llvm::dyn_cast<ReussirClosureUniqifyOp>(user)) {
      mlir::Value unique = uniqify.getUniqified();
      if (!unique.hasOneUse())
        return false;
      auto eval = llvm::dyn_cast<ReussirClosureEvalOp>(*unique.user_begin());
      return eval && eval.getClosure() == unique;
    }
    if (auto call = llvm::dyn_cast<mlir::func::CallOp>(user))
      return call.getCallee() == function.getSymName() &&
             use.getOperandNumber() == index;
    return false;
  });
}

// Splices `create`'s body before the builder's insertion point with `args`
// substituted for its parameters; returns the yielded value, if any.
mlir::Value spliceBody(mlir::OpBuilder &builder, ReussirClosureCreateOp create,
                       mlir::ValueRange args) {
  mlir::Block &body = create.getBody().front();
  mlir::IRMapping mapping;
  mapping.map(body.getArguments(), args);
  for (mlir::Operation &op : body.without_terminator())
    builder.clone(op, mapping);
  auto yield = llvm::cast<ReussirClosureYieldOp>(body.getTerminator());
  return yield.getValue() ? mapping.lookupOrDefault(yield.getValue())
                          : mlir::Value();
}

llvm::SmallVector<mlir::Value> replaceOperand(mlir::OperandRange operands,
                                              unsigned index,
                                              mlir::ValueRange replacement) {
  llvm::SmallVector<mlir::Value> result(operands.take_front(index));
  llvm::append_range(result, replacement);
  llvm::append_range(result, operands.drop_front(index + 1));
  return result;
}

// Rewrites parameter `index` of `spec`, a copy of the callee, into the
// captures of `create`, whose types are `capTypes`.
void rewriteClosureParameter(mlir::func::FuncOp spec, unsigned index,
                             ReussirClosureCreateOp create,
                             mlir::TypeRange capTypes) {
  mlir::BlockArgument param = spec.getArgument(index);
  llvm::SmallVector<unsigned> positions(capTypes.size(), index + 1);
  llvm::SmallVector<mlir::DictionaryAttr> attrs(capTypes.size());
  llvm::SmallVector<mlir::Location> locs(capTypes.size(), param.getLoc());
  if (mlir::failed(spec.insertArguments(positions, capTypes, attrs, locs)))
    llvm_unreachable("a function body accepts new arguments");
  llvm::SmallVector<mlir::Value> caps(
      spec.getArguments().slice(index + 1, capTypes.size()));

  mlir::OpBuilder builder(spec.getContext());
  for (mlir::Operation *user : llvm::to_vector(param.getUsers())) {
    builder.setInsertionPoint(user);
    if (llvm::isa<ReussirRcIncOp>(user)) {
      for (mlir::Value cap : caps)
        if (llvm::isa<RcType>(cap.getType()))
          ReussirRcIncOp::create(builder, user->getLoc(), cap);
      user->erase();
    } else if (auto release = llvm::dyn_cast<ReussirRcDecOp>(user)) {
      releaseCaptures(builder, release, caps);
      user->erase();
    } else if (auto call = llvm::dyn_cast<mlir::func::CallOp>(user)) {
      auto direct = mlir::func::CallOp::create(
          builder, call.getLoc(), spec,
          replaceOperand(call.getOperands(), index, caps));
      call.replaceAllUsesWith(direct.getResults());
      call.erase();
    } else {
      mlir::Operation *uniqify =
          llvm::isa<ReussirClosureUniqifyOp>(user) ? user : nullptr;
      auto eval = llvm::cast<ReussirClosureEvalOp>(
          uniqify ? *uniqify->user_begin() : user);
      builder.setInsertionPoint(eval);
      llvm::SmallVector<mlir::Value> args(caps);
      llvm::append_range(args, eval.getArgs());
      mlir::Value result = spliceBody(builder, create, args);
      if (eval.getNumResults())
        eval.getResult().replaceAllUsesWith(result);
      eval.erase();
      if (uniqify)
        uniqify->erase();
    }
  }
  if (mlir::failed(spec.eraseArgument(index)))
    llvm_unreachable("the closure parameter has no uses left");
}

// Whether `function` holds at most `limit` operations.
bool isSmallFunction(mlir::func::FuncOp function, unsigned limit) {
  unsigned count = 0;
  return !function
              .walk([&](mlir::Operation *) {
                return ++count > limit ? mlir::WalkResult::interrupt()
                                       : mlir::WalkResult::advance();
              })
              .wasInterrupted();
}

// Specializes callees on the closure literals passed to them. Copies are
// bounded per callee; a copy's own calls are visited in turn.
void specializeClosureLiterals(mlir::ModuleOp module, unsigned maxCalleeOps,
                               unsigned maxCopiesPerCallee) {
  mlir::SymbolTable symbols(module);
  llvm::DenseMap<mlir::Operation *, unsigned> copies;
  llvm::SmallVector<mlir::func::CallOp> worklist;
  module.walk([&](mlir::func::CallOp call) { worklist.push_back(call); });
  while (!worklist.empty()) {
    mlir::func::CallOp call = worklist.pop_back_val();
    auto callee = symbols.lookup<mlir::func::FuncOp>(call.getCallee());
    if (!callee || callee.isExternal() || callee == call->getParentOp() ||
        copies[callee] >= maxCopiesPerCallee ||
        !isSmallFunction(callee, maxCalleeOps))
      continue;
    for (auto [index, arg] : llvm::enumerate(call.getOperands())) {
      auto rc = llvm::dyn_cast<RcType>(arg.getType());
      if (!rc || !llvm::isa<ClosureType>(rc.getElementType()) ||
          !arg.hasOneUse())
        continue;
      llvm::SmallVector<mlir::Operation *> chain;
      llvm::SmallVector<mlir::Value> caps;
      ReussirClosureCreateOp create = matchFreshChain(arg, chain, caps);
      if (!create || !sameCoverageScope(create, callee) ||
          !capturesReleasable(caps) ||
          !isClosureLiteralParameter(callee, index))
        continue;

      mlir::func::FuncOp spec = clonePrivateSpecialization(
          callee, (callee.getSymName() + ".closure").str(), symbols);
      ++copies[callee];
      rewriteClosureParameter(spec, index, create,
                              mlir::ValueRange(caps).getTypes());
      mlir::OpBuilder builder(call);
      auto direct = mlir::func::CallOp::create(
          builder, call.getLoc(), spec,
          replaceOperand(call.getOperands(), index, caps));
      call.replaceAllUsesWith(direct.getResults());
      call.erase();
      mlir::Value token = create.getToken();
      // The literal's body goes away with it, and so do its calls.
      llvm::SmallPtrSet<mlir::Operation *, 8> inBody;
      create.walk([&](mlir::func::CallOp inner) { inBody.insert(inner); });
      llvm::erase_if(worklist, [&](mlir::func::CallOp pending) {
        return inBody.contains(pending);
      });
      for (mlir::Operation *op : llvm::reverse(chain))
        op->erase();
      if (token && token.use_empty())
        token.getDefiningOp()->erase();
      worklist.push_back(direct);
      spec.walk([&](mlir::func::CallOp inner) {
        if (inner.getCallee() != spec.getSymName())
          worklist.push_back(inner);
      });
      break;
    }
  }
}

struct ClosureBetaReductionPass
    : public impl::ReussirClosureBetaReductionPassBase<
          ClosureBetaReductionPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::RewritePatternSet patterns(&getContext());
    patterns.add<FoldApplyIntoEvalPattern, StripNoopUniqifyPattern,
                 InlineCreateIntoEvalPattern,
                 InlineStructurallyCarriedCreatePattern,
                 DropFreshClosurePattern, SinkFreshClosurePattern>(
        &getContext());
    mlir::FrozenRewritePatternSet frozen(std::move(patterns));
    if (mlir::failed(mlir::applyPatternsGreedily(getOperation(), frozen)))
      return signalPassFailure();
    specializeClosureLiterals(getOperation(), maxCalleeOps, maxCopiesPerCallee);
    // Reduce the closures the spliced bodies create in turn.
    if (mlir::failed(mlir::applyPatternsGreedily(getOperation(), frozen)))
      signalPassFailure();
  }
};

} // namespace
} // namespace reussir
