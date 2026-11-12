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
/// Tail recursion modulo constructors via first-class constructor contexts.
///
/// A self-recursive function returning an rc-boxed value is split into a thin
/// wrapper and a `.trmc` helper that threads a `!reussir.cctx` accumulator —
/// the partially built result with one unfilled hole. Every return-position
/// leaf of the helper is rewritten into exactly one of three shapes:
///
///   A. a constructor whose field is a direct self-call: allocate the
///      constructor with a hole, `cctx.extend` the context with it, and
///      tail-call the helper — the constructor chain becomes a loop;
///   B. a plain self tail call: thread the context through unchanged;
///   C. anything else: `cctx.apply` the context to the finished value.
///
/// Because every leaf has a sound rewrite, eligibility is purely per-site: a
/// non-constructor arm (say, a rebalancing wrapper call around the recursion)
/// no longer disables the constructor arms next to it — it simply takes the
/// apply fallback, exactly like Koka's ctail compilation.
///
/// Shapes A and B re-create the self call at the leaf, so the call moves past
/// everything that ran between it and the leaf. A call is linearized only
/// when each of those ops is speculatable and free of effects that conflict
/// with the callee's (`canPrecedeSelfCall`); otherwise it stays where it is
/// and its leaf takes the apply fallback.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/RcDecrementExpansion.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/Transformation/Passes.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/IRMapping.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/SideEffectInterfaces.h>
#include <mlir/Pass/Pass.h>

#include <generator>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRTRMCRECURSIONANALYSISPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

static constexpr llvm::StringLiteral kTrmcSuffix = ".trmc";

struct RecursiveFieldInfo {
  unsigned index;
  mlir::func::CallOp call;
};

static bool hasDirectSelfRecursiveCall(mlir::func::FuncOp funcOp) {
  bool found = false;
  llvm::StringRef name = funcOp.getName();
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() == name)
      found = true;
  });
  return found;
}

static void addOptionalAttr(mlir::OperationState &state, llvm::StringRef name,
                            mlir::Attribute attr) {
  if (attr)
    state.addAttribute(name, attr);
}

static ReussirRcCreateCompoundOp createCompoundWithHoles(
    mlir::OpBuilder &builder, mlir::Location loc, RcType rcType,
    mlir::ValueRange fields, mlir::Value token, mlir::Value region,
    mlir::FlatSymbolRefAttr vtable, bool skipRc,
    mlir::DenseI64ArrayAttr skipFields, mlir::DenseI64ArrayAttr holeFields) {
  mlir::OperationState state(loc,
                             ReussirRcCreateCompoundOp::getOperationName());
  state.addOperands(fields);
  if (token)
    state.addOperands(token);
  if (region)
    state.addOperands(region);

  llvm::SmallVector<mlir::Type> resultTypes{rcType};
  if (holeFields)
    for (int64_t index : holeFields.asArrayRef())
      resultTypes.push_back(HoleType::get(
          builder.getContext(), fields[static_cast<size_t>(index)].getType()));
  state.addTypes(resultTypes);
  state.addAttribute(
      "operandSegmentSizes",
      builder.getDenseI32ArrayAttr({static_cast<int32_t>(fields.size()),
                                    token ? 1 : 0, region ? 1 : 0}));
  addOptionalAttr(state, "vtable", vtable);
  if (skipRc)
    state.addAttribute("skipRc", builder.getUnitAttr());
  addOptionalAttr(state, "skipFields", skipFields);
  addOptionalAttr(state, "holeFields", holeFields);
  return llvm::cast<ReussirRcCreateCompoundOp>(builder.create(state));
}

static ReussirRcCreateVariantOp createVariantWithHoles(
    mlir::OpBuilder &builder, mlir::Location loc, RcType rcType,
    mlir::IntegerAttr tag, mlir::ValueRange fields, mlir::Value token,
    mlir::Value region, mlir::FlatSymbolRefAttr vtable, bool skipRc,
    mlir::DenseI64ArrayAttr skipFields, mlir::DenseI64ArrayAttr holeFields) {
  mlir::OperationState state(loc, ReussirRcCreateVariantOp::getOperationName());
  state.addAttribute("tag", tag);
  state.addOperands(fields);
  if (token)
    state.addOperands(token);
  if (region)
    state.addOperands(region);

  llvm::SmallVector<mlir::Type> resultTypes{rcType};
  if (holeFields)
    for (int64_t index : holeFields.asArrayRef())
      resultTypes.push_back(HoleType::get(
          builder.getContext(), fields[static_cast<size_t>(index)].getType()));
  state.addTypes(resultTypes);
  state.addAttribute(
      "operandSegmentSizes",
      builder.getDenseI32ArrayAttr({0, static_cast<int32_t>(fields.size()),
                                    token ? 1 : 0, region ? 1 : 0}));
  addOptionalAttr(state, "vtable", vtable);
  if (skipRc)
    state.addAttribute("skipRc", builder.getUnitAttr());
  addOptionalAttr(state, "skipFields", skipFields);
  addOptionalAttr(state, "holeFields", holeFields);
  return llvm::cast<ReussirRcCreateVariantOp>(builder.create(state));
}

// The candidate recursive fields of a fused create: fields whose value is
// produced by a direct self-call. Whether such a call is actually
// linearizable is a whole-function question answered by `TrmcPlan` — token
// reuse duplicates one source-level constructor into sibling scf.if branches
// (reuse-token vs fresh-alloc), so one call may legitimately feed several
// same-site creates. A call feeding two fields of the *same* create can never
// be linearized and is dropped here.
static llvm::SmallVector<RecursiveFieldInfo>
collectRecursiveFields(mlir::ValueRange fields, llvm::StringRef callee) {
  llvm::SmallVector<RecursiveFieldInfo> recursiveFields;
  llvm::SmallDenseSet<mlir::Operation *> seenCalls;
  llvm::SmallDenseSet<mlir::Operation *> duplicated;
  for (auto [index, field] : llvm::enumerate(fields)) {
    auto callOp =
        llvm::dyn_cast_if_present<mlir::func::CallOp>(field.getDefiningOp());
    if (!callOp || callOp.getCallee() != callee || callOp.getNumResults() != 1)
      continue;
    if (!seenCalls.insert(callOp.getOperation()).second) {
      duplicated.insert(callOp.getOperation());
      continue;
    }
    recursiveFields.push_back(
        RecursiveFieldInfo{static_cast<unsigned>(index), callOp});
  }
  llvm::erase_if(recursiveFields, [&](RecursiveFieldInfo &info) {
    return duplicated.contains(info.call.getOperation());
  });
  return recursiveFields;
}

// A fused, non-regional create usable as a class-A leaf shell.
static bool isCandidateCreate(mlir::Value value) {
  if (!value.hasOneUse())
    return false;
  if (auto create = llvm::dyn_cast_if_present<ReussirRcCreateCompoundOp>(
          value.getDefiningOp()))
    return !create.getRegion();
  if (auto create = llvm::dyn_cast_if_present<ReussirRcCreateVariantOp>(
          value.getDefiningOp()))
    return !create.getValue() && !create.getRegion();
  return false;
}

static mlir::ValueRange createFields(mlir::Operation *create) {
  if (auto compound = llvm::dyn_cast<ReussirRcCreateCompoundOp>(create))
    return compound.getFields();
  return llvm::cast<ReussirRcCreateVariantOp>(create).getFields();
}

// Two creates are the same source-level site iff they agree on op kind,
// location, tag, and recursive-field shape. Compiler duplication (token
// reuse's scf.if dualization) produces exactly such clones, and the clones
// live in mutually exclusive branches — which is what makes it sound to
// consume one shared recursive call from each of them.
static bool isSameCreateSite(mlir::Operation *lhs, mlir::Operation *rhs,
                             llvm::StringRef callee) {
  if (lhs == rhs)
    return true;
  if (lhs->getName() != rhs->getName() || lhs->getLoc() != rhs->getLoc())
    return false;
  if (auto lhsVariant = llvm::dyn_cast<ReussirRcCreateVariantOp>(lhs)) {
    auto rhsVariant = llvm::cast<ReussirRcCreateVariantOp>(rhs);
    if (lhsVariant.getTagAttr() != rhsVariant.getTagAttr())
      return false;
  }
  auto lhsFields = collectRecursiveFields(createFields(lhs), callee);
  auto rhsFields = collectRecursiveFields(createFields(rhs), callee);
  if (lhsFields.size() != rhsFields.size())
    return false;
  for (auto [l, r] : llvm::zip(lhsFields, rhsFields))
    if (l.index != r.index)
      return false;
  return true;
}

// The ops that run before `op` in its function, nearest first: its
// predecessors in the block, then those of each enclosing op.
static std::generator<mlir::Operation *> precedingOps(mlir::Operation *op) {
  for (; op && !llvm::isa<mlir::func::FuncOp>(op); op = op->getParentOp())
    for (mlir::Operation *prev = op->getPrevNode(); prev;
         prev = prev->getPrevNode())
      co_yield prev;
}

// Whether the callee cannot write what `load` reads: the reference comes from
// a box by projection and passes neither a cell nor a mutable field.
static bool readsImmutableMemory(ReussirRefLoadOp load) {
  mlir::Value ref = load.getRef();
  for (;;) {
    auto type = llvm::cast<RefType>(ref.getType());
    if (type.getCapability() == Capability::field ||
        llvm::isa<CellType>(type.getElementType()))
      return false;
    if (auto argument = llvm::dyn_cast<mlir::BlockArgument>(ref)) {
      auto dispatch = llvm::dyn_cast<ReussirRecordDispatchOp>(
          argument.getOwner()->getParentOp());
      if (!dispatch)
        return false;
      ref = dispatch.getVariant();
      continue;
    }
    mlir::Operation *def = ref.getDefiningOp();
    if (llvm::isa<ReussirRcBorrowOp>(def))
      return true;
    if (!llvm::isa<ReussirRefProjectOp, ReussirRecordCoerceOp>(def))
      return false;
    ref = def->getOperand(0);
  }
}

// Whether the caller of this function keeps `value` alive for the whole call:
// a borrowed parameter, or a value loaded through one.
static bool isPinned(mlir::Value value) {
  for (;;) {
    if (auto argument = llvm::dyn_cast<mlir::BlockArgument>(value)) {
      mlir::Operation *parent = argument.getOwner()->getParentOp();
      if (auto dispatch = llvm::dyn_cast<ReussirRecordDispatchOp>(parent)) {
        value = dispatch.getVariant();
        continue;
      }
      auto function = llvm::dyn_cast<mlir::func::FuncOp>(parent);
      return function &&
             function.getArgAttr(argument.getArgNumber(), BorrowedAttr::name);
    }
    mlir::Operation *def = value.getDefiningOp();
    if (!llvm::isa<ReussirRefLoadOp, ReussirRefProjectOp, ReussirRecordCoerceOp,
                   ReussirRcBorrowOp>(def))
      return false;
    value = def->getOperand(0);
  }
}

// Whether the self call `call` passes, at a borrowed position, a value that
// only this function keeps alive.
static bool lendsLocalValue(mlir::func::CallOp call) {
  auto function = call->getParentOfType<mlir::func::FuncOp>();
  for (auto [index, argument] : llvm::enumerate(call.getOperands()))
    if (function.getArgAttr(index, BorrowedAttr::name) && !isPinned(argument))
      return true;
  return false;
}

// Whether running `op` before a self call it used to follow is unobservable.
// The op must be speculatable, since the call may not return, and its effects
// must not conflict with the callee's. Besides pure ops, that admits reads of
// immutable memory and the bookkeeping of retains and releases: counts,
// tokens, and allocation. Another call, a store, or a read of memory the
// callee may write does not qualify, and neither does a manual count change
// (an `unsafe` retain or release), whose place the program chose. When the
// call borrows a value that only this function keeps alive (`lends`), an op
// that may release has to follow it as well.
static bool canPrecedeSelfCall(mlir::Operation *op, bool lends) {
  if (llvm::isa<mlir::CallOpInterface>(op) || isUnsafeRcOperation(op))
    return false;
  if (auto load = llvm::dyn_cast<ReussirRefLoadOp>(op))
    return readsImmutableMemory(load);
  if (llvm::isa<ReussirRcIncOp, ReussirRcDecOp, ReussirRcFetchOp,
                ReussirRcFetchSubOp, ReussirRcSetOp, ReussirRcReinterpretOp,
                ReussirRcCreateOp, ReussirRcCreateCompoundOp,
                ReussirRcCreateVariantOp, ReussirRefAcquireOp, ReussirRefDropOp,
                ReussirRefSpilledOp, ReussirTokenAllocOp, ReussirTokenFreeOp,
                ReussirTokenEnsureOp, ReussirTokenLaunderOp,
                ReussirTokenReallocOp>(op) ||
      op->hasAttr(kExpandedDecrementAttr))
    return !lends || leavesCountsAlone(op) ||
           llvm::isa<ReussirRcCreateOp, ReussirRcCreateCompoundOp,
                     ReussirRcCreateVariantOp, ReussirTokenAllocOp>(op);
  if (op->getNumRegions() == 0)
    return mlir::isPure(op);
  // Nested ops are judged one by one, so the region op only has to defer
  // both its effects and its speculatability to them.
  if (!op->hasTrait<mlir::OpTrait::HasRecursiveMemoryEffects>())
    return false;
  auto speculatable = llvm::dyn_cast<mlir::ConditionallySpeculatable>(op);
  if (!speculatable ||
      speculatable.getSpeculatability() == mlir::Speculation::NotSpeculatable)
    return false;
  return llvm::all_of(op->getRegions(), [=](mlir::Region &region) {
    return llvm::all_of(region.getOps(), [=](mlir::Operation &nested) {
      return canPrecedeSelfCall(&nested, lends);
    });
  });
}

// Whether `call` can be re-created at `terminator`.
static bool canMoveCallTo(mlir::func::CallOp call,
                          mlir::Operation *terminator) {
  bool lends = lendsLocalValue(call);
  for (mlir::Operation *op : precedingOps(terminator)) {
    if (op == call.getOperation())
      return true;
    if (!canPrecedeSelfCall(op, lends))
      return false;
  }
  return false;
}

// Keeps the recursive fields of `create` whose calls can be re-created at
// `terminator`. The rewrite emits the calls in field order, so walking back
// from the leaf they must appear in reverse field order; the first call that
// stays in place (a `pinned` one included) keeps every earlier one in place.
static void
retainMovableFields(mlir::Operation *terminator, mlir::Operation *create,
                    llvm::SmallVectorImpl<RecursiveFieldInfo> &fields,
                    const llvm::SmallPtrSetImpl<mlir::Operation *> &pinned) {
  size_t firstMovable = fields.size();
  for (mlir::Operation *op : precedingOps(terminator)) {
    if (firstMovable == 0)
      break;
    if (op == create)
      continue;
    auto *field = llvm::find_if(fields, [&](RecursiveFieldInfo &info) {
      return info.call.getOperation() == op;
    });
    if (field == fields.end()) {
      // `op` ends up before the calls not met yet.
      bool lends = llvm::any_of(llvm::ArrayRef(fields).take_front(firstMovable),
                                [](const RecursiveFieldInfo &info) {
                                  return lendsLocalValue(info.call);
                                });
      if (!canPrecedeSelfCall(op, lends))
        break;
      continue;
    }
    size_t position = field - fields.begin();
    if (pinned.contains(op) || position + 1 != firstMovable)
      break;
    firstMovable = position;
  }
  fields.erase(fields.begin(), fields.begin() + firstMovable);
}

struct LeafInfo {
  mlir::Operation *terminator;
  mlir::Value value;
};

// The per-function rewrite plan: every return-position leaf, the class-A
// creates with their linearizable recursive fields, and the class-B calls.
struct TrmcPlan {
  llvm::SmallVector<LeafInfo> leaves;
  llvm::SmallDenseMap<mlir::Operation *, llvm::SmallVector<RecursiveFieldInfo>>
      constructorLeaves;
  llvm::SmallPtrSet<mlir::Operation *, 4> tailCalls;
};

// Walks the value spine from a return down through single-result,
// single-use structured control flow and invokes `visit` on every leaf
// (terminator, yielded value) pair.
static void
forEachLeaf(mlir::Operation *terminator, mlir::Value value,
            llvm::function_ref<void(mlir::Operation *, mlir::Value)> visit) {
  mlir::Operation *def = value.getDefiningOp();
  auto allRegionsHaveOneBlock = [](mlir::Operation *op) {
    return llvm::all_of(op->getRegions(), [](mlir::Region &region) {
      return region.hasOneBlock();
    });
  };
  if (def && def->getNumResults() == 1 && value.hasOneUse() &&
      llvm::isa<mlir::scf::IfOp, mlir::scf::IndexSwitchOp,
                ReussirRecordDispatchOp, ReussirNullableDispatchOp>(def) &&
      def->getNumRegions() > 0 && allRegionsHaveOneBlock(def) &&
      llvm::none_of(def->getRegions(),
                    [](mlir::Region &region) { return region.empty(); })) {
    for (mlir::Region &region : def->getRegions()) {
      mlir::Operation *nested = region.front().getTerminator();
      mlir::Value yielded;
      if (auto yield = llvm::dyn_cast<mlir::scf::YieldOp>(nested)) {
        if (yield.getNumOperands() == 1)
          yielded = yield.getOperand(0);
      } else if (auto yield = llvm::dyn_cast<ReussirScfYieldOp>(nested)) {
        yielded = yield.getValue();
      }
      if (!yielded) {
        // A region that does not yield the spine value (e.g. it panics)
        // contributes no leaf.
        continue;
      }
      forEachLeaf(nested, yielded, visit);
    }
    return;
  }
  visit(terminator, value);
}

static TrmcPlan buildPlan(mlir::func::FuncOp funcOp, llvm::StringRef callee) {
  TrmcPlan plan;
  funcOp.walk([&](mlir::func::ReturnOp returnOp) {
    if (returnOp.getNumOperands() != 1)
      return;
    forEachLeaf(returnOp, returnOp.getOperand(0),
                [&](mlir::Operation *terminator, mlir::Value value) {
                  plan.leaves.push_back(LeafInfo{terminator, value});
                });
  });

  // Candidate creates and their candidate recursive fields.
  llvm::SmallDenseSet<mlir::Operation *> candidates;
  for (LeafInfo &leaf : plan.leaves)
    if (isCandidateCreate(leaf.value))
      candidates.insert(leaf.value.getDefiningOp());

  // A recursive call is linearizable iff every use feeds a candidate create
  // of the same site: the branches consume the shared call exactly once each
  // along mutually exclusive paths, so after every user is rewritten the
  // call itself dies.
  auto isLinearizable = [&](mlir::func::CallOp call, mlir::Operation *create) {
    for (mlir::Operation *user : call->getUsers())
      if (!candidates.contains(user) || !isSameCreateSite(create, user, callee))
        return false;
    return true;
  };

  // A shared call is consumed by several creates. If one of them cannot
  // take it, it stays in place for all of them, which in turn pins the calls
  // before it; repeat until no further call is pinned.
  llvm::SmallPtrSet<mlir::Operation *, 4> pinned;
  for (bool changed = true; changed;) {
    changed = false;
    plan.constructorLeaves.clear();
    for (LeafInfo &leaf : plan.leaves) {
      mlir::Operation *create = leaf.value.getDefiningOp();
      if (!candidates.contains(create))
        continue;
      llvm::SmallVector<RecursiveFieldInfo> fields =
          collectRecursiveFields(createFields(create), callee);
      llvm::erase_if(fields, [&](RecursiveFieldInfo &info) {
        return !isLinearizable(info.call, create);
      });
      llvm::SmallVector<RecursiveFieldInfo> movable(fields);
      retainMovableFields(leaf.terminator, create, movable, pinned);
      for (RecursiveFieldInfo &info :
           llvm::MutableArrayRef(fields).drop_back(movable.size()))
        changed |= pinned.insert(info.call.getOperation()).second;
      if (!movable.empty())
        plan.constructorLeaves.try_emplace(create, std::move(movable));
    }
  }

  for (LeafInfo &leaf : plan.leaves) {
    auto call = llvm::dyn_cast_if_present<mlir::func::CallOp>(
        leaf.value.getDefiningOp());
    if (call && call.getCallee() == callee && call.getNumResults() == 1 &&
        leaf.value.hasOneUse() && canMoveCallTo(call, leaf.terminator))
      plan.tailCalls.insert(call);
  }
  return plan;
}

struct TrmcRewriteContext {
  mlir::IRRewriter &rewriter;
  llvm::StringRef helperName;
  mlir::Value ctxArg;
  CctxType ctxType;
};

static void setLeafValue(mlir::Operation *terminator, mlir::Value oldValue,
                         mlir::Value newValue) {
  for (mlir::OpOperand &operand : terminator->getOpOperands())
    if (operand.get() == oldValue)
      operand.set(newValue);
}

// Class A: rebuild the create with holes at every linearizable recursive
// field, recurse into the non-designated fields through fresh single-node
// contexts, and turn the designated (last) field into the context-threaded
// tail call.
static void rewriteConstructorLeaf(mlir::Operation *terminator,
                                   mlir::Value value,
                                   llvm::ArrayRef<RecursiveFieldInfo> planned,
                                   TrmcRewriteContext &ctx) {
  mlir::Operation *def = value.getDefiningOp();
  auto compound = llvm::dyn_cast_if_present<ReussirRcCreateCompoundOp>(def);
  auto variant = llvm::dyn_cast_if_present<ReussirRcCreateVariantOp>(def);

  mlir::ValueRange fieldRange =
      compound ? compound.getFields() : variant.getFields();
  llvm::SmallVector<RecursiveFieldInfo> recursiveFields(planned.begin(),
                                                        planned.end());

  mlir::IRRewriter &rewriter = ctx.rewriter;
  mlir::Location loc = def->getLoc();
  rewriter.setInsertionPoint(terminator);

  llvm::SmallVector<mlir::Value> fields(fieldRange.begin(), fieldRange.end());
  llvm::SmallVector<int64_t> holeIndices;
  holeIndices.reserve(recursiveFields.size());
  for (RecursiveFieldInfo &field : recursiveFields) {
    fields[field.index] = mlir::ub::PoisonOp::create(
        rewriter, loc, fields[field.index].getType());
    holeIndices.push_back(static_cast<int64_t>(field.index));
  }
  auto holeFields = rewriter.getDenseI64ArrayAttr(holeIndices);

  mlir::Value rcPtr;
  mlir::ValueRange holes;
  if (compound) {
    auto newCreate = createCompoundWithHoles(
        rewriter, loc, compound.getRcPtr().getType(), fields,
        compound.getToken(), compound.getRegion(), compound.getVtableAttr(),
        compound.getSkipRc(), compound.getSkipFieldsAttr(), holeFields);
    rcPtr = newCreate.getRcPtr();
    holes = newCreate.getHoles();
  } else {
    auto newCreate = createVariantWithHoles(
        rewriter, loc, variant.getRcPtr().getType(), variant.getTagAttr(),
        fields, variant.getToken(), variant.getRegion(),
        variant.getVtableAttr(), variant.getSkipRc(),
        variant.getSkipFieldsAttr(), holeFields);
    rcPtr = newCreate.getRcPtr();
    holes = newCreate.getHoles();
  }

  mlir::Type resultType = ctx.ctxType.getElementType();
  // Non-designated recursive fields recurse through fresh single-node
  // contexts rooted at the new constructor; their (identical) results are
  // discarded.
  for (size_t i = 0; i + 1 < recursiveFields.size(); ++i) {
    RecursiveFieldInfo &field = recursiveFields[i];
    mlir::Value empty =
        ReussirCctxEmptyOp::create(rewriter, field.call.getLoc(), ctx.ctxType);
    mlir::Value sub = ReussirCctxExtendOp::create(
        rewriter, field.call.getLoc(), ctx.ctxType, empty, rcPtr, holes[i]);
    llvm::SmallVector<mlir::Value> args(field.call.getOperands());
    args.push_back(sub);
    mlir::func::CallOp::create(rewriter, field.call.getLoc(), ctx.helperName,
                               mlir::TypeRange{resultType}, args);
  }

  // The designated (last) recursive field extends the incoming context and
  // becomes the tail call whose result is the leaf's value.
  RecursiveFieldInfo &designated = recursiveFields.back();
  mlir::Value extended = ReussirCctxExtendOp::create(
      rewriter, designated.call.getLoc(), ctx.ctxType, ctx.ctxArg, rcPtr,
      holes[recursiveFields.size() - 1]);
  llvm::SmallVector<mlir::Value> args(designated.call.getOperands());
  args.push_back(extended);
  auto tailCall = mlir::func::CallOp::create(rewriter, designated.call.getLoc(),
                                             ctx.helperName,
                                             mlir::TypeRange{resultType}, args);

  setLeafValue(terminator, value, tailCall.getResult(0));
  rewriter.eraseOp(def);
  for (RecursiveFieldInfo &field : recursiveFields)
    if (field.call->use_empty())
      rewriter.eraseOp(field.call);
}

// Class B: a plain self tail call threads the context through unchanged.
static void rewriteTailCallLeaf(mlir::Operation *terminator, mlir::Value value,
                                TrmcRewriteContext &ctx) {
  auto call = llvm::cast<mlir::func::CallOp>(value.getDefiningOp());
  mlir::IRRewriter &rewriter = ctx.rewriter;
  rewriter.setInsertionPoint(terminator);
  llvm::SmallVector<mlir::Value> args(call.getOperands());
  args.push_back(ctx.ctxArg);
  auto newCall = mlir::func::CallOp::create(
      rewriter, call.getLoc(), ctx.helperName,
      mlir::TypeRange{ctx.ctxType.getElementType()}, args);
  setLeafValue(terminator, value, newCall.getResult(0));
  rewriter.eraseOp(call);
}

// Class C: any other leaf completes the context with its value.
static void rewriteApplyLeaf(mlir::Operation *terminator, mlir::Value value,
                             TrmcRewriteContext &ctx) {
  mlir::IRRewriter &rewriter = ctx.rewriter;
  rewriter.setInsertionPoint(terminator);
  auto apply = ReussirCctxApplyOp::create(rewriter, terminator->getLoc(),
                                          ctx.ctxType.getElementType(),
                                          ctx.ctxArg, value);
  setLeafValue(terminator, value, apply.getResult());
}

static void rewriteLeaf(mlir::Operation *terminator, mlir::Value value,
                        const TrmcPlan &plan, TrmcRewriteContext &ctx) {
  if (mlir::Operation *def = value.getDefiningOp()) {
    auto planned = plan.constructorLeaves.find(def);
    if (planned != plan.constructorLeaves.end()) {
      rewriteConstructorLeaf(terminator, value, planned->second, ctx);
      return;
    }
    if (plan.tailCalls.contains(def)) {
      rewriteTailCallLeaf(terminator, value, ctx);
      return;
    }
  }
  rewriteApplyLeaf(terminator, value, ctx);
}

static void makeHelperInternal(mlir::func::FuncOp helperFunc,
                               mlir::MLIRContext *context) {
  helperFunc.setPrivate();
  helperFunc->removeAttr("llvm.visibility");
  helperFunc->setAttr(
      "llvm.linkage",
      mlir::LLVM::LinkageAttr::get(context, mlir::LLVM::Linkage::Internal));
}

static mlir::func::FuncOp createTrmcHelper(mlir::func::FuncOp funcOp,
                                           mlir::SymbolTable &symbolTable,
                                           CctxType ctxType) {
  auto *context = funcOp.getContext();
  std::string helperName = (funcOp.getName() + kTrmcSuffix).str();
  if (auto existing = symbolTable.lookup<mlir::func::FuncOp>(helperName))
    return existing;

  auto helperFunc = llvm::cast<mlir::func::FuncOp>(funcOp->clone());
  helperFunc.setName(helperName);
  // The context comes last. `insertArgument` keeps the argument attributes
  // the original may carry (e.g. `reussir.borrowed`) in step with the
  // arguments, which a bare type change and block argument would not.
  mlir::NamedAttribute noundef(mlir::StringAttr::get(context, "llvm.noundef"),
                               mlir::UnitAttr::get(context));
  if (mlir::failed(helperFunc.insertArgument(
          helperFunc.getNumArguments(), ctxType,
          mlir::DictionaryAttr::get(context, noundef), helperFunc.getLoc())))
    llvm_unreachable("appending an argument cannot make the type invalid");
  helperFunc.setFunctionType(mlir::FunctionType::get(
      context, helperFunc.getArgumentTypes(), {ctxType.getElementType()}));
  makeHelperInternal(helperFunc, context);

  mlir::IRRewriter rewriter(context);
  TrmcRewriteContext ctx{
      rewriter, helperFunc.getName(),
      helperFunc.getArgument(helperFunc.getNumArguments() - 1), ctxType};

  // Plan on the clone: the plan owns the leaf list and the batch decision of
  // which shared recursive calls are linearizable, so rewrites can consume
  // one shared call from each of its mutually exclusive same-site creates.
  TrmcPlan plan = buildPlan(helperFunc, funcOp.getName());
  for (LeafInfo &leaf : plan.leaves)
    rewriteLeaf(leaf.terminator, leaf.value, plan, ctx);

  symbolTable.insert(helperFunc);
  return helperFunc;
}

// The original function becomes a thin wrapper: every caller enters the
// context-threaded helper through the empty context.
static void rewriteOriginalToWrapper(mlir::func::FuncOp funcOp,
                                     llvm::StringRef helperName,
                                     CctxType ctxType) {
  mlir::Region &body = funcOp.getBody();
  for (mlir::Block &block : body)
    block.dropAllReferences();

  mlir::OpBuilder builder(funcOp.getContext());
  llvm::SmallVector<mlir::Location> argLocs(funcOp.getNumArguments(),
                                            funcOp.getLoc());
  mlir::Block *entry = builder.createBlock(&body, body.begin(),
                                           funcOp.getArgumentTypes(), argLocs);
  while (&body.back() != entry)
    body.back().erase();

  builder.setInsertionPointToStart(entry);
  mlir::Value empty =
      ReussirCctxEmptyOp::create(builder, funcOp.getLoc(), ctxType);
  llvm::SmallVector<mlir::Value> args(entry->getArguments());
  args.push_back(empty);
  auto call = mlir::func::CallOp::create(
      builder, funcOp.getLoc(), helperName,
      mlir::TypeRange{ctxType.getElementType()}, args);
  mlir::func::ReturnOp::create(builder, funcOp.getLoc(), call.getResults());
}

struct TRMCRecursionAnalysisPass
    : public impl::ReussirTRMCRecursionAnalysisPassBase<
          TRMCRecursionAnalysisPass> {
  using Base::Base;

  void runOnOperation() override {
    auto moduleOp = getOperation();
    mlir::SymbolTable symbolTable(moduleOp);

    llvm::SmallVector<mlir::func::FuncOp> candidates;
    moduleOp.walk([&](mlir::func::FuncOp funcOp) {
      if (funcOp.isDeclaration() || funcOp.getName().ends_with(kTrmcSuffix))
        return;
      if (funcOp.getNumResults() != 1 ||
          !llvm::isa<RcType>(funcOp.getResultTypes().front()))
        return;
      if (!hasDirectSelfRecursiveCall(funcOp))
        return;
      if (buildPlan(funcOp, funcOp.getName()).constructorLeaves.empty())
        return;
      candidates.push_back(funcOp);
    });

    for (mlir::func::FuncOp funcOp : candidates) {
      auto ctxType =
          CctxType::get(funcOp.getContext(),
                        llvm::cast<RcType>(funcOp.getResultTypes().front()));
      mlir::func::FuncOp helper =
          createTrmcHelper(funcOp, symbolTable, ctxType);
      rewriteOriginalToWrapper(funcOp, helper.getName(), ctxType);
    }
  }
};

} // namespace
} // namespace reussir
