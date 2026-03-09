//===-- TRMCRecursionAnalysis.cpp -----------------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/IRMapping.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRTRMCRECURSIONANALYSISPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {

static constexpr llvm::StringLiteral kTrmcSuffix = ".trmc";

struct TrmcRewriteContext {
  mlir::IRRewriter &rewriter;
  mlir::func::FuncOp helperFunc;
  mlir::Value outHole;
  llvm::StringRef originalName;
  llvm::StringRef helperName;
  bool rewroteRecursiveSite = false;
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
    mlir::IRRewriter &rewriter, mlir::Location loc, RcType rcType,
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
          rewriter.getContext(), fields[static_cast<size_t>(index)].getType()));
  state.addTypes(resultTypes);
  state.addAttribute(
      "operandSegmentSizes",
      rewriter.getDenseI32ArrayAttr({static_cast<int32_t>(fields.size()),
                                     token ? 1 : 0, region ? 1 : 0}));
  addOptionalAttr(state, "vtable", vtable);
  if (skipRc)
    state.addAttribute("skipRc", rewriter.getUnitAttr());
  addOptionalAttr(state, "skipFields", skipFields);
  addOptionalAttr(state, "holeFields", holeFields);
  return llvm::cast<ReussirRcCreateCompoundOp>(rewriter.create(state));
}

static ReussirRcCreateVariantOp
createVariantWithHoles(mlir::IRRewriter &rewriter, mlir::Location loc,
                       RcType rcType, mlir::IntegerAttr tag, mlir::Value value,
                       mlir::ValueRange fields, mlir::Value token,
                       mlir::Value region, mlir::FlatSymbolRefAttr vtable,
                       bool skipRc, mlir::DenseI64ArrayAttr skipFields,
                       mlir::DenseI64ArrayAttr holeFields) {
  mlir::OperationState state(loc, ReussirRcCreateVariantOp::getOperationName());
  state.addAttribute("tag", tag);
  if (value)
    state.addOperands(value);
  state.addOperands(fields);
  if (token)
    state.addOperands(token);
  if (region)
    state.addOperands(region);

  llvm::SmallVector<mlir::Type> resultTypes{rcType};
  if (holeFields)
    for (int64_t index : holeFields.asArrayRef())
      resultTypes.push_back(HoleType::get(
          rewriter.getContext(), fields[static_cast<size_t>(index)].getType()));
  state.addTypes(resultTypes);
  state.addAttribute("operandSegmentSizes",
                     rewriter.getDenseI32ArrayAttr(
                         {value ? 1 : 0, static_cast<int32_t>(fields.size()),
                          token ? 1 : 0, region ? 1 : 0}));
  addOptionalAttr(state, "vtable", vtable);
  if (skipRc)
    state.addAttribute("skipRc", rewriter.getUnitAttr());
  addOptionalAttr(state, "skipFields", skipFields);
  addOptionalAttr(state, "holeFields", holeFields);
  return llvm::cast<ReussirRcCreateVariantOp>(rewriter.create(state));
}

static mlir::LogicalResult replaceWithVoidTerminator(mlir::IRRewriter &rewriter,
                                                     mlir::Operation *term) {
  mlir::Location loc = term->getLoc();
  rewriter.setInsertionPoint(term);
  if (llvm::isa<mlir::func::ReturnOp>(term))
    rewriter.create<mlir::func::ReturnOp>(loc);
  else if (llvm::isa<mlir::scf::YieldOp>(term))
    rewriter.create<mlir::scf::YieldOp>(loc);
  else if (llvm::isa<ReussirScfYieldOp>(term))
    rewriter.create<ReussirScfYieldOp>(loc, mlir::Value{});
  else
    return mlir::failure();
  rewriter.eraseOp(term);
  return mlir::success();
}

static mlir::LogicalResult lowerValueIntoHole(mlir::Operation *terminator,
                                              mlir::Value value,
                                              TrmcRewriteContext &ctx);

static void clearAndCloneBody(mlir::IRRewriter &rewriter, mlir::Region &dst,
                              mlir::Region &src) {
  mlir::Block *placeholder = dst.empty() ? nullptr : &dst.front();
  rewriter.cloneRegionBefore(src, dst, dst.begin());
  if (placeholder)
    placeholder->erase();
}

static mlir::LogicalResult lowerRegionTerminator(mlir::Region &region,
                                                 TrmcRewriteContext &ctx) {
  mlir::Operation *terminator = region.front().getTerminator();
  if (auto yield = llvm::dyn_cast<mlir::scf::YieldOp>(terminator)) {
    if (yield.getNumOperands() != 1)
      return mlir::failure();
    return lowerValueIntoHole(terminator, yield.getOperand(0), ctx);
  }
  if (auto yield = llvm::dyn_cast<ReussirScfYieldOp>(terminator)) {
    if (!yield.getValue())
      return mlir::failure();
    return lowerValueIntoHole(terminator, yield.getValue(), ctx);
  }
  return mlir::failure();
}

static mlir::LogicalResult lowerStructuredValue(mlir::Operation *terminator,
                                                mlir::Value value,
                                                TrmcRewriteContext &ctx) {
  if (auto indexSwitch = llvm::dyn_cast_if_present<mlir::scf::IndexSwitchOp>(
          value.getDefiningOp())) {
    if (indexSwitch.getNumResults() != 1 || !value.hasOneUse())
      return mlir::failure();
    ctx.rewriter.setInsertionPoint(indexSwitch);
    auto newIndexSwitch = ctx.rewriter.create<mlir::scf::IndexSwitchOp>(
        indexSwitch.getLoc(), mlir::TypeRange{}, indexSwitch.getArg(),
        indexSwitch.getCasesAttr(), indexSwitch.getCaseRegions().size());
    clearAndCloneBody(ctx.rewriter, newIndexSwitch.getDefaultRegion(),
                      indexSwitch.getDefaultRegion());
    for (auto [oldRegion, newRegion] : llvm::zip(
             indexSwitch.getCaseRegions(), newIndexSwitch.getCaseRegions())) {
      clearAndCloneBody(ctx.rewriter, newRegion, oldRegion);
    }
    if (failed(lowerRegionTerminator(newIndexSwitch.getDefaultRegion(), ctx)))
      return mlir::failure();
    for (mlir::Region &region : newIndexSwitch.getCaseRegions())
      if (failed(lowerRegionTerminator(region, ctx)))
        return mlir::failure();
    if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
      return mlir::failure();
    ctx.rewriter.eraseOp(indexSwitch);
    return mlir::success();
  }

  if (auto ifOp =
          llvm::dyn_cast_if_present<mlir::scf::IfOp>(value.getDefiningOp())) {
    if (ifOp.getNumResults() != 1 || !value.hasOneUse())
      return mlir::failure();
    ctx.rewriter.setInsertionPoint(ifOp);
    auto newIf = ctx.rewriter.create<mlir::scf::IfOp>(
        ifOp.getLoc(), mlir::TypeRange{}, ifOp.getCondition(),
        !ifOp.getElseRegion().empty());
    clearAndCloneBody(ctx.rewriter, newIf.getThenRegion(),
                      ifOp.getThenRegion());
    if (!ifOp.getElseRegion().empty())
      clearAndCloneBody(ctx.rewriter, newIf.getElseRegion(),
                        ifOp.getElseRegion());
    if (failed(lowerRegionTerminator(newIf.getThenRegion(), ctx)))
      return mlir::failure();
    if (!newIf.getElseRegion().empty() &&
        failed(lowerRegionTerminator(newIf.getElseRegion(), ctx)))
      return mlir::failure();
    if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
      return mlir::failure();
    ctx.rewriter.eraseOp(ifOp);
    return mlir::success();
  }

  if (auto dispatch = llvm::dyn_cast_if_present<ReussirRecordDispatchOp>(
          value.getDefiningOp())) {
    if (dispatch.getNumResults() != 1 || !value.hasOneUse())
      return mlir::failure();
    mlir::OperationState state(dispatch.getLoc(),
                               ReussirRecordDispatchOp::getOperationName());
    state.addOperands(dispatch.getVariant());
    state.addAttribute("tagSets", dispatch.getTagSetsAttr());
    for (size_t i = 0; i < dispatch->getNumRegions(); ++i)
      state.addRegion();
    ctx.rewriter.setInsertionPoint(dispatch);
    auto newDispatch =
        llvm::cast<ReussirRecordDispatchOp>(ctx.rewriter.create(state));
    for (auto [oldRegion, newRegion] :
         llvm::zip(dispatch.getRegions(), newDispatch.getRegions())) {
      clearAndCloneBody(ctx.rewriter, newRegion, oldRegion);
      if (failed(lowerRegionTerminator(newRegion, ctx)))
        return mlir::failure();
    }
    if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
      return mlir::failure();
    ctx.rewriter.eraseOp(dispatch);
    return mlir::success();
  }

  if (auto dispatch = llvm::dyn_cast_if_present<ReussirNullableDispatchOp>(
          value.getDefiningOp())) {
    if (dispatch.getNumResults() != 1 || !value.hasOneUse())
      return mlir::failure();
    mlir::OperationState state(dispatch.getLoc(),
                               ReussirNullableDispatchOp::getOperationName());
    state.addOperands(dispatch.getNullable());
    state.addRegion();
    state.addRegion();
    ctx.rewriter.setInsertionPoint(dispatch);
    auto newDispatch =
        llvm::cast<ReussirNullableDispatchOp>(ctx.rewriter.create(state));
    clearAndCloneBody(ctx.rewriter, newDispatch.getNonNullRegion(),
                      dispatch.getNonNullRegion());
    clearAndCloneBody(ctx.rewriter, newDispatch.getNullRegion(),
                      dispatch.getNullRegion());
    if (failed(lowerRegionTerminator(newDispatch.getNonNullRegion(), ctx)) ||
        failed(lowerRegionTerminator(newDispatch.getNullRegion(), ctx)))
      return mlir::failure();
    if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
      return mlir::failure();
    ctx.rewriter.eraseOp(dispatch);
    return mlir::success();
  }

  return mlir::failure();
}

static mlir::LogicalResult
emitDirectRecursiveTailCall(mlir::Operation *terminator,
                            mlir::func::CallOp call, TrmcRewriteContext &ctx) {
  if (call.getCallee() != ctx.originalName)
    return mlir::failure();
  ctx.rewriter.setInsertionPoint(terminator);
  llvm::SmallVector<mlir::Value> helperArgs(call.getOperands());
  helperArgs.push_back(ctx.outHole);
  ctx.rewriter.create<mlir::func::CallOp>(call.getLoc(), ctx.helperName,
                                          mlir::TypeRange{}, helperArgs);
  ctx.rewroteRecursiveSite = true;
  if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
    return mlir::failure();
  ctx.rewriter.eraseOp(call);
  return mlir::success();
}

static std::optional<unsigned>
findFirstRecursiveFieldIndex(mlir::ValueRange fields, llvm::StringRef callee) {
  for (auto [index, field] : llvm::enumerate(fields)) {
    auto callOp =
        llvm::dyn_cast_if_present<mlir::func::CallOp>(field.getDefiningOp());
    if (!callOp || callOp.getCallee() != callee)
      continue;
    return static_cast<unsigned>(index);
  }
  return std::nullopt;
}

static bool isLinearizableCreateUser(mlir::Operation *user,
                                     mlir::func::CallOp callOp,
                                     llvm::StringRef callee) {
  auto isRecursiveFieldUser = [&](mlir::ValueRange fields) {
    auto recursiveIndex = findFirstRecursiveFieldIndex(fields, callee);
    return recursiveIndex &&
           fields[*recursiveIndex].getDefiningOp() == callOp.getOperation();
  };

  if (auto create = llvm::dyn_cast<ReussirRcCreateCompoundOp>(user))
    return isRecursiveFieldUser(create.getFields());
  if (auto create = llvm::dyn_cast<ReussirRcCreateVariantOp>(user))
    return !create.getValue() && isRecursiveFieldUser(create.getFields());
  return false;
}

static bool isCallOnlyUsedByLinearizableCreates(mlir::func::CallOp callOp,
                                                llvm::StringRef callee) {
  if (callOp.getCallee() != callee)
    return false;
  return llvm::all_of(callOp->getUsers(), [&](mlir::Operation *user) {
    return isLinearizableCreateUser(user, callOp, callee);
  });
}

static bool regionHasDirectSelfCall(mlir::Region &region,
                                    llvm::StringRef originalName) {
  bool found = false;
  region.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() == originalName)
      found = true;
  });
  return found;
}

static bool regionHasNonLinearSelfCall(mlir::Region &region,
                                       llvm::StringRef originalName) {
  bool found = false;
  region.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() != originalName)
      return;
    if (!isCallOnlyUsedByLinearizableCreates(callOp, originalName))
      found = true;
  });
  return found;
}

static mlir::Operation *
getUniqueLinearizableCreateUser(mlir::func::CallOp callOp,
                                llvm::StringRef originalName) {
  if (callOp.getCallee() != originalName || !callOp->hasOneUse())
    return nullptr;
  mlir::Operation *user = *callOp->user_begin();
  if (!isLinearizableCreateUser(user, callOp, originalName))
    return nullptr;
  return user;
}

static bool isSameLinearizableCreateSite(mlir::Operation *lhs, mlir::Operation *rhs,
                                         llvm::StringRef originalName) {
  if (!lhs || !rhs || lhs == rhs)
    return lhs == rhs;
  if (lhs->getName() != rhs->getName() || lhs->getLoc() != rhs->getLoc())
    return false;

  auto sameRecursiveIndex = [&](mlir::ValueRange lhsFields,
                                mlir::ValueRange rhsFields) {
    return findFirstRecursiveFieldIndex(lhsFields, originalName) ==
           findFirstRecursiveFieldIndex(rhsFields, originalName);
  };

  if (auto lhsCreate = llvm::dyn_cast<ReussirRcCreateCompoundOp>(lhs)) {
    auto rhsCreate = llvm::dyn_cast<ReussirRcCreateCompoundOp>(rhs);
    return rhsCreate &&
           sameRecursiveIndex(lhsCreate.getFields(), rhsCreate.getFields());
  }

  auto lhsCreate = llvm::dyn_cast<ReussirRcCreateVariantOp>(lhs);
  auto rhsCreate = llvm::dyn_cast<ReussirRcCreateVariantOp>(rhs);
  return lhsCreate && rhsCreate && lhsCreate.getTagAttr() == rhsCreate.getTagAttr() &&
         sameRecursiveIndex(lhsCreate.getFields(), rhsCreate.getFields());
}

static bool hasMeaningfulSiblingSelfCall(mlir::Region &region,
                                         mlir::Operation *currentCreateOp,
                                         llvm::StringRef originalName) {
  bool found = false;
  region.walk([&](mlir::func::CallOp callOp) {
    if (found || callOp.getCallee() != originalName)
      return;
    mlir::Operation *user = getUniqueLinearizableCreateUser(callOp, originalName);
    if (!user || !isSameLinearizableCreateSite(currentCreateOp, user, originalName))
      found = true;
  });
  return found;
}

static bool isSiteEligibleForTrmc(mlir::Operation *createOp,
                                  mlir::func::CallOp recursiveCall,
                                  llvm::StringRef originalName) {
  if (!isCallOnlyUsedByLinearizableCreates(recursiveCall, originalName))
    return false;

  mlir::Region *currentRegion = createOp->getParentRegion();
  mlir::Operation *parentOp =
      currentRegion ? currentRegion->getParentOp() : nullptr;
  while (parentOp && !llvm::isa<mlir::func::FuncOp>(parentOp)) {
    if (parentOp->getNumRegions() > 1) {
      bool sawSiblingSelfCall = false;
      for (mlir::Region &region : parentOp->getRegions()) {
        if (&region == currentRegion)
          continue;
        if (!regionHasDirectSelfCall(region, originalName))
          continue;
        sawSiblingSelfCall = true;
        if (regionHasNonLinearSelfCall(region, originalName))
          return false;
        if (hasMeaningfulSiblingSelfCall(region, createOp, originalName))
          return true;
      }
      if (sawSiblingSelfCall)
        continue;
    }
    currentRegion = parentOp->getParentRegion();
    parentOp = currentRegion ? currentRegion->getParentOp() : nullptr;
  }
  return true;
}

static mlir::LogicalResult
emitRecursiveCreateIntoHole(mlir::Operation *terminator, mlir::Value value,
                            TrmcRewriteContext &ctx) {
  if (auto create = llvm::dyn_cast_if_present<ReussirRcCreateCompoundOp>(
          value.getDefiningOp())) {
    auto recursiveIndex =
        findFirstRecursiveFieldIndex(create.getFields(), ctx.originalName);
    if (!recursiveIndex)
      return mlir::failure();

    auto recursiveCall = llvm::cast<mlir::func::CallOp>(
        create.getFields()[*recursiveIndex].getDefiningOp());
    if (!isSiteEligibleForTrmc(create.getOperation(), recursiveCall,
                               ctx.originalName))
      return mlir::failure();
    llvm::SmallVector<mlir::Value> fields(create.getFields().begin(),
                                          create.getFields().end());
    ctx.rewriter.setInsertionPoint(terminator);
    fields[*recursiveIndex] = ctx.rewriter.create<mlir::ub::PoisonOp>(
        create.getLoc(), fields[*recursiveIndex].getType());
    auto holeFields = ctx.rewriter.getDenseI64ArrayAttr(
        {static_cast<int64_t>(*recursiveIndex)});
    auto newCreate = createCompoundWithHoles(
        ctx.rewriter, create.getLoc(), create.getRcPtr().getType(), fields,
        create.getToken(), create.getRegion(), create.getVtableAttr(),
        create.getSkipRc(), create.getSkipFieldsAttr(), holeFields);
    ctx.rewriter.setInsertionPointAfter(newCreate);
    auto store = ctx.rewriter.create<ReussirHoleStoreOp>(
        create.getLoc(), ctx.outHole, newCreate.getRcPtr());
    llvm::SmallVector<mlir::Value> helperArgs(recursiveCall.getOperands());
    helperArgs.push_back(newCreate.getHoles().front());
    ctx.rewriter.setInsertionPointAfter(store);
    ctx.rewriter.create<mlir::func::CallOp>(
        recursiveCall.getLoc(), ctx.helperName, mlir::TypeRange{}, helperArgs);
    ctx.rewroteRecursiveSite = true;
    if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
      return mlir::failure();
    ctx.rewriter.eraseOp(create);
    if (recursiveCall.use_empty())
      ctx.rewriter.eraseOp(recursiveCall);
    return mlir::success();
  }

  auto create = llvm::dyn_cast_if_present<ReussirRcCreateVariantOp>(
      value.getDefiningOp());
  if (!create || create.getValue())
    return mlir::failure();

  auto recursiveIndex =
      findFirstRecursiveFieldIndex(create.getFields(), ctx.originalName);
  if (!recursiveIndex)
    return mlir::failure();

  auto recursiveCall = llvm::cast<mlir::func::CallOp>(
      create.getFields()[*recursiveIndex].getDefiningOp());
  if (!isSiteEligibleForTrmc(create.getOperation(), recursiveCall,
                             ctx.originalName))
    return mlir::failure();
  llvm::SmallVector<mlir::Value> fields(create.getFields().begin(),
                                        create.getFields().end());
  ctx.rewriter.setInsertionPoint(terminator);
  fields[*recursiveIndex] = ctx.rewriter.create<mlir::ub::PoisonOp>(
      create.getLoc(), fields[*recursiveIndex].getType());
  auto holeFields = ctx.rewriter.getDenseI64ArrayAttr(
      {static_cast<int64_t>(*recursiveIndex)});
  auto newCreate = createVariantWithHoles(
      ctx.rewriter, create.getLoc(), create.getRcPtr().getType(),
      create.getTagAttr(), create.getValue(), fields, create.getToken(),
      create.getRegion(), create.getVtableAttr(), create.getSkipRc(),
      create.getSkipFieldsAttr(), holeFields);
  ctx.rewriter.setInsertionPointAfter(newCreate);
  auto store = ctx.rewriter.create<ReussirHoleStoreOp>(
      create.getLoc(), ctx.outHole, newCreate.getRcPtr());
  llvm::SmallVector<mlir::Value> helperArgs(recursiveCall.getOperands());
  helperArgs.push_back(newCreate.getHoles().front());
  ctx.rewriter.setInsertionPointAfter(store);
  ctx.rewriter.create<mlir::func::CallOp>(
      recursiveCall.getLoc(), ctx.helperName, mlir::TypeRange{}, helperArgs);
  ctx.rewroteRecursiveSite = true;
  if (failed(replaceWithVoidTerminator(ctx.rewriter, terminator)))
    return mlir::failure();
  ctx.rewriter.eraseOp(create);
  if (recursiveCall.use_empty())
    ctx.rewriter.eraseOp(recursiveCall);
  return mlir::success();
}

static mlir::LogicalResult rewriteRecursiveCreateSite(
    mlir::Operation *createOp, llvm::StringRef originalName,
    llvm::StringRef helperName, mlir::IRRewriter &rewriter) {
  if (auto create = llvm::dyn_cast<ReussirRcCreateCompoundOp>(createOp)) {
    auto recursiveIndex =
        findFirstRecursiveFieldIndex(create.getFields(), originalName);
    if (!recursiveIndex)
      return mlir::failure();

    auto recursiveCall = llvm::cast<mlir::func::CallOp>(
        create.getFields()[*recursiveIndex].getDefiningOp());
    if (!isSiteEligibleForTrmc(create.getOperation(), recursiveCall,
                               originalName))
      return mlir::failure();
    llvm::SmallVector<mlir::Value> fields(create.getFields().begin(),
                                          create.getFields().end());
    rewriter.setInsertionPoint(create);
    fields[*recursiveIndex] = rewriter.create<mlir::ub::PoisonOp>(
        create.getLoc(), fields[*recursiveIndex].getType());
    auto holeFields =
        rewriter.getDenseI64ArrayAttr({static_cast<int64_t>(*recursiveIndex)});
    auto newCreate = createCompoundWithHoles(
        rewriter, create.getLoc(), create.getRcPtr().getType(), fields,
        create.getToken(), create.getRegion(), create.getVtableAttr(),
        create.getSkipRc(), create.getSkipFieldsAttr(), holeFields);
    llvm::SmallVector<mlir::Value> helperArgs(recursiveCall.getOperands());
    helperArgs.push_back(newCreate.getHoles().front());
    rewriter.create<mlir::func::CallOp>(recursiveCall.getLoc(), helperName,
                                        mlir::TypeRange{}, helperArgs);
    create.getRcPtr().replaceAllUsesWith(newCreate.getRcPtr());
    rewriter.eraseOp(create);
    if (recursiveCall.use_empty())
      rewriter.eraseOp(recursiveCall);
    return mlir::success();
  }

  auto create = llvm::dyn_cast<ReussirRcCreateVariantOp>(createOp);
  if (!create || create.getValue())
    return mlir::failure();

  auto recursiveIndex =
      findFirstRecursiveFieldIndex(create.getFields(), originalName);
  if (!recursiveIndex)
    return mlir::failure();

  auto recursiveCall = llvm::cast<mlir::func::CallOp>(
      create.getFields()[*recursiveIndex].getDefiningOp());
  if (!isSiteEligibleForTrmc(create.getOperation(), recursiveCall,
                             originalName))
    return mlir::failure();
  llvm::SmallVector<mlir::Value> fields(create.getFields().begin(),
                                        create.getFields().end());
  rewriter.setInsertionPoint(create);
  fields[*recursiveIndex] = rewriter.create<mlir::ub::PoisonOp>(
      create.getLoc(), fields[*recursiveIndex].getType());
  auto holeFields =
      rewriter.getDenseI64ArrayAttr({static_cast<int64_t>(*recursiveIndex)});
  auto newCreate = createVariantWithHoles(
      rewriter, create.getLoc(), create.getRcPtr().getType(),
      create.getTagAttr(), create.getValue(), fields, create.getToken(),
      create.getRegion(), create.getVtableAttr(), create.getSkipRc(),
      create.getSkipFieldsAttr(), holeFields);
  llvm::SmallVector<mlir::Value> helperArgs(recursiveCall.getOperands());
  helperArgs.push_back(newCreate.getHoles().front());
  rewriter.create<mlir::func::CallOp>(recursiveCall.getLoc(), helperName,
                                      mlir::TypeRange{}, helperArgs);
  create.getRcPtr().replaceAllUsesWith(newCreate.getRcPtr());
  rewriter.eraseOp(create);
  if (recursiveCall.use_empty())
    rewriter.eraseOp(recursiveCall);
  return mlir::success();
}

static bool rewriteRecursiveCreateSitesInFunction(mlir::func::FuncOp funcOp,
                                                  llvm::StringRef originalName,
                                                  llvm::StringRef helperName);

static void eraseUnusedCallsTo(mlir::func::FuncOp funcOp,
                               llvm::StringRef callee) {
  mlir::IRRewriter rewriter(funcOp.getContext());
  llvm::SmallVector<mlir::func::CallOp> calls;
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() == callee && callOp->use_empty())
      calls.push_back(callOp);
  });

  for (mlir::func::CallOp callOp : calls)
    rewriter.eraseOp(callOp);
}

static mlir::LogicalResult lowerValueIntoHole(mlir::Operation *terminator,
                                              mlir::Value value,
                                              TrmcRewriteContext &ctx) {
  if (succeeded(lowerStructuredValue(terminator, value, ctx)))
    return mlir::success();

  if (auto callOp =
          llvm::dyn_cast_if_present<mlir::func::CallOp>(value.getDefiningOp());
      callOp && succeeded(emitDirectRecursiveTailCall(terminator, callOp, ctx)))
    return mlir::success();

  if (succeeded(emitRecursiveCreateIntoHole(terminator, value, ctx)))
    return mlir::success();

  ctx.rewriter.setInsertionPoint(terminator);
  ctx.rewriter.create<ReussirHoleStoreOp>(terminator->getLoc(), ctx.outHole,
                                          value);
  return replaceWithVoidTerminator(ctx.rewriter, terminator);
}

static void makeHelperInternal(mlir::func::FuncOp helperFunc,
                               mlir::MLIRContext *context) {
  helperFunc.setPrivate();
  helperFunc->removeAttr("llvm.visibility");
  helperFunc->setAttr(
      "llvm.linkage",
      mlir::LLVM::LinkageAttr::get(context, mlir::LLVM::Linkage::Internal));
}

static mlir::FailureOr<mlir::func::FuncOp>
createTrmcHelper(mlir::func::FuncOp funcOp, mlir::SymbolTable &symbolTable) {
  auto *context = funcOp.getContext();
  std::string helperName = (funcOp.getName() + kTrmcSuffix).str();
  if (auto existing = symbolTable.lookup<mlir::func::FuncOp>(helperName))
    return existing;

  auto helperFunc = llvm::cast<mlir::func::FuncOp>(funcOp->clone());
  auto resultType = llvm::dyn_cast<RcType>(funcOp.getResultTypes().front());
  if (!resultType)
    return mlir::failure();

  auto holeType = HoleType::get(context, resultType);
  llvm::SmallVector<mlir::Type> inputs(helperFunc.getArgumentTypes().begin(),
                                       helperFunc.getArgumentTypes().end());
  inputs.push_back(holeType);
  helperFunc.setName(helperName);
  helperFunc.setFunctionType(
      mlir::FunctionType::get(context, inputs, llvm::ArrayRef<mlir::Type>{}));
  helperFunc.getBody().front().addArgument(holeType, helperFunc.getLoc());
  makeHelperInternal(helperFunc, context);

  mlir::IRRewriter rewriter(context);
  TrmcRewriteContext ctx{
      rewriter,
      helperFunc,
      helperFunc.getArgument(helperFunc.getNumArguments() - 1),
      funcOp.getName(),
      helperFunc.getName(),
      false};

  llvm::SmallVector<mlir::func::ReturnOp> returns;
  helperFunc.walk(
      [&](mlir::func::ReturnOp returnOp) { returns.push_back(returnOp); });
  for (mlir::func::ReturnOp returnOp : returns) {
    if (returnOp.getNumOperands() != 1 ||
        failed(lowerValueIntoHole(returnOp, returnOp.getOperand(0), ctx)))
      return mlir::failure();
  }

  eraseUnusedCallsTo(helperFunc, funcOp.getName());

  if (!ctx.rewroteRecursiveSite)
    return mlir::failure();
  symbolTable.insert(helperFunc);
  return helperFunc;
}

static bool rewriteRecursiveCreateSitesInFunction(mlir::func::FuncOp funcOp,
                                                  llvm::StringRef originalName,
                                                  llvm::StringRef helperName) {
  mlir::IRRewriter rewriter(funcOp.getContext());
  llvm::SmallVector<mlir::Operation *> creates;
  funcOp.walk([&](mlir::Operation *op) {
    if (llvm::isa<ReussirRcCreateCompoundOp, ReussirRcCreateVariantOp>(op))
      creates.push_back(op);
  });

  bool rewroteAny = false;
  for (mlir::Operation *op : creates) {
    if (!op->getBlock())
      continue;
    if (succeeded(
            rewriteRecursiveCreateSite(op, originalName, helperName, rewriter)))
      rewroteAny = true;
  }
  return rewroteAny;
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
      candidates.push_back(funcOp);
    });

    for (mlir::func::FuncOp funcOp : candidates) {
      auto helperOr = createTrmcHelper(funcOp, symbolTable);
      if (failed(helperOr))
        continue;
      if (!rewriteRecursiveCreateSitesInFunction(funcOp, funcOp.getName(),
                                                 helperOr->getName())) {
        helperOr->erase();
        continue;
      }
      eraseUnusedCallsTo(funcOp, funcOp.getName());
    }
  }
};

} // namespace
} // namespace reussir
