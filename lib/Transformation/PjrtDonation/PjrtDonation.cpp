// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include "Reussir/Analysis/PjrtDonationAnalysis.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Transformation/Passes.h"
#include <llvm/ADT/DenseMap.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/SCF/IR/SCF.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRPJRTDONATIONPASS
#include "Reussir/Transformation/Passes.h.inc"
namespace {
using llvm::SmallVector;
using Decision = PjrtDonationAnalysis::Decision;
struct CallPlan {
  ReussirPJRTJitCallOp call;
  SmallVector<mlir::Value> owners;
  SmallVector<Decision> decisions;
  SmallVector<int32_t> mapping;
  SmallVector<bool> explicitDonors;
};
bool unsharedIterationValue(mlir::Value value) {
  if (!llvm::isa_and_nonnull<ReussirArrayFromIfrtOp, ReussirArrayToDeviceOp>(
          value.getDefiningOp()))
    return false;
  return llvm::all_of(value.getUsers(), [](mlir::Operation *op) {
    return llvm::isa<mlir::scf::YieldOp, ReussirArrayToIfrtOp,
                     ReussirArrayToHostOp, ReussirRcIsUniqueOp>(op);
  });
}

void carryLoopGuards(mlir::ModuleOp module) {
  SmallVector<mlir::Operation *> loops;
  module.walk<mlir::WalkOrder::PostOrder>([&](mlir::Operation *op) {
    if (llvm::isa<mlir::scf::ForOp, mlir::scf::WhileOp>(op))
      loops.push_back(op);
  });
  for (auto *operation : loops) {
    auto forOp = llvm::dyn_cast<mlir::scf::ForOp>(operation);
    auto whileOp = llvm::dyn_cast<mlir::scf::WhileOp>(operation);
    auto arguments =
        forOp ? forOp.getRegionIterArgs() : whileOp.getAfterArguments();
    auto yield = llvm::dyn_cast<mlir::scf::YieldOp>(
        forOp ? forOp.getBody()->getTerminator()
              : whileOp.getAfter().front().getTerminator());
    if (!yield)
      continue;
    SmallVector<unsigned> carried;
    SmallVector<SmallVector<ReussirRcIsUniqueOp, 2>, 2> checks;
    for (auto [i, arg] : llvm::enumerate(arguments)) {
      if (i >= yield.getNumOperands() ||
          !unsharedIterationValue(yield.getOperand(i)))
        continue;
      if (whileOp && (i >= whileOp.getBeforeArguments().size() ||
                      whileOp.getConditionOp().getArgs()[i] !=
                          whileOp.getBeforeArguments()[i]))
        continue;
      if (whileOp &&
          !llvm::all_of(
              whileOp.getBeforeArguments()[i].getUsers(),
              [&](mlir::Operation *user) {
                return user == whileOp.getConditionOp().getOperation() ||
                       llvm::isa<ReussirArrayToHostOp, ReussirRcIsUniqueOp>(
                           user);
              }))
        continue;
      SmallVector<ReussirRcIsUniqueOp, 2> found;
      bool safe = true;
      for (auto *user : arg.getUsers()) {
        if (auto check = llvm::dyn_cast<ReussirRcIsUniqueOp>(user);
            check && check->hasAttr("pjrt.donation_guard"))
          found.push_back(check);
        else if (!llvm::isa<ReussirArrayToIfrtOp, ReussirArrayToHostOp,
                            ReussirRcDecOp>(user))
          safe = false;
      }
      if (safe && !found.empty()) {
        carried.push_back(i);
        checks.push_back(std::move(found));
      }
    }
    if (carried.empty())
      continue;
    mlir::OpBuilder builder(operation);
    auto loc = operation->getLoc();
    SmallVector<mlir::Value> inits(forOp ? forOp.getInitArgs()
                                         : whileOp.getInits());
    for (unsigned i : carried)
      inits.push_back(ReussirRcIsUniqueOp::create(
          builder, loc, builder.getI1Type(), inits[i]));
    SmallVector<mlir::Value> replacements;
    mlir::Block *body;
    if (forOp) {
      auto next = mlir::scf::ForOp::create(builder, loc, forOp.getLowerBound(),
                                           forOp.getUpperBound(),
                                           forOp.getStep(), inits);
      next->setDiscardableAttrs(forOp->getDiscardableAttrDictionary());
      next.getRegion().takeBody(forOp.getRegion());
      body = next.getBody();
      llvm::append_range(replacements,
                         next.getResults().take_front(forOp.getNumResults()));
    } else {
      SmallVector<mlir::Type> types(whileOp.getResultTypes());
      types.append(carried.size(), builder.getI1Type());
      auto next = mlir::scf::WhileOp::create(builder, loc, types, inits);
      next->setDiscardableAttrs(whileOp->getDiscardableAttrDictionary());
      next.getBefore().takeBody(whileOp.getBefore());
      next.getAfter().takeBody(whileOp.getAfter());
      auto condition = next.getConditionOp();
      for (unsigned i = 0; i < carried.size(); ++i) {
        auto flag =
            next.getBefore().front().addArgument(builder.getI1Type(), loc);
        condition.getArgsMutable().append(flag);
      }
      body = &next.getAfter().front();
      llvm::append_range(replacements,
                         next.getResults().take_front(whileOp.getNumResults()));
    }
    for (auto &group : checks) {
      auto flag = body->addArgument(builder.getI1Type(), loc);
      for (auto check : group) {
        check.replaceAllUsesWith(flag);
        check.erase();
      }
      builder.setInsertionPoint(yield);
      auto next = mlir::arith::ConstantIntOp::create(builder, loc, 1, 1);
      yield.getResultsMutable().append(next.getResult());
    }
    operation->replaceAllUsesWith(replacements);
    operation->erase();
  }
  module.walk(
      [](ReussirRcIsUniqueOp op) { op->removeAttr("pjrt.donation_guard"); });
}

struct ReussirPjrtDonationPass
    : impl::ReussirPjrtDonationPassBase<ReussirPjrtDonationPass> {
  using Base::Base;
  void runOnOperation() override {
    auto module = getOperation();
    SmallVector<ReussirPJRTJitCallOp> calls;
    module.walk([&](ReussirPJRTJitCallOp call) {
      if (!call->hasAttr("pjrt.donation_planned"))
        calls.push_back(call);
    });
    if (calls.empty())
      return;
    PjrtDonationAnalysis analysis(module, maxAnalysisWork);
    SmallVector<CallPlan, 0> plans;
    // Finish ownership queries before introducing any guards or new views.
    for (auto call : calls) {
      CallPlan plan;
      plan.call = call;
      plan.explicitDonors.resize(call.getInputs().size(), false);
      for (int32_t i : call.getDonatedInputIndices())
        plan.explicitDonors[i] = true;
      for (auto attr : call.getIoAliases())
        plan.explicitDonors[llvm::cast<mlir::DenseI32ArrayAttr>(attr)[0]] =
            true;
      bool explicitContract = llvm::is_contained(plan.explicitDonors, true);
      llvm::DenseMap<mlir::Value, int32_t> representatives;
      for (auto [i, input] : llvm::enumerate(call.getInputs())) {
        auto view = input.getDefiningOp<ReussirArrayToIfrtOp>();
        auto owner = view ? view.getArray() : mlir::Value{};
        plan.owners.push_back(owner);
        auto decision = Decision::Forbidden;
        if (inferDonation && !explicitContract &&
            !call->hasAttr("reussir.disable_donation") && owner)
          decision = analysis.analyze(owner, call);
        plan.decisions.push_back(decision);
        auto [it, inserted] =
            representatives.try_emplace(owner ? owner : input, i);
        bool keep =
            explicitContract || inserted ||
            call.getInputs()[it->second].getType() != input.getType() ||
            (call.getArgAttrs() &&
             (*call.getArgAttrs())[i] != (*call.getArgAttrs())[it->second]);
        plan.mapping.push_back(keep ? int32_t(i) : it->second);
      }
      plans.push_back(std::move(plan));
    }
    analysisWork += analysis.getWork();
    if (analysis.exhausted())
      ++exhaustedAnalyses;
    for (auto &plan : plans) {
      mlir::OpBuilder builder(plan.call);
      SmallVector<mlir::Value> guards;
      llvm::DenseMap<mlir::Value, mlir::Value> ownerGuards;
      for (unsigned i = 0; i < plan.owners.size(); ++i) {
        auto owner = plan.owners[i];
        auto decision = plan.decisions[i];
        bool explicitDonor = plan.explicitDonors[i];
        mlir::Value guard;
        if (decision == Decision::Forbidden && !explicitDonor)
          guard = mlir::arith::ConstantIntOp::create(builder,
                                                     plan.call.getLoc(), 0, 1);
        else if (!owner) {
          // Raw IFRT explicit contracts already carry the caller's ownership
          // promise. RC-backed contracts additionally check live uniqueness.
          guard = mlir::arith::ConstantIntOp::create(builder,
                                                     plan.call.getLoc(), 1, 1);
        } else {
          auto found = ownerGuards.find(owner);
          if (found != ownerGuards.end())
            guard = found->second;
          else if (decision == Decision::Unconditional && !explicitDonor) {
            guard = mlir::arith::ConstantIntOp::create(
                builder, plan.call.getLoc(), 1, 1);
            ++staticDonors;
          } else {
            guard = ReussirRcIsUniqueOp::create(builder, plan.call.getLoc(),
                                                builder.getI1Type(), owner);
            guard.getDefiningOp()->setAttr("pjrt.donation_guard",
                                           builder.getUnitAttr());
            ++guardedDonors;
          }
          ownerGuards[owner] = guard;
          auto original =
              plan.call.getInputs()[i].getDefiningOp<ReussirArrayToIfrtOp>();
          if (!original.getMayDonate()) {
            auto view = ReussirArrayToIfrtOp::create(
                builder, plan.call.getLoc(), plan.call.getInputs()[i].getType(),
                owner, builder.getUnitAttr());
            plan.call.getInputsMutable().slice(i, 1).assign(view.getView());
          }
        }
        guards.push_back(guard);
      }
      plan.call.getDonationGuardsMutable().assign(guards);
      plan.call.setDonationMappingAttr(
          builder.getDenseI32ArrayAttr(plan.mapping));
      plan.call->setAttr("pjrt.donation_planned", builder.getUnitAttr());
    }
    carryLoopGuards(module);
  }
};
} // namespace
} // namespace reussir
