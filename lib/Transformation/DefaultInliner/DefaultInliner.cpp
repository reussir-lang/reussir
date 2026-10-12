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
/// This file implements the Reussir default inliner pass: MLIR's SCC inliner
/// driven directly through `mlir::Inliner` with a programmatic
/// `InlinerConfig`, rather than through `createInlinerPass` +
/// `initializeOptions` string parsing. The textual route resolves its
/// `default-pipeline` through the global pass registry — forcing the driver
/// to register upstream passes it otherwise never names — and re-parses the
/// policy from strings on every construction; building the config in C++
/// keeps the policy typed, registry-free, and in one place.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Transformation/Passes.h"

#include <utility>

#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SCCIterator.h>
#include <mlir/Analysis/CallGraph.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/CallInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>
#include <mlir/Transforms/Inliner.h>
#include <mlir/Transforms/Passes.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRDEFAULTINLINERPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

// The callable optimizer between inlining rounds: canonicalize with region
// simplification disabled, matching the rest of the pipeline's
// canonicalization policy.
void addCanonicalizerWithoutRegionSimplification(mlir::OpPassManager &pm) {
  mlir::GreedyRewriteConfig config;
  config.setRegionSimplificationLevel(
      mlir::GreedySimplifyRegionLevel::Disabled);
  pm.addPass(mlir::createCanonicalizerPass(config));
}

// Whether the callable region holds at most `limit` operations, counted
// recursively through nested regions. The walk stops as soon as the limit
// is crossed, so a query costs O(limit) however large the callee is.
bool calleeIsSmall(mlir::Region *callable, unsigned limit) {
  if (!callable)
    return false;
  unsigned count = 0;
  auto result = callable->walk([&](mlir::Operation *) {
    return ++count > limit ? mlir::WalkResult::interrupt()
                           : mlir::WalkResult::advance();
  });
  return !result.wasInterrupted();
}

// Marks a call with the name of the function it is written in, so that a
// call an inlining copied in (still marked with the function it came from)
// can be told from the calls of the function itself.
constexpr llvm::StringLiteral kHomeAttr = "reussir.inliner_home";

bool copiedByInlining(mlir::Operation *call) {
  auto home = call->getAttrOfType<mlir::StringAttr>(kHomeAttr);
  auto function = call->getParentOfType<mlir::SymbolOpInterface>();
  return home && function && home != function.getNameAttr();
}

struct DefaultInlinerPass
    : public impl::ReussirDefaultInlinerPassBase<DefaultInlinerPass> {
  using Base::Base;
  void runOnOperation() override {
    // Limit iterations across each SCC. The profitability rule below also
    // limits recursive expansion through calls introduced within a round.
    const mlir::InlinerConfig config(addCanonicalizerWithoutRegionSimplification,
                                     /*maxInliningIterations=*/1);
    mlir::CallGraph &callGraph = getAnalysis<mlir::CallGraph>();
    // The callables on a cycle of the call graph.
    llvm::DenseSet<const mlir::CallGraphNode *> recursive;
    for (auto scc = llvm::scc_begin(&std::as_const(callGraph)); !scc.isAtEnd();
         ++scc)
      if (scc.hasCycle())
        recursive.insert(scc->begin(), scc->end());
    getOperation()->walk([](mlir::CallOpInterface call) {
      if (auto function = call->getParentOfType<mlir::SymbolOpInterface>())
        call->setAttr(kHomeAttr, function.getNameAttr());
    });
    // Nested (callable-optimizer) pipelines run through the pass's own
    // instrumented pipeline runner. Profitability caps the callee's size:
    // bottom-up processing means an uncapped policy re-copies each level's
    // fully-inlined bodies into every caller, growing the module
    // geometrically in call-tree depth (see the pass description). Within
    // its one iteration, the inliner also inlines the calls an inlining
    // copied in. Its history prevents repeated callees along a chain, but
    // distinct members of a recursive SCC can still expand within a round.
    // Decline introduced calls into these SCCs; this is a growth heuristic,
    // not a global size bound.
    mlir::Inliner inliner(
        getOperation(), callGraph, *this, getAnalysisManager(),
        [this](mlir::Pass &, mlir::OpPassManager &pipeline,
               mlir::Operation *op) { return runPipeline(pipeline, op); },
        config, [&](const mlir::Inliner::ResolvedCall &call) {
          if (!calleeIsSmall(call.targetNode->getCallableRegion(),
                             maxCalleeOps))
            return false;
          mlir::CallOpInterface callOp = call.call;
          return !recursive.contains(call.targetNode) ||
                 !copiedByInlining(callOp.getOperation());
        });
    if (mlir::failed(inliner.doInlining()))
      signalPassFailure();
    getOperation()->walk(
        [](mlir::CallOpInterface call) { call->removeAttr(kHomeAttr); });
  }
};

} // namespace
} // namespace reussir
