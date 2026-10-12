//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/LLVMPass/RetconDrop.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/Coroutines/CoroCleanup.h>
#include <llvm/Transforms/Coroutines/CoroEarly.h>
#include <llvm/Transforms/Coroutines/CoroSplit.h>

namespace reussir::llvmpass {
void lowerRetconDrops(llvm::Module &module) {
  if (llvm::none_of(module, [](const llvm::Function &function) {
        return function.hasFnAttribute("reussir.retcon_abi");
      }))
    return;
  llvm::PassBuilder builder;
  llvm::LoopAnalysisManager loops;
  llvm::FunctionAnalysisManager functions;
  llvm::CGSCCAnalysisManager graph;
  llvm::ModuleAnalysisManager modules;
  builder.registerLoopAnalyses(loops);
  builder.registerFunctionAnalyses(functions);
  builder.registerCGSCCAnalyses(graph);
  builder.registerModuleAnalyses(modules);
  builder.crossRegisterProxies(loops, functions, graph, modules);
  llvm::ModulePassManager passes;
  passes.addPass(llvm::CoroEarlyPass());
  passes.addPass(
      llvm::createModuleToPostOrderCGSCCPassAdaptor(llvm::CoroSplitPass()));
  passes.addPass(llvm::CoroCleanupPass());
  passes.addPass(RetconDropABIPass());
  passes.run(module, modules);
}

llvm::PreservedAnalyses RetconDropABIPass::run(llvm::Module &module,
                                               llvm::ModuleAnalysisManager &) {
  using namespace llvm;
  SmallVector<Function *> functions;
  for (auto &function : module)
    if (!function.isDeclaration() &&
        function.hasFnAttribute("reussir.retcon_abi"))
      functions.push_back(&function);
  if (functions.empty())
    return PreservedAnalyses::all();
  auto &ctx = module.getContext();
  auto *ptrTy = PointerType::getUnqual(ctx);
  SmallVector<std::pair<Function *, Function *>> callbacks;
  for (auto *function : functions) {
    function->removeFnAttr("reussir.retcon_abi");
    auto *type = FunctionType::get(
        Type::getVoidTy(ctx),
        {function->getArg(0)->getType(), function->getArg(1)->getType(), ptrTy},
        false);
    auto *wrapper = Function::Create(type, GlobalValue::PrivateLinkage,
                                     function->getName() + ".callback", module);
    auto attrs = function->getAttributes();
    wrapper->setAttributes(AttributeList::get(
        ctx, attrs.getFnAttrs(), {},
        {attrs.getParamAttrs(0), attrs.getParamAttrs(1), AttributeSet{}}));
    function->replaceAllUsesWith(wrapper);
    callbacks.emplace_back(function, wrapper);
  }
  for (auto [function, wrapper] : callbacks) {
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", wrapper));
    auto *step =
        builder.CreateCall(function, {wrapper->getArg(0), wrapper->getArg(1)});
    step->setCallingConv(function->getCallingConv());
    step->setAttributes(function->getAttributes());
    auto *resume = builder.CreateExtractValue(step, 0);
    auto *child = builder.CreateSelect(builder.CreateIsNotNull(resume),
                                       builder.CreateExtractValue(step, 1),
                                       ConstantPointerNull::get(ptrTy));
    auto *result = builder.CreateInsertValue(step, child, 1);
    builder.CreateStore(result, wrapper->getArg(2));
    builder.CreateRetVoid();
  }
  return PreservedAnalyses::none();
}
} // namespace reussir::llvmpass
