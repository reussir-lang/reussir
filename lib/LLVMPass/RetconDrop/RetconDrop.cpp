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
#include <llvm/Transforms/Coroutines/MaterializationUtils.h>

namespace reussir::llvmpass {
namespace {
class DtorRetconABI : public llvm::coro::AnyRetconABI {
public:
  DtorRetconABI(llvm::Function &function, llvm::coro::Shape &shape)
      : AnyRetconABI(function, shape, llvm::coro::isTriviallyMaterializable) {}

  void buildCoroutineFrame(bool optimizeFrame) override {
    AnyRetconABI::buildCoroutineFrame(optimizeFrame);
    auto name = F.getFnAttribute("reussir.retcon_routine").getValueAsString();
    auto *routine = F.getParent()->getNamedGlobal(name);
    if (!routine || !routine->hasInitializer())
      llvm::report_fatal_error("missing destructor routine descriptor");
    auto *init = llvm::cast<llvm::ConstantStruct>(routine->getInitializer());
    auto *id = Shape.getRetconCoroId();
    uint64_t size = std::max(Shape.FrameSize, id->getStorageSize());
    uint64_t align =
        std::max(Shape.FrameAlign.value(), id->getStorageAlignment().value());
    if (!llvm::isUInt<32>(size) || !llvm::isUInt<32>(align))
      llvm::report_fatal_error(
          "destructor coroutine frame exceeds retcon limits");
    routine->setInitializer(llvm::ConstantStruct::get(
        init->getType(), init->getOperand(0),
        llvm::ConstantInt::get(init->getOperand(1)->getType(), size),
        llvm::ConstantInt::get(init->getOperand(2)->getType(), align)));
    routine->setConstant(true);
    id->setArgOperand(
        0, llvm::ConstantInt::get(id->getArgOperand(0)->getType(), size));
    id->setArgOperand(
        1, llvm::ConstantInt::get(id->getArgOperand(1)->getType(), align));
    Shape.RetconLowering.IsFrameInlineInStorage = true;
    F.removeFnAttr("reussir.retcon_routine");
  }
};

} // namespace

void populateRetconDropPasses(llvm::ModulePassManager &passes) {
  llvm::SmallVector<llvm::CoroSplitPass::BaseABITy> abis{
      [](llvm::Function &function, llvm::coro::Shape &shape) {
        return std::make_unique<DtorRetconABI>(function, shape);
      }};
  passes.addPass(llvm::CoroEarlyPass());
  passes.addPass(llvm::createModuleToPostOrderCGSCCPassAdaptor(
      llvm::CoroSplitPass(std::move(abis))));
  passes.addPass(llvm::CoroCleanupPass());
  passes.addPass(RetconDropABIPass());
}

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
  populateRetconDropPasses(passes);
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
    // MLIR emits direct root calls using retcon's internal result type. Only
    // address-taken entries need the runtime C ABI; preserve direct calls so
    // LLVM can inline them without an output-parameter round trip.
    SmallVector<CallBase *> directCalls;
    for (auto *user : function->users())
      if (auto *call = dyn_cast<CallBase>(user))
        if (call->getCalledFunction() == function)
          directCalls.push_back(call);
    function->replaceAllUsesWith(wrapper);
    for (auto *call : directCalls)
      call->setCalledFunction(function);
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
