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
    id->setArgOperand(
        0, llvm::ConstantInt::get(id->getArgOperand(0)->getType(), size));
    id->setArgOperand(
        1, llvm::ConstantInt::get(id->getArgOperand(1)->getType(), align));
    Shape.RetconLowering.IsFrameInlineInStorage = true;
    F.removeFnAttr("reussir.retcon_routine");
  }
};

void lowerLazyWrapper(llvm::Function &wrapper) {
  using namespace llvm;
  auto &module = *wrapper.getParent();
  auto name = wrapper.getFnAttribute("reussir.retcon_wrapper").getValueAsString();
  auto *routine = module.getNamedGlobal(name);
  if (!routine || !routine->hasInitializer())
    report_fatal_error("missing destructor wrapper descriptor");
  auto *descriptor = cast<ConstantStruct>(routine->getInitializer());
  auto size = cast<ConstantInt>(descriptor->getOperand(1))->getZExtValue();
  auto align = cast<ConstantInt>(descriptor->getOperand(2))->getZExtValue();
  wrapper.removeFnAttr("reussir.retcon_wrapper");
  // Bound native stack use. Large or over-aligned frames retain the original
  // arena-backed entry; recursive children always use the explicit executor.
  if (size > 256 || align > 16)
    return;

  // Keep the address-taken frame out of recursive callers. Inlining this
  // wrapper before tail-recursion elimination can prevent their stack reuse.
  wrapper.addFnAttr(Attribute::NoInline);
  auto *start = cast<Function>(descriptor->getOperand(0));
  auto &ctx = module.getContext();
  auto *ptr = PointerType::getUnqual(ctx);
  auto *stepType = StructType::get(ctx, {ptr, ptr});
  auto finish = module.getOrInsertFunction(
      "__reussir_finish_dtor",
      FunctionType::get(Type::getVoidTy(ctx), {ptr, ptr, ptr}, false));
  auto linkage = wrapper.getLinkage();
  wrapper.deleteBody();
  wrapper.setLinkage(linkage);
  auto *entry = BasicBlock::Create(ctx, "entry", &wrapper);
  auto *suspended = BasicBlock::Create(ctx, "suspended", &wrapper);
  auto *done = BasicBlock::Create(ctx, "done", &wrapper);
  IRBuilder<> builder(entry);
  auto *buffer = builder.CreateAlloca(ArrayType::get(builder.getInt8Ty(), size),
                                     nullptr, "frame");
  buffer->setAlignment(Align(align));
  auto *result = builder.CreateAlloca(stepType, nullptr, "step");
  // A direct call exposes a non-suspending root to ordinary LLVM inlining.
  builder.CreateCall(start, {buffer, wrapper.getArg(0), result});
  auto *step = builder.CreateLoad(stepType, result);
  auto *resume = builder.CreateExtractValue(step, 0);
  builder.CreateCondBr(builder.CreateIsNotNull(resume), suspended, done);
  builder.SetInsertPoint(suspended);
  builder.CreateCall(finish, {buffer, resume, builder.CreateExtractValue(step, 1)});
  builder.CreateBr(done);
  builder.SetInsertPoint(done);
  builder.CreateRetVoid();
}
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
  for (auto &function : module)
    if (function.hasFnAttribute("reussir.retcon_wrapper"))
      lowerLazyWrapper(function);
  return PreservedAnalyses::none();
}
} // namespace reussir::llvmpass
