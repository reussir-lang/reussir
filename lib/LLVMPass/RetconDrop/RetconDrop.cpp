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
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/Coroutines/CoroCleanup.h>
#include <llvm/Transforms/Coroutines/CoroEarly.h>
#include <llvm/Transforms/Coroutines/CoroSplit.h>
#include <llvm/Transforms/Utils/Cloning.h>

namespace reussir::llvmpass {
void lowerRetconDrops(llvm::Module &module) {
  if (llvm::none_of(module, [](const llvm::Function &function) {
        return function.hasFnAttribute("reussir.retcon_drop");
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
  passes.addPass(RetconDropPass());
  passes.addPass(llvm::CoroEarlyPass());
  passes.addPass(
      llvm::createModuleToPostOrderCGSCCPassAdaptor(llvm::CoroSplitPass()));
  passes.addPass(llvm::CoroCleanupPass());
  passes.addPass(RetconDropABIPass());
  passes.run(module, modules);
}

llvm::PreservedAnalyses RetconDropPass::run(llvm::Module &module,
                                            llvm::ModuleAnalysisManager &) {
  using namespace llvm;
  SmallVector<Function *> functions;
  for (auto &function : module)
    if (!function.isDeclaration() &&
        function.hasFnAttribute("reussir.retcon_drop"))
      functions.push_back(&function);
  if (functions.empty())
    return PreservedAnalyses::all();

  auto &ctx = module.getContext();
  const auto &layout = module.getDataLayout();
  auto *ptrTy = PointerType::getUnqual(ctx);
  auto *sizeTy = layout.getIntPtrType(ctx);
  auto *stepTy = StructType::get(ctx, {ptrTy, ptrTy});
  auto *taskTy = StructType::get(ctx, {ptrTy, ptrTy});
  auto *routineTy = StructType::get(ctx, {ptrTy, sizeTy, sizeTy});
  auto *entryTy = FunctionType::get(stepTy, {ptrTy, ptrTy}, false);
  auto *resumeTy =
      FunctionType::get(stepTy, {ptrTy, Type::getInt1Ty(ctx)}, false);
  auto *resume = Function::Create(resumeTy, GlobalValue::PrivateLinkage,
                                  "__reussir_retcon_prototype", module);
  resume->addParamAttr(1, Attribute::ZExt);
  resume->addFnAttr("reussir.retcon_abi");
  for (auto *function : functions)
    for (auto attr : {Attribute::SanitizeAddress, Attribute::SanitizeMemory,
                      Attribute::SanitizeThread})
      if (function->hasFnAttribute(attr))
        resume->addFnAttr(attr);
  IRBuilder<> prototype(BasicBlock::Create(ctx, "entry", resume));
  prototype.CreateUnreachable();
  auto allocate =
      module.getOrInsertFunction("__reussir_allocate", ptrTy, sizeTy, sizeTy);
  auto deallocate = module.getOrInsertFunction("__reussir_dealloc_unsized",
                                               Type::getVoidTy(ctx), ptrTy);
  DenseMap<Function *, Function *> entries;
  DenseMap<Function *, GlobalVariable *> routines;
  for (auto *function : functions) {
    assert(function->arg_size() == 1 &&
           function->getArg(0)->getType()->isPointerTy() &&
           function->getReturnType()->isVoidTy());
    auto *entry = Function::Create(entryTy, GlobalValue::PrivateLinkage,
                                   function->getName() + ".start", module);
    entry->addFnAttr(Attribute::PresplitCoroutine);
    entry->addFnAttr(Attribute::NoUnwind);
    for (auto attr : {Attribute::SanitizeAddress, Attribute::SanitizeMemory,
                      Attribute::SanitizeThread})
      if (function->hasFnAttribute(attr))
        entry->addFnAttr(attr);
    entries[function] = entry;
    auto *initializer = ConstantStruct::get(
        routineTy,
        {entry, ConstantInt::get(sizeTy, layout.getPointerSize()),
         ConstantInt::get(sizeTy, layout.getPointerABIAlignment(0).value())});
    routines[function] =
        new GlobalVariable(module, routineTy, true, GlobalValue::PrivateLinkage,
                           initializer, function->getName() + ".routine");
  }

  for (auto *function : functions) {
    auto *entry = entries.lookup(function);
    ValueToValueMapTy mapping;
    mapping[function->getArg(0)] = entry->getArg(1);
    SmallVector<ReturnInst *> returns;
    CloneFunctionInto(entry, function, mapping,
                      CloneFunctionChangeType::LocalChangesOnly, returns);
    entry->setDSOLocal(true);
    entry->setVisibility(GlobalValue::DefaultVisibility);
    entry->setComdat(nullptr);
    entry->removeFnAttr("reussir.retcon_drop");
    entry->removeFnAttr(Attribute::NoCallback);
    entry->addFnAttr(Attribute::PresplitCoroutine);
    entry->addFnAttr("reussir.retcon_abi");
    entry->getArg(0)->setName("storage");
    entry->getArg(1)->setName("object");
    Align frameAlign = layout.getPointerABIAlignment(0);
    for (auto &instruction : instructions(entry)) {
      if (auto *alloca = dyn_cast<AllocaInst>(&instruction))
        frameAlign = std::max(frameAlign, alloca->getAlign());
      if (instruction.getType()->isSized())
        frameAlign =
            std::max(frameAlign, layout.getABITypeAlign(instruction.getType()));
    }
    auto *allocator = Function::Create(
        FunctionType::get(ptrTy, {sizeTy}, false), GlobalValue::PrivateLinkage,
        function->getName() + ".allocate", module);
    IRBuilder<> allocBuilder(BasicBlock::Create(ctx, "entry", allocator));
    allocBuilder.CreateRet(allocBuilder.CreateCall(
        allocate,
        {ConstantInt::get(sizeTy, frameAlign.value()), allocator->getArg(0)}));

    IRBuilder<> builder(&*entry->getEntryBlock().getFirstInsertionPt());
    auto *task = builder.CreateAlloca(taskTy, nullptr, "child.task");
    auto *id = builder.CreateIntrinsic(
        Intrinsic::coro_id_retcon, {},
        {builder.getInt32(layout.getPointerSize()),
         builder.getInt32(layout.getPointerABIAlignment(0).value()),
         entry->getArg(0), resume, allocator, deallocate.getCallee()});
    auto *handle = builder.CreateIntrinsic(
        Intrinsic::coro_begin, {}, {id, ConstantPointerNull::get(ptrTy)});
    SmallVector<CallInst *> calls;
    for (auto &instruction : instructions(entry))
      if (auto *call = dyn_cast<CallInst>(&instruction))
        if (routines.contains(call->getCalledFunction()))
          calls.push_back(call);
    for (auto *call : calls) {
      builder.SetInsertPoint(call);
      builder.CreateStore(routines.lookup(call->getCalledFunction()),
                          builder.CreateStructGEP(taskTy, task, 0));
      builder.CreateStore(call->getArgOperand(0),
                          builder.CreateStructGEP(taskTy, task, 1));
      builder.CreateIntrinsic(Intrinsic::coro_suspend_retcon,
                              {builder.getInt1Ty()}, {task});
      call->eraseFromParent();
    }
    for (auto *ret : returns) {
      builder.SetInsertPoint(ret);
      builder.CreateIntrinsic(
          Intrinsic::coro_end, {},
          {handle, builder.getFalse(), ConstantTokenNone::get(ctx)});
      builder.CreateUnreachable();
      ret->eraseFromParent();
    }
  }

  auto executorSize =
      module.getOrInsertFunction("__reussir_dtor_executor_size", sizeTy);
  auto executorAlign =
      module.getOrInsertFunction("__reussir_dtor_executor_align", sizeTy);
  auto create = module.getOrInsertFunction("__reussir_create_dtor_executor",
                                           ptrTy, ptrTy);
  auto destroy = module.getOrInsertFunction("__reussir_destroy_dtor_executor",
                                            Type::getVoidTy(ctx), ptrTy);
  auto run = module.getOrInsertFunction(
      "__reussir_run_dtor", Type::getVoidTy(ctx), ptrTy, ptrTy, ptrTy);
  for (auto *function : functions) {
    auto linkage = function->getLinkage();
    function->deleteBody();
    function->setLinkage(linkage);
    function->removeFnAttr("reussir.retcon_drop");
    function->removeFnAttr(Attribute::NoCallback);
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", function));
    auto *size = builder.CreateCall(executorSize);
    auto *align = builder.CreateCall(executorAlign);
    auto *padding = builder.CreateSub(align, ConstantInt::get(sizeTy, 1));
    auto *storage = builder.CreateAlloca(builder.getInt8Ty(),
                                         builder.CreateAdd(size, padding));
    auto *address =
        builder.CreateAdd(builder.CreatePtrToInt(storage, sizeTy), padding);
    auto *aligned = builder.CreateIntToPtr(
        builder.CreateAnd(address, builder.CreateNeg(align)), ptrTy);
    auto *executor = builder.CreateCall(create, {aligned});
    builder.CreateCall(
        run, {executor, function->getArg(0), routines.lookup(function)});
    builder.CreateCall(destroy, {executor});
    builder.CreateRetVoid();
  }
  return PreservedAnalyses::none();
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
  DenseMap<Function *, Function *> wrappers;
  for (auto *function : functions) {
    function->removeFnAttr("reussir.retcon_abi");
    auto *type = FunctionType::get(
        Type::getVoidTy(ctx),
        {function->getArg(0)->getType(), function->getArg(1)->getType(), ptrTy},
        false);
    auto *wrapper = Function::Create(type, GlobalValue::PrivateLinkage,
                                     function->getName() + ".callback", module);
    wrapper->setAttributes(function->getAttributes());
    function->replaceAllUsesWith(wrapper);
    wrappers[function] = wrapper;
  }
  for (auto *function : functions) {
    auto *wrapper = wrappers.lookup(function);
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", wrapper));
    auto *step =
        builder.CreateCall(function, {wrapper->getArg(0), wrapper->getArg(1)});
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
