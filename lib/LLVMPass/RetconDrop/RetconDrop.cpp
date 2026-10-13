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
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
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
    if (function->getArg(1)->getType()->isIntegerTy(1)) {
      // Retcon's continuation pointer becomes a runtime action descriptor.
      // Starts remain callable through DtorRoutine. Keep the callback until the
      // normal optimizer exposes any free-only completion body.
      auto *sizeTy = module.getDataLayout().getIntPtrType(ctx);
      auto *actionType = StructType::get(ctx, {ptrTy, sizeTy});
      auto *action = new GlobalVariable(
          module, actionType, true, GlobalValue::PrivateLinkage,
          ConstantStruct::get(actionType, wrapper, ConstantInt::get(sizeTy, 0)),
          function->getName() + ".action");
      action->setMetadata("reussir.dtor.resume", MDNode::get(ctx, {}));
      function->replaceAllUsesWith(action);
    } else {
      function->replaceAllUsesWith(wrapper);
    }
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
namespace {
// This optimization must follow LLVM CoroSplit and ordinary LLVM cleanup:
// resume functions and the spilled payload offsets do not exist in MLIR.
// Root control flow and the stack-frame policy remain in MLIR; this pass only
// classifies generated callbacks and finalizes their runtime action
// descriptors. Match only a straight-line completion callback. In particular,
// do not move frees across other effects, discard volatile/atomic accesses, or
// turn a conditional free into an unconditional one. The runtime performs the
// frame load at resume time, rather than capturing its value when the child
// yields.
std::optional<uint64_t> getDeferredFreeOffset(llvm::Function &function) {
  using namespace llvm;
  if (function.size() != 1 || function.arg_size() != 3 ||
      !function.getReturnType()->isVoidTy() ||
      !function.getArg(0)->getType()->isPointerTy() ||
      !function.getArg(1)->getType()->isIntegerTy(1) ||
      !function.getArg(2)->getType()->isPointerTy())
    return std::nullopt;
  const auto &layout = function.getParent()->getDataLayout();
  auto pointerSize = layout.getPointerSize();
  CallInst *deallocation = nullptr;
  bool completes = false;
  for (auto &instruction : function.getEntryBlock()) {
    if (auto *call = dyn_cast<CallInst>(&instruction)) {
      auto *callee = call->getCalledFunction();
      if (!callee || call->hasOperandBundles())
        return std::nullopt;
      if (callee->getName() == "__reussir_dealloc_unsized") {
        if (deallocation || call->arg_size() != 1 ||
            !call->getType()->isVoidTy())
          return std::nullopt;
        deallocation = call;
        continue;
      }
      if (auto *zero = dyn_cast<MemSetInst>(call)) {
        auto *length = dyn_cast<ConstantInt>(zero->getLength());
        auto *value = dyn_cast<ConstantInt>(zero->getValue());
        if (zero->isVolatile() || zero->getDest() != function.getArg(2) ||
            !length || length->getZExtValue() != 2 * pointerSize || !value ||
            !value->isZero())
          return std::nullopt;
        completes = true;
        continue;
      }
      if (callee->isIntrinsic() &&
          (callee->getIntrinsicID() == Intrinsic::assume ||
           callee->getIntrinsicID() ==
               Intrinsic::experimental_noalias_scope_decl))
        continue;
      return std::nullopt;
    }
    if (auto *load = dyn_cast<LoadInst>(&instruction)) {
      if (load->isVolatile() || load->isAtomic())
        return std::nullopt;
      continue;
    }
    if (isa<GetElementPtrInst, ReturnInst>(instruction))
      continue;
    if (auto *store = dyn_cast<StoreInst>(&instruction)) {
      auto *value = dyn_cast<Constant>(store->getValueOperand());
      int64_t offset = 0;
      auto *base = GetPointerBaseWithConstantOffset(store->getPointerOperand(),
                                                    offset, layout);
      if (store->isVolatile() || store->isAtomic() || !value ||
          !value->isNullValue() || base != function.getArg(2) ||
          (offset != 0 && offset != pointerSize) ||
          layout.getTypeStoreSize(value->getType()) >
              2 * pointerSize - static_cast<uint64_t>(offset))
        return std::nullopt;
      // Require a complete null pointer in the resume field, not a partial
      // byte store or a zero in the child field alone.
      if (offset == 0 &&
          layout.getTypeStoreSize(value->getType()) >= pointerSize)
        completes = true;
      continue;
    }
    return std::nullopt;
  }
  if (!deallocation || !completes)
    return std::nullopt;
  auto *payload = dyn_cast<LoadInst>(deallocation->getArgOperand(0));
  if (!payload || !payload->getType()->isPointerTy() ||
      payload->getType()->getPointerAddressSpace() != 0)
    return std::nullopt;
  int64_t offset = 0;
  auto *base = GetPointerBaseWithConstantOffset(payload->getPointerOperand(),
                                                offset, layout);
  if (base != function.getArg(0) || offset < 0)
    return std::nullopt;
  return offset;
}
} // namespace

llvm::PreservedAnalyses
RetconDropDeferredFreePass::run(llvm::Module &module,
                                llvm::ModuleAnalysisManager &) {
  using namespace llvm;
  bool changed = false;
  for (auto &action : module.globals()) {
    if (!action.getMetadata("reussir.dtor.resume") || !action.isConstant() ||
        !action.hasInitializer())
      continue;
    auto *descriptor = dyn_cast<ConstantStruct>(action.getInitializer());
    if (!descriptor || descriptor->getNumOperands() != 2)
      continue;
    auto *callback = dyn_cast<Function>(descriptor->getOperand(0));
    if (!callback)
      continue;
    auto offset = getDeferredFreeOffset(*callback);
    if (!offset)
      continue;
    action.setInitializer(ConstantStruct::get(
        descriptor->getType(),
        ConstantPointerNull::get(PointerType::getUnqual(module.getContext())),
        ConstantInt::get(descriptor->getOperand(1)->getType(), *offset)));
    changed = true;
  }
  return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
} // namespace reussir::llvmpass
