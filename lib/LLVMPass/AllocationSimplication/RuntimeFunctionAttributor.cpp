//===- RuntimeFunctionAttributor.cpp - Reussir runtime attrib pass ------===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/LLVMPass/RuntimeFunctionAttributor.h"

#include <optional>

#include <llvm/IR/Attributes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

namespace reussir::llvmpass {
namespace {

bool setAllocateAttributes(llvm::Function &fn) {
  bool changed = false;
  llvm::LLVMContext &ctx = fn.getContext();
  constexpr auto allocKind = llvm::AllocFnKind::Alloc |
                             llvm::AllocFnKind::Uninitialized |
                             llvm::AllocFnKind::Aligned;

  if (!fn.hasFnAttribute(llvm::Attribute::AllocKind) ||
      fn.getFnAttribute(llvm::Attribute::AllocKind).getAllocKind() !=
          allocKind) {
    fn.addFnAttr(llvm::Attribute::getWithAllocKind(ctx, allocKind));
    changed = true;
  }

  if (!fn.hasFnAttribute(llvm::Attribute::AllocSize)) {
    fn.addFnAttr(llvm::Attribute::getWithAllocSizeArgs(ctx, 1, std::nullopt));
    changed = true;
  } else {
    auto allocSizeArgs =
        fn.getFnAttribute(llvm::Attribute::AllocSize).getAllocSizeArgs();
    if (allocSizeArgs.first != 0 || allocSizeArgs.second.has_value()) {
      fn.addFnAttr(llvm::Attribute::getWithAllocSizeArgs(ctx, 1, std::nullopt));
      changed = true;
    }
  }

  if (fn.getFnAttribute("alloc-family").getValueAsString() != "reussir") {
    fn.addFnAttr("alloc-family", "reussir");
    changed = true;
  }

  return changed;
}

bool setDeallocateAttributes(llvm::Function &fn) {
  bool changed = false;
  llvm::LLVMContext &ctx = fn.getContext();
  constexpr auto freeKind = llvm::AllocFnKind::Free;

  if (!fn.hasFnAttribute(llvm::Attribute::AllocKind) ||
      fn.getFnAttribute(llvm::Attribute::AllocKind).getAllocKind() !=
          freeKind) {
    fn.addFnAttr(llvm::Attribute::getWithAllocKind(ctx, freeKind));
    changed = true;
  }

  if (fn.getFnAttribute("alloc-family").getValueAsString() != "reussir") {
    fn.addFnAttr("alloc-family", "reussir");
    changed = true;
  }

  if (fn.hasFnAttribute(llvm::Attribute::AllocSize)) {
    fn.removeFnAttr(llvm::Attribute::AllocSize);
    changed = true;
  }

  return changed;
}

} // namespace

llvm::PreservedAnalyses
RuntimeFunctionAttributorPass::run(llvm::Module &module,
                                   llvm::ModuleAnalysisManager &) {
  bool changed = false;

  if (auto *allocate = module.getFunction("__reussir_allocate"))
    changed |= setAllocateAttributes(*allocate);

  if (auto *deallocate = module.getFunction("__reussir_deallocate"))
    changed |= setDeallocateAttributes(*deallocate);

  return changed ? llvm::PreservedAnalyses::none()
                 : llvm::PreservedAnalyses::all();
}

} // namespace reussir::llvmpass
