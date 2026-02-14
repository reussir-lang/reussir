//===-- AttachNativeTarget.cpp - Attach native target pass ------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/AttachNativeTarget.h"

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/Dialect/DLTI/DLTI.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Target/LLVMIR/Import.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRATTACHNATIVETARGETPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {
struct ReussirAttachNativeTargetPass
    : public impl::ReussirAttachNativeTargetPassBase<
          ReussirAttachNativeTargetPass> {
  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    mlir::MLIRContext *context = &getContext();

    // Initialize native target
    llvm::InitializeNativeTarget();
    llvm::InitializeAllTargets();

    // Get the target triple
    std::string triple = llvm::sys::getDefaultTargetTriple();
    std::string cpu = llvm::sys::getHostCPUName().str();
    llvm::StringMap<bool> featuresMap = llvm::sys::getHostCPUFeatures();
    llvm::SubtargetFeatures features;
    for (const auto &[str, enable] : featuresMap)
      features.AddFeature(str, enable);
    std::string featuresStr = features.getString();

    std::string error;
    const llvm::Target *target =
        llvm::TargetRegistry::lookupTarget(triple, error);
    if (!target) {
      module.emitError("Failed to lookup native target: " + error);
      return signalPassFailure();
    }

    llvm::TargetOptions targetOptions;
    auto tm = std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
        llvm::Triple(triple), cpu, featuresStr, targetOptions, std::nullopt));
    if (!tm) {
      module.emitError("Failed to create target machine");
      return signalPassFailure();
    }

    const llvm::DataLayout &dl = tm->createDataLayout();

    // Attach attributes
    module->setAttr(
        mlir::LLVM::LLVMDialect::getDataLayoutAttrName(),
        mlir::StringAttr::get(context, dl.getStringRepresentation()));
    module->setAttr(mlir::LLVM::LLVMDialect::getTargetTripleAttrName(),
                    mlir::StringAttr::get(context, triple));

    mlir::DataLayoutSpecInterface dlSpec =
        mlir::translateDataLayout(dl, context);
    module->setAttr(mlir::DLTIDialect::kDataLayoutAttrName, dlSpec);
  }
};
} // namespace
} // namespace reussir
