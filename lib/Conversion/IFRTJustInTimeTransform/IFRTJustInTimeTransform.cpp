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
/// This file packages outlined IFRT kernels for runtime compilation.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>

#ifdef REUSSIR_ENABLE_OPENXLA
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Conversion/OpenXLATarget.h"
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/BLAKE3.h>
#include <mlir/Bytecode/BytecodeWriter.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/IR/Verifier.h>
#include <stablehlo/transforms/optimization/Passes.h>
#endif

namespace reussir {
#define GEN_PASS_DEF_REUSSIRIFRTJUSTINTIMETRANSFORMPASS
#define GEN_PASS_DEF_REUSSIRPREPAREPJRTPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {
struct ReussirPreparePjrtPass
    : impl::ReussirPreparePjrtPassBase<ReussirPreparePjrtPass> {
  using Base::Base;
  void runOnOperation() override {
    bool hasIfrt = false;
    getOperation().walk([&](mlir::Operation *op) {
      hasIfrt |= op->getName().getDialectNamespace() == "ifrt" ||
                 op->hasAttr("ifrt.function");
    });
    if (hasIfrt) {
#ifdef REUSSIR_ENABLE_OPENXLA
      mlir::OpPassManager pipeline(mlir::ModuleOp::getOperationName());
      ReussirIFRTJustInTimeTransformPassOptions options;
      options.optimizeKernels = optimizeKernels;
      pipeline.addPass(createReussirIFRTJustInTimeTransformPass(options));
      pipeline.addPass(mlir::createSymbolDCEPass());
      if (mlir::failed(runPipeline(pipeline, getOperation()))) {
        signalPassFailure();
        return;
      }
#else
      getOperation().emitError(
          "PJRT preparation requires an OpenXLA-enabled build");
      signalPassFailure();
      return;
#endif
    }
    // Source kernels remain StableHLO until a launch packages them into a
    // device executable. Never send an unlaunched kernel to host conversion,
    // even when its body happens to contain only func.return.
    bool unlaunchedKernel = false;
    getOperation().walk([&](mlir::func::FuncOp function) {
      if (function->hasAttr("reussir.kernel_function")) {
        function.emitError("an unlaunched "
                           "kernel cannot enter native host lowering");
        unlaunchedKernel = true;
      }
    });
    if (unlaunchedKernel)
      signalPassFailure();
  }
};

struct ReussirIFRTJustInTimeTransformPass
    : impl::ReussirIFRTJustInTimeTransformPassBase<
          ReussirIFRTJustInTimeTransformPass> {
  using Base::Base;
  void runOnOperation() override {
#ifndef REUSSIR_ENABLE_OPENXLA
    getOperation().emitError(
        "IFRT JIT transformation requires an OpenXLA-enabled build");
    signalPassFailure();
#else
    struct Call {
      mlir::Operation *op;
      IfrtCallInfo info;
      mlir::ModuleOp kernel;
      mlir::StringAttr compileOptions;
    };
    struct Artifact {
      std::string bytes;
      mlir::StringAttr checksum;
      std::string symbol;
    };
    llvm::SmallVector<Call> calls;
    llvm::MapVector<mlir::Operation *, Artifact> artifacts;
    mlir::SymbolTableCollection symbols;
    auto result =
        getOperation().walk([&](mlir::Operation *op) -> mlir::WalkResult {
          auto info = getIfrtCallInfo(op);
          if (!info)
            return mlir::WalkResult::advance();
          auto callee = symbols.lookupNearestSymbolFrom<mlir::func::FuncOp>(
              op, info->callee);
          auto kernel =
              callee ? llvm::dyn_cast<mlir::ModuleOp>(callee->getParentOp())
                     : mlir::ModuleOp{};
          if (!kernel || kernel == getOperation() ||
              callee.getSymName() != "main") {
            op->emitOpError("requires an outlined kernel module with @main; "
                            "run IFRT outlining first");
            return mlir::WalkResult::interrupt();
          }
          if (!artifacts.contains(kernel)) {
            // Validate a detached copy: external symbols must not resolve
            // through the surrounding host module. Serialization must not
            // mutate the source.
            mlir::OwningOpRef<mlir::ModuleOp> copy(kernel.clone());
            (*copy)->removeAttr(mlir::SymbolTable::getSymbolAttrName());
            (*copy)->removeAttr(mlir::SymbolTable::getVisibilityAttrName());
            // PJRT's HLO importer cannot carry Future values across func.call.
            // Use MLIR's registered function/dialect inliners on the isolated
            // compilation unit, then discard helpers that are no longer used.
            mlir::PassManager preparation(&getContext());
            preparation.addPass(mlir::createInlinerPass());
            preparation.addPass(mlir::createSymbolDCEPass());
            if (mlir::failed(preparation.run(*copy)))
              return mlir::WalkResult::interrupt();
            if (optimizeKernels) {
              mlir::PassManager optimization(&getContext());
              optimization.addNestedPass<mlir::func::FuncOp>(
                  mlir::stablehlo::
                      createStablehloTargetIndependentOptimizationPass());
              if (mlir::failed(optimization.run(*copy)))
                return mlir::WalkResult::interrupt();
            }
            if (mlir::failed(verifyIfrtKernelModule(*copy)) ||
                mlir::failed(mlir::verify(*copy)))
              return mlir::WalkResult::interrupt();
            Artifact artifact;
            llvm::raw_string_ostream stream(artifact.bytes);
            if (mlir::failed(mlir::writeBytecodeToFile(*copy, stream)))
              return mlir::WalkResult::interrupt();
            llvm::BLAKE3 hasher;
            hasher.update(artifact.bytes);
            auto digest = hasher.final();
            artifact.checksum = mlir::StringAttr::get(
                &getContext(), llvm::toHex(digest, /*LowerCase=*/true));
            artifact.symbol =
                mangledBlake3Symbol("REUSSIR_IFRT_BYTECODE", artifact.bytes);
            artifacts.insert({kernel, std::move(artifact)});
          }
          auto options = serializeIfrtCompileOptions(op);
          if (mlir::failed(options))
            return mlir::WalkResult::interrupt();
          calls.push_back({op, *info, kernel, *options});
          return mlir::WalkResult::advance();
        });
    if (result.wasInterrupted()) {
      signalPassFailure();
      return;
    }

    // Only rewrite after every kernel has been validated and serialized.
    llvm::DenseMap<std::pair<mlir::Operation *, mlir::StringAttr>,
                   ReussirIFRTBytecodeOp>
        emitted;
    for (auto &call : calls) {
      auto &artifact = artifacts[call.kernel];
      auto *scope = mlir::SymbolTable::getNearestSymbolTable(call.op);
      auto &table = symbols.getSymbolTable(scope);
      auto key = std::make_pair(
          scope, mlir::StringAttr::get(&getContext(), artifact.symbol));
      auto code = emitted.lookup(key);
      if (!code)
        code = table.lookup<ReussirIFRTBytecodeOp>(artifact.symbol);
      if (!code || code.getBytecode() != artifact.bytes ||
          code.getChecksumAttr() != artifact.checksum) {
        mlir::OpBuilder builder(&getContext());
        builder.setInsertionPointToEnd(&scope->getRegion(0).front());
        code = ReussirIFRTBytecodeOp::create(
            builder, call.kernel.getLoc(),
            builder.getStringAttr(artifact.symbol),
            builder.getStringAttr("private"),
            builder.getStringAttr(artifact.bytes), artifact.checksum);
        table.insert(code);
      }
      code->setAttr("kernel_type",
                    mlir::TypeAttr::get(
                        call.kernel.lookupSymbol<mlir::func::FuncOp>("main")
                            .getFunctionType()));
      emitted[key] = code;
      mlir::OpBuilder builder(call.op);
      auto callee = mlir::SymbolRefAttr::get(code.getSymNameAttr());
      auto jit = ReussirPJRTJitCallOp::create(
          builder, call.op->getLoc(),
          call.op->getResults().take_front(call.info.numOutputs).getTypes(),
          call.op->getResult(call.info.numOutputs).getType(),
          call.op->getOperands().take_front(call.info.numInputs),
          call.op->getOperands().drop_front(call.info.numInputs),
          mlir::ValueRange{}, callee, call.info.devices, call.compileOptions,
          call.info.ioAliases, call.info.donatedInputIndices,
          call.info.argAttrs, call.info.resAttrs, builder.getDenseI32ArrayAttr({}));
      jit->setDiscardableAttrs(call.op->getDiscardableAttrDictionary());
      call.op->replaceAllUsesWith(jit.getResults());
      call.op->erase();
    }
    getOperation().walk([](mlir::Operation *op) {
      if (!isIfrtAfter(op))
        return;
      mlir::OpBuilder builder(op);
      auto after = ReussirPJRTAfterOp::create(
          builder, op->getLoc(), op->getResult(0).getType(), op->getOperands());
      op->replaceAllUsesWith(after->getResults());
      op->erase();
    });
    getOperation().walk([](mlir::func::FuncOp func) {
      if (!func->removeAttr("ifrt.function"))
        return;
      // Argument donation annotations require the IFRT function marker.
      for (unsigned i = 0; i < func.getNumArguments(); ++i)
        func.removeArgAttr(i, "ifrt.donated");
    });
#endif
  }
};
} // namespace
} // namespace reussir
