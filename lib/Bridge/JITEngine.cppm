//===-- JITEngine.cppm - Reussir JIT engine ---------------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the JIT engine for Reussir.
//===----------------------------------------------------------------------===//
module;
#include <llvm/ADT/StringRef.h>
#include <llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/MaterializationUnit.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <mlir/IR/MLIRContext.h>
#include <mlir/Pass/PassManager.h>
#include <optional>
#include <spdlog/spdlog.h>
#include <tpde-llvm/OrcCompiler.hpp>

#include "Reussir/Bridge.h"

export module Reussir.JITEngine;
import Reussir.Bridge;

namespace reussir {
namespace {
using namespace llvm;
using namespace llvm::orc;
class ReussirASTLayer;

// Holding a pointer to the AST.
class ReussirASTMaterializationUnit final : public MaterializationUnit {
public:
  ReussirASTMaterializationUnit(ReussirASTLayer &ast_layer, ASTStablePtr ast,
                                SmallVector<std::string> symbol_names,
                                SmallVector<uint8_t> symbol_flags);

  void
  materialize(std::unique_ptr<MaterializationResponsibility> resp) override;

  StringRef getName() const override { return "ReussirJITMaterializationUnit"; }

  ~ReussirASTMaterializationUnit() override;

private:
  ReussirASTLayer &ast_layer;
  ASTStablePtr ast;
  SmallVector<std::string> symbol_names;
  SmallVector<uint8_t> symbol_flags;

  void discard([[maybe_unused]] const JITDylib &dylib,
               [[maybe_unused]] const SymbolStringPtr &Sym) override {
    llvm::report_fatal_error("discard is not supported yet");
  }
};

class ReussirASTLayer final {
public:
  ReussirASTLayer(ASTCallbackFn ast_callback_fn, ASTFreeFn ast_free_fn,
                  IRLayer &inner_layer, llvm::orc::ThreadSafeContext ts_context,
                  mlir::MLIRContext &context, mlir::PassManager &pm,
                  const DataLayout &data_layout)
      : ast_callback_fn(ast_callback_fn), ast_free_fn(ast_free_fn),
        inner_layer(inner_layer), ts_context(ts_context), context(context),
        pm(pm), data_layout(data_layout) {}

  Error add(ResourceTrackerSP resource_tracker, ASTStablePtr ast,
            const char *symbol_names[], uint8_t symbol_flags[],
            size_t symbol_count) {
    llvm::ArrayRef<const char *> raw_names(symbol_names, symbol_count);
    llvm::ArrayRef<uint8_t> raw_flags(symbol_flags, symbol_count);
    SmallVector<std::string> symbol_names_str(raw_names);
    SmallVector<uint8_t> symbol_flags_vec(raw_flags);
    return resource_tracker->getJITDylib().define(
        std::make_unique<ReussirASTMaterializationUnit>(
            *this, ast, std::move(symbol_names_str),
            std::move(symbol_flags_vec)));
  };

  void emit(std::unique_ptr<MaterializationResponsibility> resp,
            ASTStablePtr ast) {
    auto texture = ast_callback_fn(ast);
    emit(std::move(resp), texture);
  }

  void emit(std::unique_ptr<MaterializationResponsibility> resp,
            const char *texture) {
    auto unique_module = ts_context.withContextDo([&](LLVMContext *llvmCtx) {
      return bridge::translateToModule(texture, *llvmCtx, context, pm,
                                       data_layout);
    });
    auto concurrentModule =
        ThreadSafeModule(std::move(unique_module), ts_context);
    inner_layer.emit(std::move(resp), std::move(concurrentModule));
  }

  MaterializationUnit::Interface
  getInterface(ASTStablePtr ast, llvm::ArrayRef<std::string> symbol_names,
               llvm::ArrayRef<uint8_t> symbol_flags) {
    MangleAndInterner mangle(inner_layer.getExecutionSession(), data_layout);
    SymbolFlagsMap symbols;
    for (size_t i = 0; i < symbol_names.size(); ++i) {
      symbols[mangle(symbol_names[i])] = JITSymbolFlags(
          static_cast<JITSymbolFlags::FlagNames>(symbol_flags[i]));
    }
    return MaterializationUnit::Interface(std::move(symbols), nullptr);
  }

private:
  ASTCallbackFn ast_callback_fn;
  ASTFreeFn ast_free_fn;
  IRLayer &inner_layer;
  llvm::orc::ThreadSafeContext ts_context;
  mlir::MLIRContext &context;
  mlir::PassManager &pm;
  const DataLayout &data_layout;

  friend class ReussirASTMaterializationUnit;
};

ReussirASTMaterializationUnit::~ReussirASTMaterializationUnit() {
  if (ast != nullptr)
    ast_layer.ast_free_fn(ast);
  ast = nullptr;
}

void ReussirASTMaterializationUnit::materialize(
    std::unique_ptr<MaterializationResponsibility> resp) {
  ast_layer.emit(std::move(resp), ast);
}

ReussirASTMaterializationUnit::ReussirASTMaterializationUnit(
    ReussirASTLayer &ast_layer, ASTStablePtr ast,
    SmallVector<std::string> symbol_names, SmallVector<uint8_t> symbol_flags)
    : MaterializationUnit(
          ast_layer.getInterface(ast, symbol_names, symbol_flags)),
      ast_layer(ast_layer), ast(ast), symbol_names(symbol_names),
      symbol_flags(symbol_flags) {}

IRCompileLayer createCompilerLayer(llvm::orc::JITTargetMachineBuilder jtmb,
                                   ReussirOptOption opt, ExecutionSession &es,
                                   ObjectLayer &base) {
  if (opt == REUSSIR_OPT_TPDE) {
#ifdef REUSSIR_HAS_TPDE
    return {
        es, base,
        std::make_unique<tpde_llvm::ConcurrentOrcCompiler>(std::move(jtmb))};
#else
    spdlog::warning("TPDE is not supported on this platform");
#endif
  }
  return {es, base, std::make_unique<llvm::orc::ConcurrentIRCompiler>(jtmb)};
}

class JITEngine final {
private:
  std::unique_ptr<ExecutionSession> execution_session;
  std::unique_ptr<EPCIndirectionUtils> epc_indirection_utils;

  DataLayout data_layout;
  MangleAndInterner mangle_and_interner;

  RTDyldObjectLinkingLayer object_layer;
  IRCompileLayer compile_layer;
  IRTransformLayer optimize_layer;
  mlir::MLIRContext context;
  std::unique_ptr<mlir::PassManager> pm;
  llvm::orc::ThreadSafeContext ts_context;
  ReussirASTLayer ast_layer;

  JITDylib &main_dynlib;

  static Expected<ThreadSafeModule>
  optimizeModule(ThreadSafeModule tsm,
                 [[maybe_unused]] const MaterializationResponsibility &resp,
                 ReussirOptOption opt) {
    tsm.withModuleDo([&](Module &m) { bridge::runNPMOptimization(m, opt); });
    return std::move(tsm);
  }

public:
  JITEngine(std::unique_ptr<ExecutionSession> es,
            std::unique_ptr<EPCIndirectionUtils> epciu,
            JITTargetMachineBuilder jtmb, DataLayout dl, ReussirOptOption opt,
            ASTCallbackFn ast_callback_fn, ASTFreeFn ast_free_fn)
      : execution_session(std::move(es)),
        epc_indirection_utils(std::move(epciu)), data_layout(dl),
        mangle_and_interner(*execution_session, dl),
        object_layer(*execution_session,
                     [](const MemoryBuffer &) {
                       return std::make_unique<SectionMemoryManager>();
                     }),
        compile_layer(createCompilerLayer(std::move(jtmb), opt,
                                          *execution_session, object_layer)),
        optimize_layer(
            *execution_session, compile_layer,
            [opt](ThreadSafeModule tsm, MaterializationResponsibility &resp) {
              return optimizeModule(std::move(tsm), resp, opt);
            }),
        context(bridge::buildMLIRContext()),
        pm(bridge::buildPassManager(context)),
        ts_context(std::make_unique<llvm::LLVMContext>()),
        ast_layer(ast_callback_fn, ast_free_fn, optimize_layer, ts_context,
                  context, *pm, data_layout),
        main_dynlib(execution_session->createBareJITDylib("<main>")) {
    main_dynlib.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            data_layout.getGlobalPrefix())));
  }

  ~JITEngine() {
    if (auto err = execution_session->endSession())
      execution_session->reportError(std::move(err));
    if (auto err = epc_indirection_utils->cleanup())
      execution_session->reportError(std::move(err));
  }

  const DataLayout &getDataLayout() const { return data_layout; }
  JITDylib &getMainJITDylib() { return main_dynlib; }
  Error addModule(const char *texture,
                  ResourceTrackerSP resource_tracker = nullptr) {
    if (!resource_tracker)
      resource_tracker = main_dynlib.getDefaultResourceTracker();

    auto unique_module = ts_context.withContextDo([&](LLVMContext *llvmCtx) {
      return bridge::translateToModule(texture, *llvmCtx, context, *pm,
                                       data_layout);
    });
    auto concurrentModule =
        ThreadSafeModule(std::move(unique_module), ts_context);
    return optimize_layer.add(resource_tracker, std::move(concurrentModule));
  }
  Error addModule(ASTStablePtr ast, const char *symbol_names[],
                  uint8_t symbol_flags[], size_t symbol_count,
                  ResourceTrackerSP resource_tracker = nullptr) {
    if (!resource_tracker)
      resource_tracker = main_dynlib.getDefaultResourceTracker();
    return ast_layer.add(resource_tracker, ast, symbol_names, symbol_flags,
                         symbol_count);
  }
  Expected<ExecutorSymbolDef> lookup(StringRef Name) {
    return execution_session->lookup({&main_dynlib},
                                     mangle_and_interner(Name.str()));
  }
};
} // namespace
} // namespace reussir

static void handleLazyCallThroughError() {
  llvm::report_fatal_error("Failed to handle lazy call through");
}

export extern "C" {
  ReussirJIT reussir_bridge_jit_create(ASTCallbackFn ast_callback_fn,
                                       ASTFreeFn ast_free_fn,
                                       ReussirOptOption opt) {
    using namespace llvm::orc;
    auto epc = SelfExecutorProcessControl::Create();
    if (!epc) {
      spdlog::error("Failed to create SelfExecutorProcessControl");
      return nullptr;
    }

    auto es = std::make_unique<ExecutionSession>(std::move(*epc));

    auto epciu = EPCIndirectionUtils::Create(*es);
    if (!epciu) {
      spdlog::error("Failed to create EPCIndirectionUtils");
      return nullptr;
    }

    (*epciu)->createLazyCallThroughManager(
        *es, ExecutorAddr::fromPtr(&handleLazyCallThroughError));

    if (setUpInProcessLCTMReentryViaEPCIU(**epciu)) {
      spdlog::error("Failed to set up in process LCTM reentry via EPCIU");
      return nullptr;
    }

    JITTargetMachineBuilder jtmb(
        es->getExecutorProcessControl().getTargetTriple());

    auto dl = jtmb.getDefaultDataLayoutForTarget();
    if (!dl) {
      spdlog::error("Failed to get default data layout for target");
      return nullptr;
    }
    return new reussir::JITEngine(std::move(es), std::move(*epciu),
                                  std::move(jtmb), std::move(*dl), opt,
                                  ast_callback_fn, ast_free_fn);
  }
  void reussir_bridge_jit_destroy(ReussirJIT jit) {
    delete static_cast<reussir::JITEngine *>(jit);
  }
  bool reussir_bridge_jit_add_lazy_module(
      ReussirJIT jit, ASTStablePtr ast, const char *symbol_names[],
      uint8_t symbol_flags[], size_t symbol_count) {
    if (static_cast<reussir::JITEngine *>(jit)->addModule(
            ast, symbol_names, symbol_flags, symbol_count)) {
      spdlog::error("Failed to add lazy module");
      return false;
    }
    return true;
  }
  bool reussir_bridge_jit_add_module(ReussirJIT jit, const char *texture) {
    if (static_cast<reussir::JITEngine *>(jit)->addModule(texture)) {
      spdlog::error("Failed to add module");
      return false;
    }
    return true;
  }
  void *reussir_bridge_jit_lookup_symbol(ReussirJIT jit,
                                         const char *symbol_name) {
    auto def = static_cast<reussir::JITEngine *>(jit)->lookup(symbol_name);
    if (!def) {
      spdlog::error("Failed to lookup symbol: {}", symbol_name);
      return nullptr;
    }
    return def->getAddress().toPtr<void *>();
  }
}
