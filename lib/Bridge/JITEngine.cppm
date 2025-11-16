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
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <optional>

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
                  IRLayer &inner_layer, const DataLayout &data_layout)
      : ast_callback_fn(ast_callback_fn), ast_free_fn(ast_free_fn),
        inner_layer(inner_layer), data_layout(data_layout) {}

  Error addWithCleanup(ResourceTrackerSP resource_tracker, ASTStablePtr ast,
                       const char *symbol_names[], uint8_t symbol_flags[],
                       size_t symbol_count) {
    llvm::ArrayRef<const char *> raw_names(symbol_names, symbol_count);
    llvm::ArrayRef<uint8_t> raw_flags(symbol_flags, symbol_count);
    SmallVector<std::string> symbol_names_str(raw_names);
    SmallVector<uint8_t> symbol_flags_vec(raw_flags);
    for (const auto *ptr : raw_names)
      free(const_cast<char *>(ptr));
    free(symbol_names);
    free(symbol_flags);
    return resource_tracker->getJITDylib().define(
        std::make_unique<ReussirASTMaterializationUnit>(
            *this, ast, std::move(symbol_names_str),
            std::move(symbol_flags_vec)));
  };

private:
  ASTCallbackFn ast_callback_fn;
  ASTFreeFn ast_free_fn;
  IRLayer &inner_layer;
  const DataLayout &data_layout;

  friend class ReussirASTMaterializationUnit;
};

ReussirASTMaterializationUnit::~ReussirASTMaterializationUnit() {
  if (ast != nullptr)
    ast_layer.ast_free_fn(ast);
  ast = nullptr;
}

class JITEngine final {
private:
  std::unique_ptr<ExecutionSession> execution_session;
  std::unique_ptr<EPCIndirectionUtils> epc_indirection_utils;

  DataLayout data_layout;
  MangleAndInterner mangle_and_interner;

  RTDyldObjectLinkingLayer object_layer;
  IRCompileLayer compile_layer;
  std::optional<IRTransformLayer> optimize_layer;
  ReussirASTLayer ast_layer;

  JITDylib &main_dynlib;
};
} // namespace
} // namespace reussir
