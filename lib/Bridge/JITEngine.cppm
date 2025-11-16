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
#include <optional>
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

#include "Reussir/Bridge.h"

export module Reussir.JITEngine;
import Reussir.Bridge;

namespace reussir {
namespace {
using namespace llvm;
using namespace llvm::orc;
using SString =
    SmallString<CalculateSmallVectorDefaultInlinedElements<char>::value>;
class ReussirASTLayer;

// Holding a pointer to the AST.
class ReussirASTMaterializationUnit final : public MaterializationUnit {
public:
  ReussirASTMaterializationUnit(ReussirASTLayer &ast_layer, ASTStablePtr ast);

  void
  materialize(std::unique_ptr<MaterializationResponsibility> resp) override;

  StringRef getName() const override { return "ReussirJITMaterializationUnit"; }

  ~ReussirASTMaterializationUnit() override;

private:
  ReussirASTLayer &ast_layer;
  ASTStablePtr ast;
  std::vector<SString> symbol_names;
  std::vector<uint8_t> symbol_flags;

  void discard([[maybe_unused]] const JITDylib &dylib,
               [[maybe_unused]] const SymbolStringPtr &Sym) override {
    llvm::report_fatal_error("discard is not supported yet");
  }
};

class ReussirASTLayer final {
private:
  ASTCallbackFn ast_callback_fn;
  ASTFreeFn ast_free_fn;

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
