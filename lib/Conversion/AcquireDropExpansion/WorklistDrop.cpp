//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache License v2.0 or
// the MIT License. See
// https://github.com/reussir-lang/reussir/blob/main/LICENSE
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//
#include "WorklistDrop.h"
#include "Reussir/Transformation/SpecialPointerTag.h"
#include "TailDrop.h"
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/SCF/IR/SCF.h>

namespace reussir {
bool isForwardDrop(mlir::ModuleOp module) {
  auto order = module->getAttrOfType<mlir::StringAttr>("reussir.drop_order");
  return !order || order.getValue() == "forward";
}
bool isInsideWorklistGlue(mlir::Operation *site) {
  auto function = site->getParentOfType<mlir::func::FuncOp>();
  return function && function->hasAttr(kWorklistGlueAttr);
}

namespace {
class WorklistGlue {
  mlir::ModuleOp module;
  mlir::PatternRewriter &builder;
  mlir::SymbolTableCollection *symbols;
  mlir::Location loc;

public:
  WorklistGlue(mlir::ModuleOp module, mlir::PatternRewriter &builder,
               mlir::SymbolTableCollection *symbols, mlir::Location loc)
      : module(module), builder(builder), symbols(symbols), loc(loc) {}

  mlir::func::FuncOp function(mlir::Type type, WorklistRole role,
                              llvm::function_ref<void(mlir::Value)> body,
                              bool indexed = false) {
    auto direction = isForwardDrop(module) ? WorklistDirection::Forward
                                          : WorklistDirection::Backward;
    auto key = WorklistRoleAttr::get(builder.getContext(), role, direction);
    for (auto function : module.getOps<mlir::func::FuncOp>())
      if (function->getAttr(REUSSIR_WORKLIST_ROLE) == key &&
          function.getNumArguments() == (indexed ? 2 : 1) &&
          function.getArgumentTypes()[0] == type)
        return function;
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(module.getBody());
    llvm::SmallVector<mlir::Type> args{type};
    if (indexed)
      args.push_back(builder.getIndexType());
    auto function = mlir::func::FuncOp::create(
        builder, loc,
        ("__reussir_drop." + stringifyWorklistRole(role) + "." +
         stringifyWorklistDirection(direction)).str(),
        builder.getFunctionType(args, {}));
    function.setPrivate();
    function->setAttr(REUSSIR_WORKLIST_ROLE, key);
    function->setAttr(kWorklistGlueAttr, builder.getUnitAttr());
    function->setAttr("llvm.linkage",
                      builder.getAttr<mlir::LLVM::LinkageAttr>(
                          mlir::LLVM::linkage::Linkage::Private));
    if (symbols)
      symbols->getSymbolTable(module).insert(function);
    else
      mlir::SymbolTable(module).insert(function);
    inheritSanitizerPassthrough(module, function);
    function.setArgAttr(0, "llvm.noalias", builder.getUnitAttr());
    function.setArgAttr(0, "llvm.nonnull", builder.getUnitAttr());
    function.setArgAttr(0, "llvm.noundef", builder.getUnitAttr());
    builder.setInsertionPointToStart(function.addEntryBlock());
    body(function.getArgument(0));
    mlir::func::ReturnOp::create(builder, loc);
    return function;
  }

  void schedule(mlir::func::FuncOp function, mlir::Value arg, bool run) {
    auto callee = mlir::FlatSymbolRefAttr::get(function.getSymNameAttr());
    if (run)
      ReussirDropRunOp::create(builder, loc, callee, arg);
    else
      ReussirDropDeferOp::create(builder, loc, callee, arg);
  }

  mlir::func::FuncOp walk(mlir::Type type) {
    return function(type, WorklistRole::Walk, [&](mlir::Value arg) {
      ReussirRefDropOp::create(builder, loc, arg, true, nullptr);
    });
  }
};
} // namespace

} // namespace reussir
