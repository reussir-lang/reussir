//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache License v2.0 or
// the MIT License. See
// https://github.com/reussir-lang/reussir/blob/main/LICENSE
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//
#include "WorklistDrop.h"
#include "Reussir/Transformation/SpecialPointerTag.h"
#include "TailDrop.h"
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

  mlir::func::FuncOp function(mlir::Type type, llvm::StringRef role,
                              llvm::function_ref<void(mlir::Value)> body,
                              bool indexed = false) {
    auto order = isForwardDrop(module) ? "forward" : "backward";
    auto key = builder.getStringAttr((role + "." + order).str());
    for (auto function : module.getOps<mlir::func::FuncOp>())
      if (function->getAttr("reussir.worklist_role") == key &&
          function.getNumArguments() == (indexed ? 2 : 1) &&
          function.getArgumentTypes()[0] == type)
        return function;
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(module.getBody());
    llvm::SmallVector<mlir::Type> args{type};
    if (indexed)
      args.push_back(builder.getIndexType());
    auto function = mlir::func::FuncOp::create(
        builder, loc, ("__reussir_drop." + key.getValue()).str(),
        builder.getFunctionType(args, {}));
    function.setPrivate();
    function->setAttr("reussir.worklist_role", key);
    function->setAttr(kWorklistGlueAttr, builder.getUnitAttr());
    function->setAttr("llvm.linkage",
                      builder.getAttr<mlir::LLVM::LinkageAttr>(
                          mlir::LLVM::linkage::Linkage::Private));
    if (symbols)
      symbols->getSymbolTable(module).insert(function);
    else
      mlir::SymbolTable(module).insert(function);
    inheritSanitizerPassthrough(module, function);
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

  void drop(mlir::Value ref, bool run) {
    auto task = function(ref.getType(), "walk", [&](mlir::Value arg) {
      ReussirRefDropOp::create(builder, loc, arg, true, nullptr);
    });
    schedule(task, ref, run);
  }

  void array(mlir::Value ref) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto type = llvm::cast<ArrayType>(refType.getElementType());
    auto task = function(
        refType, "array",
        [&](mlir::Value arg) {
          auto cursor = builder.getInsertionBlock()->getArgument(1);
          mlir::Value view = ReussirArrayViewOp::create(
              builder, loc, getArrayViewMemRefType(type), arg);
          llvm::SmallVector<mlir::Value> extents;
          mlir::Value total =
              mlir::arith::ConstantIndexOp::create(builder, loc, 1);
          for (auto [dim, extent] : llvm::enumerate(type.getShape())) {
            mlir::Value size =
                mlir::ShapedType::isDynamic(extent)
                    ? mlir::memref::DimOp::create(builder, loc, view,
                                                  static_cast<int64_t>(dim))
                          .getResult()
                    : mlir::arith::ConstantIndexOp::create(builder, loc, extent)
                          .getResult();
            extents.push_back(size);
            total = mlir::arith::MulIOp::create(builder, loc, total, size);
          }
          mlir::Value present = mlir::arith::CmpIOp::create(
              builder, loc, mlir::arith::CmpIPredicate::ult, cursor, total);
          auto branch = mlir::scf::IfOp::create(builder, loc, present, false);
          mlir::OpBuilder::InsertionGuard guard(builder);
          builder.setInsertionPointToStart(branch.thenBlock());
          mlir::Value next = mlir::arith::AddIOp::create(
              builder, loc, cursor,
              mlir::arith::ConstantIndexOp::create(builder, loc, 1));
          auto callback = llvm::cast<mlir::func::FuncOp>(
              branch->getParentOfType<mlir::func::FuncOp>());
          ReussirDropDeferIndexedOp::create(
              builder, loc,
              mlir::FlatSymbolRefAttr::get(callback.getSymNameAttr()), arg,
              next);
          llvm::SmallVector<mlir::Value> indices(extents.size());
          mlir::Value remaining = cursor;
          for (size_t dim = extents.size(); dim-- > 0;) {
            indices[dim] = mlir::arith::RemUIOp::create(builder, loc, remaining,
                                                        extents[dim]);
            remaining = mlir::arith::DivUIOp::create(builder, loc, remaining,
                                                     extents[dim]);
          }
          for (auto [dim, index] : llvm::enumerate(indices)) {
            mlir::Type projected =
                dim + 1 == indices.size()
                    ? mlir::Type(RefType::get(
                          builder.getContext(), type.getElementType(),
                          Capability::unspecified, refType.getAtomicKind()))
                    : mlir::Type(getProjectedArrayViewType(
                          llvm::cast<mlir::MemRefType>(view.getType())));
            view = ReussirArrayProjectOp::create(builder, loc, projected, view,
                                                 index);
          }
          ReussirRefDropOp::create(builder, loc, view);
        },
        true);
    ReussirDropDeferIndexedOp::create(
        builder, loc, mlir::FlatSymbolRefAttr::get(task.getSymNameAttr()), ref,
        mlir::arith::ConstantIndexOp::create(builder, loc, 0));
  }

  void release(mlir::Value rc, bool run) {
    auto type = llvm::cast<RcType>(rc.getType());
    auto task = function(type, "release", [&](mlir::Value arg) {
      if (llvm::isa<FFIObjectType, ClosureType>(type.getElementType())) {
        if (type.getAtomicKind() == AtomicKind::atomic)
          ReussirRcSetOp::create(
              builder, loc, arg,
              mlir::arith::ConstantIndexOp::create(builder, loc, 1));
        auto dec = ReussirRcDecOp::create(builder, loc, NullableType{}, arg,
                                          mlir::IntegerAttr{},
                                          mlir::DenseI64ArrayAttr{});
        dec->setAttr(kWorklistRawAttr, builder.getUnitAttr());
        return;
      }
      auto refType =
          RefType::get(builder.getContext(), type.getElementType(),
                       Capability::unspecified, type.getAtomicKind());
      mlir::Value ref = ReussirRcBorrowOp::create(builder, loc, refType, arg);
      if (isForwardDrop(module)) {
        auto free = function(type, "free", [&](mlir::Value owner) {
          emitBoxFree(owner, loc, module, builder);
        });
        schedule(free, arg, false);
        drop(ref, false);
      } else {
        ReussirRefDropOp::create(builder, loc, ref, true, nullptr);
        emitBoxFree(arg, loc, module, builder);
      }
    });
    schedule(task, rc, run);
  }
};
} // namespace

void emitWorklistDrop(mlir::Value ref, mlir::Location loc,
                      mlir::PatternRewriter &rewriter,
                      mlir::SymbolTableCollection *symbols, bool run) {
  WorklistGlue(rewriter.getInsertionBlock()
                   ->getParentOp()
                   ->getParentOfType<mlir::ModuleOp>(),
               rewriter, symbols, loc)
      .drop(ref, run);
}
void emitWorklistRelease(mlir::Value rc, mlir::Location loc,
                         mlir::PatternRewriter &rewriter,
                         mlir::SymbolTableCollection *symbols, bool run) {
  WorklistGlue(rewriter.getInsertionBlock()
                   ->getParentOp()
                   ->getParentOfType<mlir::ModuleOp>(),
               rewriter, symbols, loc)
      .release(rc, run);
}

void emitWorklistArrayDrop(mlir::Value ref, mlir::Location loc,
                           mlir::PatternRewriter &rewriter,
                           mlir::SymbolTableCollection *symbols) {
  WorklistGlue(rewriter.getInsertionBlock()
                   ->getParentOp()
                   ->getParentOfType<mlir::ModuleOp>(),
               rewriter, symbols, loc)
      .array(ref);
}

void emitWorklistDecrement(mlir::Value rc, mlir::Location loc,
                           mlir::PatternRewriter &rewriter,
                           mlir::SymbolTableCollection *symbols, bool run) {
  auto type = llvm::cast<RcType>(rc.getType());
  auto module = rewriter.getInsertionBlock()
                    ->getParentOp()
                    ->getParentOfType<mlir::ModuleOp>();
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  if (module->hasAttr(kSpecialPtrTagAttr) && type.mayCarrySpecialPointerTag()) {
    mlir::Value immediate =
        ReussirRcIsImmediateOp::create(rewriter, loc, rewriter.getI1Type(), rc);
    auto branch = mlir::scf::IfOp::create(rewriter, loc, immediate, true);
    rewriter.setInsertionPointToStart(branch.elseBlock());
  }
  bool atomic = type.getAtomicKind() == AtomicKind::atomic;
  mlir::Value count =
      atomic ? ReussirRcFetchSubOp::create(rewriter, loc, rc).getRefCount()
             : ReussirRcFetchOp::create(rewriter, loc, rc).getRefCount();
  mlir::Value one = mlir::arith::ConstantIndexOp::create(rewriter, loc, 1);
  mlir::Value unique = mlir::arith::CmpIOp::create(
      rewriter, loc, mlir::arith::CmpIPredicate::eq, count, one);
  auto branch = mlir::scf::IfOp::create(rewriter, loc, unique, !atomic);
  if (!atomic) {
    rewriter.setInsertionPointToStart(branch.elseBlock());
    ReussirRcSetOp::create(
        rewriter, loc, rc,
        mlir::arith::SubIOp::create(rewriter, loc, count, one));
  }
  rewriter.setInsertionPointToStart(branch.thenBlock());
  emitWorklistRelease(rc, loc, rewriter, symbols, run);
}
} // namespace reussir
