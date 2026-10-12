//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#include "TailDrop.h"
#include "Reussir/IR/RecordDrop.h"
#include "Reussir/Transformation/SpecialPointerTag.h"
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {
namespace {
bool canTailRelease(RcType type) {
  if (type.getCapability() != Capability::shared)
    return false;
  if (auto record = llvm::dyn_cast<RecordType>(type.getElementType()))
    return record.classifyRecordDrop() == RecordDropKind::TailLoop;
  if (auto array = llvm::dyn_cast<ArrayType>(type.getElementType()))
    return !array.hasTargetAttr() &&
           llvm::all_of(array.getShape(), [](int64_t n) { return n == 1; });
  if (auto cell = llvm::dyn_cast<CellType>(type.getElementType()))
    return !cell.getMutex() && !cell.getFlatlock() && !cell.getRwlock();
  return false;
}

class TailDropEmitter {
  mlir::ModuleOp module;
  mlir::func::FuncOp function;
  mlir::PatternRewriter &builder;
  mlir::Location loc;
  llvm::DenseMap<mlir::Type, mlir::func::FuncOp> helpers;
  llvm::SmallVector<RcType> pending;

  mlir::func::FuncOp helper(RcType type) {
    auto [it, inserted] = helpers.try_emplace(type);
    if (inserted) {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPoint(function);
      auto name =
          (function.getSymName() + "." + llvm::Twine(helpers.size())).str();
      auto callee = mlir::func::FuncOp::create(
          builder, loc, name, builder.getFunctionType({type}, {}));
      callee.setPrivate();
      callee->setAttr("llvm.linkage",
                      builder.getAttr<mlir::LLVM::LinkageAttr>(
                          mlir::LLVM::linkage::Linkage::Private));
      inheritSanitizerPassthrough(module, callee);
      it->second = callee;
      pending.push_back(type);
    }
    return it->second;
  }
  void tailRelease(mlir::Value next) {
    auto callee = helper(llvm::cast<RcType>(next.getType()));
    auto call = mlir::func::CallOp::create(builder, loc, callee,
                                           mlir::ValueRange{next});
    call->setAttr(kMustTailCallAttr, builder.getUnitAttr());
  }
  void freeOwner(mlir::Value owner) {
    auto rc = llvm::cast<RcType>(owner.getType());
    auto box = rc.getInnerBoxType();
    mlir::DataLayout layout(module);
    auto align = layout.getTypeABIAlignment(box);
    auto record = llvm::dyn_cast<RecordType>(rc.getElementType());
    TokenType tokenType =
        box.hasDynamicArrayPayload() ||
                (record && record.isVariant() && box.isHeaderFused() &&
                 !record.getFixed())
            ? TokenType::getDynamic(builder.getContext(), align)
            : TokenType::get(builder.getContext(), align,
                             layout.getTypeSize(box).getFixedValue());
    auto token = ReussirRcReinterpretOp::create(builder, loc, tokenType, owner);
    ReussirTokenFreeOp::create(builder, loc, token.getReinterpreted());
  }
  void emitPayload(mlir::Value ref, mlir::Value owner) {
    auto refType = llvm::cast<RefType>(ref.getType());
    mlir::Type type = refType.getElementType();
    auto projectedRefType = [&](mlir::Type type) {
      return RefType::get(builder.getContext(), type, Capability::unspecified,
                          refType.getAtomicKind());
    };
    if (auto rc = llvm::dyn_cast<RcType>(type); rc && canTailRelease(rc)) {
      mlir::Value next = ReussirRefLoadOp::create(builder, loc, rc, ref);
      freeOwner(owner);
      tailRelease(next);
      return;
    }
    if (auto nullable = llvm::dyn_cast<NullableType>(type)) {
      auto rc = llvm::dyn_cast<RcType>(nullable.getPtrTy());
      if (rc && canTailRelease(rc)) {
        mlir::Value next =
            ReussirRefLoadOp::create(builder, loc, nullable, ref);
        mlir::Value present =
            ReussirNullableCheckOp::create(builder, loc, next);
        freeOwner(owner);
        auto branch = mlir::scf::IfOp::create(builder, loc, present, false);
        mlir::OpBuilder::InsertionGuard guard(builder);
        builder.setInsertionPointToStart(branch.thenBlock());
        mlir::Value value =
            ReussirNullableCoerceOp::create(builder, loc, rc, next);
        tailRelease(value);
        return;
      }
    }
    if (auto record = llvm::dyn_cast<RecordType>(type)) {
      if (record.isVariant() && !record.getMembers().empty()) {
        llvm::SmallVector<mlir::Attribute> tags;
        for (size_t i = 0; i < record.getMembers().size(); ++i)
          tags.push_back(builder.getDenseI64ArrayAttr({int64_t(i)}));
        auto dispatch = ReussirRecordDispatchOp::create(
            builder, loc, mlir::Type{}, ref, builder.getArrayAttr(tags),
            tags.size());
        mlir::OpBuilder::InsertionGuard guard(builder);
        for (size_t i = 0; i < tags.size(); ++i) {
          auto member = getProjectedType(
              record.getMembers()[i], record.getMemberIsField()[i],
              Capability::unspecified, refType.getAtomicKind());
          auto *arm = builder.createBlock(&dispatch.getRegions()[i], {},
                                          {projectedRefType(member)}, {loc});
          if (record.getMemberIsField()[i])
            freeOwner(owner);
          else
            emitPayload(arm->getArgument(0), owner);
          ReussirScfYieldOp::create(builder, loc, nullptr);
        }
        return;
      }
      if (record.isCompound()) {
        llvm::SmallVector<size_t> members;
        for (auto [i, member, field] :
             llvm::enumerate(record.getMembers(), record.getMemberIsField()))
          if (!field && !isTriviallyCopyable(getProjectedType(
                            member, false, Capability::unspecified,
                            refType.getAtomicKind())))
            members.push_back(i);
        for (size_t i : members) {
          auto member = getProjectedType(record.getMembers()[i], false,
                                         Capability::unspecified,
                                         refType.getAtomicKind());
          mlir::Value slot = ReussirRefProjectOp::create(
              builder, loc, projectedRefType(member), ref,
              builder.getIndexAttr(i));
          if (i == members.back()) {
            emitPayload(slot, owner);
            return;
          }
          ReussirRefDropOp::create(builder, loc, slot);
        }
        freeOwner(owner);
        return;
      }
    }
    if (auto cell = llvm::dyn_cast<CellType>(type);
        cell && !cell.getMutex() && !cell.getFlatlock() && !cell.getRwlock()) {
      mlir::Value slot = ReussirRefProjectOp::create(
          builder, loc, projectedRefType(cell.getElementType()), ref,
          builder.getIndexAttr(0));
      emitPayload(slot, owner);
      return;
    }
    if (auto array = llvm::dyn_cast<ArrayType>(type);
        array && !array.hasTargetAttr() &&
        llvm::all_of(array.getShape(), [](int64_t n) { return n == 1; })) {
      mlir::Value view = ReussirArrayViewOp::create(
          builder, loc, getArrayViewMemRefType(array), ref);
      mlir::Value zero = mlir::arith::ConstantIndexOp::create(builder, loc, 0);
      for (size_t dim = 0; dim < array.getRank(); ++dim) {
        mlir::Type projected =
            dim + 1 == array.getRank()
                ? mlir::Type(projectedRefType(array.getElementType()))
                : mlir::Type(getProjectedArrayViewType(
                      llvm::cast<mlir::MemRefType>(view.getType())));
        view =
            ReussirArrayProjectOp::create(builder, loc, projected, view, zero);
      }
      emitPayload(view, owner);
      return;
    }
    if (!isTriviallyCopyable(type))
      ReussirRefDropOp::create(builder, loc, ref);
    freeOwner(owner);
  }
  void emitHeapRelease(mlir::Value rc) {
    auto type = llvm::cast<RcType>(rc.getType());
    bool atomic = type.getAtomicKind() == AtomicKind::atomic;
    mlir::Value count =
        atomic ? ReussirRcFetchSubOp::create(builder, loc, rc).getRefCount()
               : ReussirRcFetchOp::create(builder, loc, rc).getRefCount();
    mlir::Value one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
    mlir::Value unique = mlir::arith::CmpIOp::create(
        builder, loc, mlir::arith::CmpIPredicate::eq, count, one);
    auto branch = mlir::scf::IfOp::create(builder, loc, unique, !atomic);
    mlir::OpBuilder::InsertionGuard guard(builder);
    if (!atomic) {
      builder.setInsertionPointToStart(branch.elseBlock());
      mlir::Value decremented =
          mlir::arith::SubIOp::create(builder, loc, count, one);
      ReussirRcSetOp::create(builder, loc, rc, decremented);
    }
    builder.setInsertionPointToStart(branch.thenBlock());
    auto refType = RefType::get(builder.getContext(), type.getElementType(),
                                Capability::unspecified, type.getAtomicKind());
    mlir::Value ref = ReussirRcBorrowOp::create(builder, loc, refType, rc);
    emitPayload(ref, rc);
  }
  void emitHelper(RcType type) {
    auto callee = helpers.lookup(type);
    builder.setInsertionPointToStart(callee.addEntryBlock());
    mlir::Value rc = callee.getArgument(0);
    if (module->hasAttr(kSpecialPtrTagAttr) &&
        type.mayCarrySpecialPointerTag()) {
      mlir::Value immediate =
          ReussirRcIsImmediateOp::create(builder, loc, builder.getI1Type(), rc);
      auto branch = mlir::scf::IfOp::create(builder, loc, immediate, true);
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(branch.elseBlock());
      emitHeapRelease(rc);
    } else {
      emitHeapRelease(rc);
    }
    mlir::func::ReturnOp::create(builder, loc);
  }

public:
  TailDropEmitter(mlir::ModuleOp module, mlir::func::FuncOp function,
                  mlir::PatternRewriter &builder)
      : module(module), function(function), builder(builder),
        loc(function.getLoc()) {
    auto type = llvm::cast<RcType>(function.getArgumentTypes()[0]);
    helpers.try_emplace(type, function);
    pending.push_back(type);
    for (size_t i = 0; i < pending.size(); ++i)
      emitHelper(pending[i]);
  }
};
} // namespace

bool emitTailDropRelease(mlir::Value rc, mlir::Location loc,
                         mlir::ModuleOp module, mlir::PatternRewriter &rewriter,
                         mlir::SymbolTableCollection *symbols) {
  auto type = llvm::cast<RcType>(rc.getType());
  auto record = llvm::dyn_cast<RecordType>(type.getElementType());
  if (!record || !record.getName() || !canTailRelease(type))
    return false;
  std::optional<mlir::SymbolTable> ownTable;
  auto &table =
      symbols ? symbols->getSymbolTable(module) : ownTable.emplace(module);
  std::string name =
      (record.getDtorName(type.getAtomicKind()).getValue() + ".tail_release")
          .str();
  auto function = table.lookup<mlir::func::FuncOp>(name);
  if (!function) {
    mlir::OpBuilder::InsertionGuard guard(rewriter);
    rewriter.getContext()->getOrLoadDialect<mlir::scf::SCFDialect>();
    rewriter.setInsertionPointToStart(module.getBody());
    function = mlir::func::FuncOp::create(rewriter, loc, name,
                                          rewriter.getFunctionType({type}, {}));
    function.setPrivate();
    table.insert(function);
    function->setAttr("llvm.linkage",
                      rewriter.getAttr<mlir::LLVM::LinkageAttr>(
                          mlir::LLVM::linkage::Linkage::LinkonceODR));
    inheritSanitizerPassthrough(module, function);
    TailDropEmitter(module, function, rewriter);
  }
  mlir::func::CallOp::create(rewriter, loc, function, mlir::ValueRange{rc});
  return true;
}
} // namespace reussir
