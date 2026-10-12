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
#include <mlir/Dialect/ControlFlow/IR/ControlFlowOps.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {
namespace {
bool canIterate(RcType type) {
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
  mlir::Block *exit;
  llvm::DenseMap<mlir::Type, mlir::Block *> states;
  llvm::SmallVector<RcType> pending;

  mlir::Block *block() { return builder.createBlock(&function.getBody()); }
  mlir::Block *state(RcType type) {
    auto [it, inserted] = states.try_emplace(type);
    if (inserted) {
      mlir::OpBuilder::InsertionGuard guard(builder);
      it->second = block();
      it->second->addArgument(type, loc);
      pending.push_back(type);
    }
    return it->second;
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
  void finish(mlir::Value owner) {
    freeOwner(owner);
    mlir::cf::BranchOp::create(builder, loc, exit);
  }
  void emitPayload(mlir::Value ref, mlir::Value owner) {
    auto refType = llvm::cast<RefType>(ref.getType());
    mlir::Type type = refType.getElementType();
    auto projectedRefType = [&](mlir::Type type) {
      return RefType::get(builder.getContext(), type, Capability::unspecified,
                          refType.getAtomicKind());
    };
    if (auto rc = llvm::dyn_cast<RcType>(type); rc && canIterate(rc)) {
      mlir::Value next = ReussirRefLoadOp::create(builder, loc, rc, ref);
      freeOwner(owner);
      mlir::cf::BranchOp::create(builder, loc, state(rc),
                                 mlir::ValueRange{next});
      return;
    }
    if (auto nullable = llvm::dyn_cast<NullableType>(type)) {
      auto rc = llvm::dyn_cast<RcType>(nullable.getPtrTy());
      if (rc && canIterate(rc)) {
        mlir::Value next =
            ReussirRefLoadOp::create(builder, loc, nullable, ref);
        mlir::Value present =
            ReussirNullableCheckOp::create(builder, loc, next);
        freeOwner(owner);
        auto *current = builder.getInsertionBlock();
        auto *nonnull = block();
        builder.setInsertionPointToEnd(current);
        mlir::cf::CondBranchOp::create(builder, loc, present, nonnull, exit);
        builder.setInsertionPointToEnd(nonnull);
        mlir::Value value =
            ReussirNullableCoerceOp::create(builder, loc, rc, next);
        mlir::cf::BranchOp::create(builder, loc, state(rc),
                                   mlir::ValueRange{value});
        return;
      }
    }
    if (auto record = llvm::dyn_cast<RecordType>(type)) {
      if (record.isVariant() && !record.getMembers().empty()) {
        auto *current = builder.getInsertionBlock();
        llvm::SmallVector<mlir::Block *> arms;
        for (size_t i = 0; i < record.getMembers().size(); ++i)
          arms.push_back(block());
        builder.setInsertionPointToEnd(current);
        mlir::Value tag = ReussirRecordTagOp::create(builder, loc, ref);
        tag = mlir::arith::IndexCastOp::create(builder, loc,
                                               builder.getI32Type(), tag);
        llvm::SmallVector<int32_t> tags;
        llvm::SmallVector<mlir::ValueRange> operands;
        for (size_t i = 0; i + 1 < arms.size(); ++i) {
          tags.push_back(i);
          operands.emplace_back();
        }
        mlir::cf::SwitchOp::create(builder, loc, tag, arms.back(),
                                   mlir::ValueRange{}, tags,
                                   llvm::ArrayRef(arms).drop_back(), operands);
        for (auto [i, arm] : llvm::enumerate(arms)) {
          builder.setInsertionPointToEnd(arm);
          if (record.getMemberIsField()[i]) {
            finish(owner);
            continue;
          }
          auto member = getProjectedType(record.getMembers()[i], false,
                                         Capability::unspecified,
                                         refType.getAtomicKind());
          mlir::Value payload = ReussirRecordCoerceOp::create(
              builder, loc, projectedRefType(member), builder.getIndexAttr(i),
              ref);
          emitPayload(payload, owner);
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
        finish(owner);
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
    finish(owner);
  }
  void emitState(RcType type) {
    builder.setInsertionPointToEnd(states.lookup(type));
    mlir::Value rc = builder.getInsertionBlock()->getArgument(0);
    if (module->hasAttr(kSpecialPtrTagAttr) &&
        type.mayCarrySpecialPointerTag()) {
      mlir::Value immediate =
          ReussirRcIsImmediateOp::create(builder, loc, builder.getI1Type(), rc);
      auto *current = builder.getInsertionBlock();
      auto *heap = block();
      builder.setInsertionPointToEnd(current);
      mlir::cf::CondBranchOp::create(builder, loc, immediate, exit, heap);
      builder.setInsertionPointToEnd(heap);
    }
    bool atomic = type.getAtomicKind() == AtomicKind::atomic;
    mlir::Value count =
        atomic ? ReussirRcFetchSubOp::create(builder, loc, rc).getRefCount()
               : ReussirRcFetchOp::create(builder, loc, rc).getRefCount();
    mlir::Value one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
    mlir::Value unique = mlir::arith::CmpIOp::create(
        builder, loc, mlir::arith::CmpIPredicate::eq, count, one);
    auto *current = builder.getInsertionBlock();
    auto *destroy = block();
    auto *shared = block();
    builder.setInsertionPointToEnd(current);
    mlir::cf::CondBranchOp::create(builder, loc, unique, destroy, shared);
    builder.setInsertionPointToEnd(shared);
    if (!atomic) {
      mlir::Value decremented =
          mlir::arith::SubIOp::create(builder, loc, count, one);
      ReussirRcSetOp::create(builder, loc, rc, decremented);
    }
    mlir::cf::BranchOp::create(builder, loc, exit);
    builder.setInsertionPointToEnd(destroy);
    auto refType = RefType::get(builder.getContext(), type.getElementType(),
                                Capability::unspecified, type.getAtomicKind());
    mlir::Value ref = ReussirRcBorrowOp::create(builder, loc, refType, rc);
    emitPayload(ref, rc);
  }

public:
  TailDropEmitter(mlir::ModuleOp module, mlir::func::FuncOp function,
                  mlir::PatternRewriter &builder)
      : module(module), function(function), builder(builder),
        loc(function.getLoc()) {
    auto *entry = function.addEntryBlock();
    exit = block();
    mlir::func::ReturnOp::create(builder, loc);
    builder.setInsertionPointToEnd(entry);
    auto type = llvm::cast<RcType>(entry->getArgument(0).getType());
    mlir::cf::BranchOp::create(builder, loc, state(type),
                               entry->getArguments());
    for (size_t i = 0; i < pending.size(); ++i)
      emitState(pending[i]);
  }
};
} // namespace

bool emitTailDropRelease(mlir::Value rc, mlir::Location loc,
                         mlir::ModuleOp module, mlir::PatternRewriter &rewriter,
                         mlir::SymbolTableCollection *symbols) {
  auto type = llvm::cast<RcType>(rc.getType());
  auto record = llvm::dyn_cast<RecordType>(type.getElementType());
  if (!record || !record.getName() || !canIterate(type))
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
    rewriter.getContext()->getOrLoadDialect<mlir::cf::ControlFlowDialect>();
    rewriter.setInsertionPointToStart(module.getBody());
    function = mlir::func::FuncOp::create(rewriter, loc, name,
                                          rewriter.getFunctionType({type}, {}));
    function.setPrivate();
    function->setAttr(kTailDropReleaseAttr, rewriter.getUnitAttr());
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
