//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache-2.0 or MIT.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//
#include "WorklistDrop.h"
#include "Reussir/IR/RecordDrop.h"
#include "Reussir/Transformation/SpecialPointerTag.h"
#include "Sync/IR/SyncOps.h"
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/ControlFlow/IR/ControlFlowOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>

namespace reussir {
namespace {
enum class Action : unsigned { Drop, Release, Free, Unlock };

class WorklistDropEmitter {
  mlir::ModuleOp module;
  mlir::func::FuncOp function;
  mlir::PatternRewriter &builder;
  mlir::SymbolTable &symbols;
  mlir::Location loc;
  mlir::Type ptr;
  llvm::DenseMap<std::pair<mlir::Type, unsigned>, mlir::func::FuncOp> callbacks;
  struct Pending {
    mlir::Type type;
    Action action;
    mlir::func::FuncOp function;
  };
  llvm::SmallVector<Pending> pending;

  mlir::Value asPointer(mlir::Value value) {
    if (value.getType() == ptr)
      return value;
    return mlir::UnrealizedConversionCastOp::create(
               builder, loc, mlir::TypeRange{ptr}, mlir::ValueRange{value})
        .getResult(0);
  }
  mlir::func::CallOp runtime(llvm::StringRef name, mlir::TypeRange results,
                             mlir::ValueRange args = {}) {
    auto callee = symbols.lookup<mlir::func::FuncOp>(name);
    if (!callee) {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(module.getBody());
      callee = mlir::func::FuncOp::create(
          builder, loc, name,
          builder.getFunctionType(args.getTypes(), results));
      callee.setPrivate();
      symbols.insert(callee);
    }
    return mlir::func::CallOp::create(builder, loc, callee, args);
  }
  mlir::func::FuncOp callback(mlir::Type type, Action action) {
    auto [it, inserted] = callbacks.try_emplace({type, unsigned(action)});
    if (inserted) {
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPoint(function);
      auto name =
          (function.getSymName() + ".step." + llvm::Twine(callbacks.size()))
              .str();
      auto callee = mlir::func::FuncOp::create(
          builder, loc, name, builder.getFunctionType({type, ptr}, {}));
      callee.setPrivate();
      callee->setAttr("llvm.linkage", builder.getAttr<mlir::LLVM::LinkageAttr>(
                                          mlir::LLVM::Linkage::Private));
      inheritSanitizerPassthrough(module, callee);
      symbols.insert(callee);
      it->second = callee;
      pending.push_back({type, action, callee});
    }
    return it->second;
  }
  void enqueue(mlir::Value object, mlir::Value context, Action action) {
    auto callee = callback(object.getType(), action);
    auto address =
        mlir::func::ConstantOp::create(builder, loc, callee.getFunctionType(),
                                       mlir::FlatSymbolRefAttr::get(callee));
    runtime("__reussir_enqueue_dtor", {},
            {context, asPointer(object), asPointer(address)});
  }
  RefType projected(mlir::Type type, RefType parent) {
    return RefType::get(builder.getContext(), type, Capability::unspecified,
                        parent.getAtomicKind());
  }
  void free(mlir::Value owner) {
    auto rc = llvm::cast<RcType>(owner.getType());
    auto box = rc.getInnerBoxType();
    mlir::DataLayout layout(module);
    auto align = layout.getTypeABIAlignment(box);
    auto record = llvm::dyn_cast<RecordType>(rc.getElementType());
    auto tokenType =
        box.hasDynamicArrayPayload() ||
                (record && record.isVariant() && box.isHeaderFused() &&
                 !record.getFixed())
            ? TokenType::getDynamic(builder.getContext(), align)
            : TokenType::get(builder.getContext(), align,
                             layout.getTypeSize(box).getFixedValue());
    auto token = ReussirRcReinterpretOp::create(builder, loc, tokenType, owner);
    ReussirTokenFreeOp::create(builder, loc, token.getReinterpreted());
  }
  void releaseHeap(mlir::Value owner, mlir::Value context) {
    auto rc = llvm::cast<RcType>(owner.getType());
    bool atomic = rc.getAtomicKind() == AtomicKind::atomic;
    mlir::Value count =
        atomic ? ReussirRcFetchSubOp::create(builder, loc, owner).getRefCount()
               : ReussirRcFetchOp::create(builder, loc, owner).getRefCount();
    mlir::Value one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
    auto unique = mlir::arith::CmpIOp::create(
        builder, loc, mlir::arith::CmpIPredicate::eq, count, one);
    auto branch = mlir::scf::IfOp::create(builder, loc, unique, !atomic);
    mlir::OpBuilder::InsertionGuard guard(builder);
    if (!atomic) {
      builder.setInsertionPointToStart(branch.elseBlock());
      auto countNext = mlir::arith::SubIOp::create(builder, loc, count, one);
      ReussirRcSetOp::create(builder, loc, owner, countNext);
    }
    builder.setInsertionPointToStart(branch.thenBlock());
    auto refType = RefType::get(builder.getContext(), rc.getElementType(),
                                Capability::unspecified, rc.getAtomicKind());
    auto ref = ReussirRcBorrowOp::create(builder, loc, refType, owner);
    enqueue(owner, context, Action::Free);
    drop(ref, context);
  }
  void release(mlir::Value owner, mlir::Value context) {
    auto rc = llvm::cast<RcType>(owner.getType());
    if (module->hasAttr(kSpecialPtrTagAttr) && rc.mayCarrySpecialPointerTag()) {
      auto immediate = ReussirRcIsImmediateOp::create(
          builder, loc, builder.getI1Type(), owner);
      auto branch = mlir::scf::IfOp::create(builder, loc, immediate, true);
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(branch.elseBlock());
      releaseHeap(owner, context);
    } else {
      releaseHeap(owner, context);
    }
  }
  bool managed(RcType rc) {
    return rc.getCapability() == Capability::shared &&
           !llvm::isa<FFIObjectType, ClosureType>(rc.getElementType());
  }
  void array(mlir::Value ref, mlir::Value context, ArrayType type) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto view = ReussirArrayViewOp::create(builder, loc,
                                           getArrayViewMemRefType(type), ref)
                    .getView();
    mlir::Value zero = mlir::arith::ConstantIndexOp::create(builder, loc, 0);
    mlir::Value one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
    llvm::SmallVector<mlir::Value> lower(type.getRank(), zero), upper,
        steps(type.getRank(), one);
    for (auto [i, extent] : llvm::enumerate(type.getShape()))
      upper.push_back(
          mlir::ShapedType::isDynamic(extent)
              ? mlir::memref::DimOp::create(builder, loc, view, int64_t(i))
                    .getResult()
              : mlir::arith::ConstantIndexOp::create(builder, loc, extent)
                    .getResult());
    mlir::scf::buildLoopNest(
        builder, loc, lower, upper, steps,
        [&](mlir::OpBuilder &bodyBuilder, mlir::Location,
            mlir::ValueRange indices) {
          mlir::OpBuilder::InsertionGuard guard(builder);
          builder.restoreInsertionPoint(bodyBuilder.saveInsertionPoint());
          mlir::Value current = view;
          for (auto [i, index] : llvm::enumerate(indices)) {
            auto last =
                mlir::arith::SubIOp::create(builder, loc, upper[i], one);
            auto reverse =
                mlir::arith::SubIOp::create(builder, loc, last, index);
            mlir::Type result =
                i + 1 == type.getRank()
                    ? mlir::Type(projected(type.getElementType(), refType))
                    : mlir::Type(getProjectedArrayViewType(
                          llvm::cast<mlir::MemRefType>(current.getType())));
            current = ReussirArrayProjectOp::create(builder, loc, result,
                                                    current, reverse);
          }
          enqueue(current, context, Action::Drop);
        });
  }
  mlir::Value cellStorage(mlir::Value ref, CellType cell) {
    return ReussirRefToMemrefOp::create(
        builder, loc, mlir::MemRefType::get({}, lockGuardedStorageType(cell)),
        ref);
  }
  void unlock(mlir::Value ref) {
    auto cell = llvm::cast<CellType>(
        llvm::cast<RefType>(ref.getType()).getElementType());
    auto storage = cellStorage(ref, cell);
    if (cell.getMutex()) {
      auto raw = mlir::sync::SyncMutexGetRawMutexOp::create(
          builder, loc,
          mlir::MemRefType::get(
              {}, mlir::sync::RawMutexType::get(builder.getContext())),
          storage);
      mlir::sync::SyncRawMutexUnlockOp::create(builder, loc, raw);
    } else if (cell.getRwlock()) {
      auto raw = mlir::sync::SyncRwLockGetRawRwLockOp::create(
          builder, loc,
          mlir::MemRefType::get(
              {}, mlir::sync::RawRwLockType::get(builder.getContext())),
          storage);
      mlir::sync::SyncRawRwLockWriteUnlockOp::create(builder, loc, raw);
    } else {
      mlir::sync::SyncCombiningLockReleaseOp::create(builder, loc, storage);
    }
  }
  void cell(mlir::Value ref, mlir::Value context, CellType type) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto slotType = projected(type.getElementType(), refType);
    if (!type.getLockGuarded()) {
      auto slot = ReussirRefProjectOp::create(builder, loc, slotType, ref,
                                              builder.getIndexAttr(0));
      enqueue(slot, context, Action::Drop);
      return;
    }
    auto storage = cellStorage(ref, type);
    auto payloadType = mlir::MemRefType::get({}, type.getElementType());
    mlir::Value payload;
    if (type.getMutex()) {
      auto raw = mlir::sync::SyncMutexGetRawMutexOp::create(
          builder, loc,
          mlir::MemRefType::get(
              {}, mlir::sync::RawMutexType::get(builder.getContext())),
          storage);
      mlir::sync::SyncRawMutexLockOp::create(builder, loc, raw);
      payload = mlir::sync::SyncMutexGetPayloadOp::create(builder, loc,
                                                          payloadType, storage);
    } else if (type.getRwlock()) {
      auto raw = mlir::sync::SyncRwLockGetRawRwLockOp::create(
          builder, loc,
          mlir::MemRefType::get(
              {}, mlir::sync::RawRwLockType::get(builder.getContext())),
          storage);
      mlir::sync::SyncRawRwLockWriteLockOp::create(builder, loc, raw);
      payload = mlir::sync::SyncRwLockGetPayloadOp::create(
          builder, loc, payloadType, storage);
    } else {
      auto acquired = mlir::sync::SyncCombiningLockTryAcquireOp::create(
          builder, loc, builder.getI1Type(), storage);
      mlir::cf::AssertOp::create(builder, loc, acquired,
                                 "destroying a contended combining-lock cell");
      payload = mlir::sync::SyncCombiningLockGetPayloadOp::create(
                    builder, loc, mlir::TypeRange{payloadType}, storage)
                    .getResult(0);
    }
    auto slot = ReussirRefFromMemrefOp::create(builder, loc, slotType, payload);
    enqueue(ref, context, Action::Unlock);
    enqueue(slot, context, Action::Drop);
  }
  void drop(mlir::Value ref, mlir::Value context) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto type = refType.getElementType();
    if (isTriviallyCopyable(type))
      return;
    if (auto rc = llvm::dyn_cast<RcType>(type); rc && managed(rc)) {
      auto value = ReussirRefLoadOp::create(builder, loc, rc, ref);
      release(value, context);
      return;
    }
    if (auto nullable = llvm::dyn_cast<NullableType>(type)) {
      auto rc = llvm::dyn_cast<RcType>(nullable.getPtrTy());
      if (rc && managed(rc)) {
        auto value = ReussirRefLoadOp::create(builder, loc, nullable, ref);
        auto present = ReussirNullableCheckOp::create(builder, loc, value);
        auto branch = mlir::scf::IfOp::create(builder, loc, present, false);
        mlir::OpBuilder::InsertionGuard guard(builder);
        builder.setInsertionPointToStart(branch.thenBlock());
        auto owner = ReussirNullableCoerceOp::create(builder, loc, rc, value);
        release(owner, context);
        return;
      }
    }
    if (auto record = llvm::dyn_cast<RecordType>(type)) {
      if (record.isVariant()) {
        llvm::SmallVector<mlir::Attribute> tags;
        for (size_t i = 0; i < record.getMembers().size(); ++i)
          tags.push_back(builder.getDenseI64ArrayAttr({int64_t(i)}));
        auto dispatch = ReussirRecordDispatchOp::create(
            builder, loc, mlir::Type{}, ref, builder.getArrayAttr(tags),
            tags.size());
        mlir::OpBuilder::InsertionGuard guard(builder);
        for (size_t i = 0; i < tags.size(); ++i) {
          bool field = record.getMemberIsField()[i];
          auto member = getProjectedType(record.getMembers()[i], field,
                                         Capability::unspecified,
                                         refType.getAtomicKind());
          auto *arm = builder.createBlock(&dispatch.getRegions()[i], {},
                                          {projected(member, refType)}, {loc});
          if (!field && !isTriviallyCopyable(member))
            enqueue(arm->getArgument(0), context, Action::Drop);
          ReussirScfYieldOp::create(builder, loc, nullptr);
        }
      } else {
        for (size_t i = record.getMembers().size(); i-- > 0;) {
          if (record.getMemberIsField()[i])
            continue;
          auto member = getProjectedType(record.getMembers()[i], false,
                                         Capability::unspecified,
                                         refType.getAtomicKind());
          if (isTriviallyCopyable(member))
            continue;
          auto slot = ReussirRefProjectOp::create(builder, loc,
                                                  projected(member, refType),
                                                  ref, builder.getIndexAttr(i));
          enqueue(slot, context, Action::Drop);
        }
      }
      return;
    }
    if (auto type = llvm::dyn_cast<ArrayType>(refType.getElementType());
        type && !type.hasTargetAttr()) {
      array(ref, context, type);
      return;
    }
    if (auto type = llvm::dyn_cast<CellType>(refType.getElementType())) {
      cell(ref, context, type);
      return;
    }
    ReussirRefDropOp::create(builder, loc, ref);
  }

public:
  WorklistDropEmitter(mlir::ModuleOp module, mlir::func::FuncOp function,
                      mlir::PatternRewriter &builder,
                      mlir::SymbolTable &symbols, Action action)
      : module(module), function(function), builder(builder), symbols(symbols),
        loc(function.getLoc()),
        ptr(mlir::LLVM::LLVMPointerType::get(builder.getContext())) {
    builder.setInsertionPointToStart(function.addEntryBlock());
    auto size =
        runtime("__reussir_dtor_worklist_size", {builder.getIndexType()})
            .getResult(0);
    auto align =
        runtime("__reussir_dtor_worklist_align", {builder.getIndexType()})
            .getResult(0);
    auto one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
    auto extra = mlir::arith::SubIOp::create(builder, loc, align, one);
    auto bytes = mlir::arith::AddIOp::create(builder, loc, size, extra);
    auto word = builder.getIntegerType(
        mlir::DataLayout(module).getTypeSizeInBits(ptr).getFixedValue());
    auto count = mlir::arith::IndexCastOp::create(builder, loc, word, bytes);
    auto storage = mlir::LLVM::AllocaOp::create(builder, loc, ptr,
                                                builder.getI8Type(), count, 1);
    auto address = mlir::LLVM::PtrToIntOp::create(builder, loc, word, storage);
    auto padding = mlir::arith::IndexCastOp::create(builder, loc, word, extra);
    auto rounded = mlir::arith::AddIOp::create(builder, loc, address, padding);
    auto zero = mlir::arith::ConstantIntOp::create(builder, loc, word, 0);
    auto alignment =
        mlir::arith::IndexCastOp::create(builder, loc, word, align);
    auto mask = mlir::arith::SubIOp::create(builder, loc, zero, alignment);
    auto aligned = mlir::arith::AndIOp::create(builder, loc, rounded, mask);
    auto pointer =
        mlir::LLVM::IntToPtrOp::create(builder, loc, ptr, aligned, nullptr);
    auto context = runtime("__reussir_create_dtor_worklist", {ptr}, {pointer})
                       .getResult(0);
    auto root = callback(function.getArgumentTypes()[0], action);
    mlir::func::CallOp::create(
        builder, loc, root, mlir::ValueRange{function.getArgument(0), context});
    runtime("__reussir_drain_dtor_worklist", {}, {context});
    runtime("__reussir_destroy_dtor_worklist", {}, {context});
    mlir::func::ReturnOp::create(builder, loc);
    for (size_t i = 0; i < pending.size(); ++i) {
      auto item = pending[i];
      builder.setInsertionPointToStart(item.function.addEntryBlock());
      auto object = item.function.getArgument(0);
      auto context = item.function.getArgument(1);
      switch (item.action) {
      case Action::Drop:
        drop(object, context);
        break;
      case Action::Release:
        release(object, context);
        break;
      case Action::Free:
        free(object);
        break;
      case Action::Unlock:
        unlock(object);
        break;
      }
      mlir::func::ReturnOp::create(builder, loc);
    }
  }
};

bool emit(mlir::Value object, RecordType record, AtomicKind atomic,
          Action action, mlir::Location loc, mlir::ModuleOp module,
          mlir::PatternRewriter &rewriter,
          mlir::SymbolTableCollection *symbols) {
  if (!record || !record.getName() ||
      record.classifyRecordDrop() != RecordDropKind::Worklist)
    return false;
  std::optional<mlir::SymbolTable> ownTable;
  auto &table =
      symbols ? symbols->getSymbolTable(module) : ownTable.emplace(module);
  auto name =
      (record.getDtorName(atomic).getValue() +
       (action == Action::Drop ? ".worklist_drop" : ".worklist_release"))
          .str();
  auto function = table.lookup<mlir::func::FuncOp>(name);
  if (!function) {
    mlir::OpBuilder::InsertionGuard guard(rewriter);
    rewriter.setInsertionPointToStart(module.getBody());
    function = mlir::func::FuncOp::create(
        rewriter, loc, name, rewriter.getFunctionType({object.getType()}, {}));
    function.setPrivate();
    function->setAttr("llvm.linkage", rewriter.getAttr<mlir::LLVM::LinkageAttr>(
                                          mlir::LLVM::Linkage::LinkonceODR));
    inheritSanitizerPassthrough(module, function);
    table.insert(function);
    WorklistDropEmitter(module, function, rewriter, table, action);
  }
  mlir::func::CallOp::create(rewriter, loc, function, mlir::ValueRange{object});
  return true;
}
} // namespace

bool emitWorklistDropRelease(mlir::Value rc, mlir::Location loc,
                             mlir::ModuleOp module,
                             mlir::PatternRewriter &rewriter,
                             mlir::SymbolTableCollection *symbols) {
  auto type = llvm::cast<RcType>(rc.getType());
  return type.getCapability() == Capability::shared &&
         emit(rc, llvm::dyn_cast<RecordType>(type.getElementType()),
              type.getAtomicKind(), Action::Release, loc, module, rewriter,
              symbols);
}
bool emitWorklistDrop(mlir::Value ref, mlir::Location loc,
                      mlir::ModuleOp module, mlir::PatternRewriter &rewriter,
                      mlir::SymbolTableCollection *symbols) {
  auto type = llvm::cast<RefType>(ref.getType());
  return type.getCapability() == Capability::unspecified &&
         emit(ref, llvm::dyn_cast<RecordType>(type.getElementType()),
              type.getAtomicKind(), Action::Drop, loc, module, rewriter,
              symbols);
}
} // namespace reussir
