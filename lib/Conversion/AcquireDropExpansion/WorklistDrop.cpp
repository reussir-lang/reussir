//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache License v2.0 or
// the MIT License. See
// https://github.com/reussir-lang/reussir/blob/main/LICENSE
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//
#include "WorklistDrop.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/Transformation/SpecialPointerTag.h"
#include "TailDrop.h"
#include <format>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <string_view>

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
  bool tailRecursiveDrops;

public:
  WorklistGlue(mlir::ModuleOp module, mlir::PatternRewriter &builder,
               mlir::SymbolTableCollection *symbols, mlir::Location loc,
               bool tailRecursiveDrops = true)
      : module(module), builder(builder), symbols(symbols), loc(loc),
        tailRecursiveDrops(tailRecursiveDrops) {
    auto target = module->getAttrOfType<mlir::StringAttr>(
        mlir::LLVM::LLVMDialect::getTargetTripleAttrName());
    this->tailRecursiveDrops &=
        !target || !llvm::Triple(target.getValue()).isWasm();
  }

  mlir::func::FuncOp function(mlir::Type type, WorklistRole role,
                              llvm::function_ref<void(mlir::Value)> body,
                              bool indexed = false) {
    auto direction = isForwardDrop(module) ? WorklistDirection::Forward
                                           : WorklistDirection::Backward;
    auto key = WorklistRoleAttr::get(builder.getContext(), role, direction);
    llvm::SmallVector<mlir::Type> args{type};
    if (indexed)
      args.push_back(builder.getIndexType());
    auto functionType = builder.getFunctionType(args, {});
    std::string signature;
    llvm::raw_string_ostream stream(signature);
    printTypeWithRecordBodiesOnce(functionType, stream);
    auto nameSpace = std::format(
        "REUSSIR_DROP_{}_{}", std::string_view(stringifyWorklistRole(role)),
        std::string_view(stringifyWorklistDirection(direction)));
    auto name = mangledBlake3Symbol(nameSpace, signature);
    mlir::SymbolTableCollection ownSymbols;
    auto &table = (symbols ? *symbols : ownSymbols).getSymbolTable(module);
    if (auto existing = table.lookup<mlir::func::FuncOp>(name))
      return existing;
    mlir::OpBuilder::InsertionGuard guard(builder);
    builder.setInsertionPointToStart(module.getBody());
    auto function =
        mlir::func::FuncOp::create(builder, loc, name, functionType);
    function.setPrivate();
    function->setAttr(REUSSIR_WORKLIST_ROLE, key);
    function->setAttr(kWorklistGlueAttr, builder.getUnitAttr());
    function->setAttr("llvm.linkage",
                      builder.getAttr<mlir::LLVM::LinkageAttr>(
                          mlir::LLVM::linkage::Linkage::LinkonceODR));
    table.insert(function);
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

  bool dispatchLeaf(mlir::Value ref, llvm::function_ref<void()> onLeaf,
                    llvm::function_ref<void()> onOther) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto record = llvm::dyn_cast<RecordType>(refType.getElementType());
    if (!record || !record.isVariant() || record.hasObservableCleanup()) {
      return false;
    }
    llvm::SmallVector<bool> leaf;
    llvm::SmallVector<mlir::Attribute> tags;
    for (const auto &[i, member, field] :
         llvm::enumerate(record.getMembers(), record.getMemberIsField())) {
      leaf.push_back(field || isTriviallyCopyable(getProjectedType(
                                  member, false, Capability::shared)));
      tags.push_back(builder.getDenseI64ArrayAttr({int64_t(i)}));
    }
    if (!llvm::is_contained(leaf, true)) {
      return false;
    }
    auto dispatch = ReussirRecordDispatchOp::create(
        builder, loc, mlir::Type{}, ref, builder.getArrayAttr(tags),
        tags.size());
    mlir::OpBuilder::InsertionGuard guard(builder);
    for (const auto &[i, member, field] :
         llvm::enumerate(record.getMembers(), record.getMemberIsField())) {
      auto projected = getProjectedType(member, field, Capability::unspecified,
                                        refType.getAtomicKind());
      builder.createBlock(
          &dispatch.getRegions()[i], {},
          {RefType::get(builder.getContext(), projected,
                        Capability::unspecified, refType.getAtomicKind())},
          {loc});
      if (leaf[i])
        onLeaf();
      else
        onOther();
      ReussirScfYieldOp::create(builder, loc, nullptr);
    }
    return true;
  }

  void drop(mlir::Value ref, bool run) {
    auto body = [&](mlir::Value arg) {
      auto enqueue = [&] { schedule(walk(arg.getType()), arg, run); };
      if (!dispatchLeaf(arg, [] {}, enqueue))
        enqueue();
    };
    if (run) {
      auto entry = function(ref.getType(), WorklistRole::Entry, body);
      mlir::func::CallOp::create(builder, loc, entry, mlir::ValueRange{ref});
    } else {
      body(ref);
    }
  }

  // Whether cleanup can finish in this frame without a queued continuation.
  //
  // | Type (trivial case checked first) | Returns true when |
  // |---|---|
  // | Trivially copyable | Always |
  // | Nonrecursive record | No observable cleanup |
  // | Recursive record | No observable cleanup; tailRecursiveDrops enabled;
  // |                  | no worklist cleanup in the record or its descendants |
  // | Rigid RC | Always |
  // | Other RC | Payload qualifies; a TailLoop record also needs canApplyTailDrop |
  // | Nullable | Wrapped pointer type qualifies |
  // | Array | No target; a zero extent or qualifying element type |
  // | Cell | No mutex, flatlock, or rwlock; element type qualifies |
  // | Other | Never |
  bool canFinishInline(mlir::Type type) const {
    if (isTriviallyCopyable(type))
      return true;
    return llvm::TypeSwitch<mlir::Type, bool>(type)
        .Case<RecordType>([&](RecordType record) {
          return !record.hasObservableCleanup() &&
                 (!record.hasRecursiveCleanup() ||
                  (tailRecursiveDrops && !record.hasWorklistCleanup()));
        })
        .Case<RcType>([&](RcType rc) {
          if (rc.getCapability() == Capability::rigid)
            return true;
          auto record = llvm::dyn_cast<RecordType>(rc.getElementType());
          if (record &&
              record.classifyRecordDrop() == RecordDropKind::TailLoop &&
              !canApplyTailDrop(rc))
            return false;
          return canFinishInline(rc.getElementType());
        })
        .Case<NullableType>([&](NullableType nullable) {
          return canFinishInline(nullable.getPtrTy());
        })
        .Case<ArrayType>([&](ArrayType array) {
          return !array.hasTargetAttr() &&
                 (llvm::is_contained(array.getShape(), 0) ||
                  canFinishInline(array.getElementType()));
        })
        .Case<CellType>([&](CellType cell) {
          return !cell.getMutex() && !cell.getFlatlock() && !cell.getRwlock() &&
                 canFinishInline(cell.getElementType());
        })
        .Default(false);
  }

  void free(mlir::Value rc) {
    auto task =
        function(rc.getType(), WorklistRole::Free, [&](mlir::Value owner) {
          emitBoxFree(owner, loc, module, builder);
        });
    schedule(task, rc, false);
  }

  static bool canReleasePure(mlir::Type type) {
    if (isTriviallyCopyable(type))
      return true;
    if (auto nullable = llvm::dyn_cast<NullableType>(type))
      type = nullable.getPtrTy();
    if (auto rc = llvm::dyn_cast<RcType>(type))
      return rc.getCapability() == Capability::shared;
    auto record = llvm::dyn_cast<RecordType>(type);
    if (!record || record.hasObservableCleanup())
      return false;
    return llvm::all_of(
        llvm::zip(record.getMembers(), record.getMemberIsField()),
        [&](auto member) {
          auto [type, field] = member;
          auto projected = getProjectedType(type, field, Capability::shared);
          auto nested = llvm::dyn_cast<RecordType>(projected);
          return field || isTriviallyCopyable(projected) ||
                 ((!record.isCompound() || !nested || !nested.isVariant()) &&
                  canReleasePure(projected));
        });
  }

  // Drop one reference and report whether the child needs release.
  mlir::Value deadTest(mlir::Value child) {
    auto i1 = builder.getI1Type();
    // `cond ? value() : false`
    auto guarded = [&](mlir::Value cond,
                       llvm::function_ref<mlir::Value()> value) {
      auto branch = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{i1},
                                            cond, true, true);
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(branch.thenBlock());
      mlir::scf::YieldOp::create(builder, loc, mlir::ValueRange{value()});
      builder.setInsertionPointToStart(branch.elseBlock());
      mlir::Value no = mlir::arith::ConstantIntOp::create(builder, loc, i1, 0);
      mlir::scf::YieldOp::create(builder, loc, mlir::ValueRange{no});
      return branch.getResult(0);
    };
    if (auto nullable = llvm::dyn_cast<NullableType>(child.getType()))
      return guarded(ReussirNullableCheckOp::create(builder, loc, child), [&] {
        return deadTest(ReussirNullableCoerceOp::create(
            builder, loc, nullable.getPtrTy(), child));
      });
    auto type = llvm::cast<RcType>(child.getType());
    auto test = [&]() -> mlir::Value {
      bool atomic = type.getAtomicKind() == AtomicKind::atomic;
      mlir::Value count =
          atomic
              ? ReussirRcFetchSubOp::create(builder, loc, child).getRefCount()
              : ReussirRcFetchOp::create(builder, loc, child).getRefCount();
      mlir::Value one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
      mlir::Value unique = mlir::arith::CmpIOp::create(
          builder, loc, mlir::arith::CmpIPredicate::eq, count, one);
      if (!atomic) {
        auto branch = mlir::scf::IfOp::create(builder, loc, unique, true);
        mlir::OpBuilder::InsertionGuard guard(builder);
        builder.setInsertionPointToStart(branch.elseBlock());
        ReussirRcSetOp::create(
            builder, loc, child,
            mlir::arith::SubIOp::create(builder, loc, count, one));
      }
      return unique;
    };
    // An immediate owns no memory (see emitDecrement).
    if (module->hasAttr(kSpecialPtrTagAttr) &&
        type.mayCarrySpecialPointerTag()) {
      mlir::Value immediate =
          ReussirRcIsImmediateOp::create(builder, loc, i1, child);
      mlir::Value yes = mlir::arith::ConstantIntOp::create(builder, loc, i1, 1);
      return guarded(mlir::arith::XOrIOp::create(builder, loc, immediate, yes),
                     test);
    }
    return test();
  }

  void tailRelease(mlir::Value child) {
    if (auto nullable = llvm::dyn_cast<NullableType>(child.getType()))
      child = ReussirNullableCoerceOp::create(builder, loc, nullable.getPtrTy(),
                                              child);
    auto call = mlir::func::CallOp::create(
        builder, loc, releaseFunction(llvm::cast<RcType>(child.getType())),
        mlir::ValueRange{child});
    call->setAttr(kMustTailCallAttr, builder.getUnitAttr());
  }

  // Collect children before freeing the parent, then defer siblings and
  // continue directly with one child. Only used without observable cleanup.
  void releasePure(mlir::Value ref, mlir::Value box) {
    auto refType = llvm::cast<RefType>(ref.getType());
    auto type = refType.getElementType();
    if (auto record = llvm::dyn_cast<RecordType>(type);
        record && record.isVariant()) {
      llvm::SmallVector<mlir::Attribute> tags;
      for (size_t i = 0; i < record.getMembers().size(); ++i)
        tags.push_back(builder.getDenseI64ArrayAttr({int64_t(i)}));
      auto dispatch = ReussirRecordDispatchOp::create(
          builder, loc, mlir::Type{}, ref, builder.getArrayAttr(tags),
          tags.size());
      mlir::OpBuilder::InsertionGuard guard(builder);
      for (const auto &[i, member, field] :
           llvm::enumerate(record.getMembers(), record.getMemberIsField())) {
        auto projected = getProjectedType(
            member, field, Capability::unspecified, refType.getAtomicKind());
        auto *arm = builder.createBlock(
            &dispatch.getRegions()[i], {},
            {RefType::get(builder.getContext(), projected,
                          Capability::unspecified, refType.getAtomicKind())},
            {loc});
        if (field)
          emitBoxFree(box, loc, module, builder);
        else
          releasePure(arm->getArgument(0), box);
        ReussirScfYieldOp::create(builder, loc, nullptr);
      }
      return;
    }
    llvm::SmallVector<mlir::Value> children, dead;
    auto collect = [&](auto &&self, mlir::Value slot) -> void {
      auto slotType = llvm::cast<RefType>(slot.getType());
      auto valueType = slotType.getElementType();
      if (isTriviallyCopyable(valueType))
        return;
      if (auto record = llvm::dyn_cast<RecordType>(valueType)) {
        assert(record.isCompound());
        for (const auto &[i, member, field] :
             llvm::enumerate(record.getMembers(), record.getMemberIsField())) {
          if (field)
            continue;
          auto projected = getProjectedType(
              member, false, Capability::unspecified, slotType.getAtomicKind());
          self(self, ReussirRefProjectOp::create(
                         builder, loc,
                         RefType::get(builder.getContext(), projected,
                                      Capability::unspecified,
                                      slotType.getAtomicKind()),
                         slot, builder.getIndexAttr(i)));
        }
        return;
      }
      mlir::Value child =
          ReussirRefLoadOp::create(builder, loc, valueType, slot);
      children.push_back(child);
      dead.push_back(deadTest(child));
    };
    collect(collect, ref);
    emitBoxFree(box, loc, module, builder);
    if (children.empty())
      return;
    if (isForwardDrop(module)) {
      std::reverse(children.begin(), children.end());
      std::reverse(dead.begin(), dead.end());
    }
    auto defer = [&](size_t i) {
      auto branch = mlir::scf::IfOp::create(builder, loc, dead[i], false);
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(branch.thenBlock());
      mlir::Value child = children[i];
      if (auto nullable = llvm::dyn_cast<NullableType>(child.getType()))
        child = ReussirNullableCoerceOp::create(builder, loc,
                                                nullable.getPtrTy(), child);
      release(child, false);
    };
    size_t first = children.size() > 2 ? children.size() - 2 : 0;
    for (size_t i = 0; i < first; ++i)
      defer(i);
    // Leave one continuation in the caller instead of pushing and popping it.
    auto continueAt = [&](auto &&self, size_t n) -> void {
      if (n == first)
        return;
      auto branch =
          mlir::scf::IfOp::create(builder, loc, dead[n - 1], n > first + 1);
      mlir::OpBuilder::InsertionGuard guard(builder);
      builder.setInsertionPointToStart(branch.thenBlock());
      for (size_t i = first; i + 1 < n; ++i)
        defer(i);
      tailRelease(children[n - 1]);
      if (n > first + 1) {
        builder.setInsertionPointToStart(branch.elseBlock());
        self(self, n - 1);
      }
    };
    continueAt(continueAt, children.size());
  }

  // Release a dead box, including its payload and allocation.
  mlir::func::FuncOp releaseFunction(RcType type) {
    return function(type, WorklistRole::Release, [&](mlir::Value arg) {
      if (llvm::isa<FFIObjectType, ClosureType>(type.getElementType())) {
        if (type.getAtomicKind() == AtomicKind::atomic)
          ReussirRcIncOp::create(builder, loc, arg);
        auto dec = ReussirRcDecOp::create(builder, loc, NullableType{}, arg,
                                          mlir::IntegerAttr{},
                                          mlir::DenseI64ArrayAttr{});
        dec->setAttr(REUSSIR_NO_DEFER, builder.getUnitAttr());
        return;
      }
      if (isTriviallyCopyable(type.getElementType())) {
        emitBoxFree(arg, loc, module, builder);
        return;
      }
      auto refType =
          RefType::get(builder.getContext(), type.getElementType(),
                       Capability::unspecified, type.getAtomicKind());
      mlir::Value ref = ReussirRcBorrowOp::create(builder, loc, refType, arg);
      if (canFinishInline(type.getElementType())) {
        ReussirRefDropOp::create(builder, loc, ref, true, nullptr);
        emitBoxFree(arg, loc, module, builder);
        return;
      }
      auto record = llvm::dyn_cast<RecordType>(type.getElementType());
      auto target = module->getAttrOfType<mlir::StringAttr>(
          mlir::LLVM::LLVMDialect::getTargetTripleAttrName());
      if (record && !record.hasObservableCleanup() && canReleasePure(record) &&
          (!target || !llvm::Triple(target.getValue()).isWasm())) {
        releasePure(ref, arg);
        return;
      }
      if (isForwardDrop(module)) {
        free(arg);
        auto call = mlir::func::CallOp::create(builder, loc, walk(refType),
                                               mlir::ValueRange{ref});
        auto triple = module->getAttrOfType<mlir::StringAttr>(
            mlir::LLVM::LLVMDialect::getTargetTripleAttrName());
        if (!triple || !llvm::Triple(triple.getValue()).isWasm())
          call->setAttr(kMustTailCallAttr, builder.getUnitAttr());
      } else {
        ReussirRefDropOp::create(builder, loc, ref, true, nullptr);
        emitBoxFree(arg, loc, module, builder);
      }
    });
  }

  void release(mlir::Value rc, bool run) {
    auto type = llvm::cast<RcType>(rc.getType());
    auto payload = type.getElementType();
    if (isTriviallyCopyable(payload)) {
      emitBoxFree(rc, loc, module, builder);
      return;
    }
    auto task = releaseFunction(type);
    if (canFinishInline(payload)) {
      mlir::func::CallOp::create(builder, loc, task, mlir::ValueRange{rc});
      return;
    }
    auto enqueue = [&] {
      if (!run && !llvm::isa<FFIObjectType, ClosureType>(payload))
        ReussirRcDeferOp::create(
            builder, loc, mlir::FlatSymbolRefAttr::get(task.getSymNameAttr()),
            rc);
      else
        schedule(task, rc, run);
    };
    if (!llvm::isa<RecordType>(payload)) {
      enqueue();
      return;
    }
    auto refType = RefType::get(builder.getContext(), payload,
                                Capability::unspecified, type.getAtomicKind());
    mlir::Value ref = ReussirRcBorrowOp::create(builder, loc, refType, rc);
    if (!dispatchLeaf(
            ref, [&] { emitBoxFree(rc, loc, module, builder); }, enqueue))
      enqueue();
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
                         mlir::SymbolTableCollection *symbols, bool run,
                         bool tailRecursiveDrops) {
  WorklistGlue(rewriter.getInsertionBlock()
                   ->getParentOp()
                   ->getParentOfType<mlir::ModuleOp>(),
               rewriter, symbols, loc, tailRecursiveDrops)
      .release(rc, run);
}

void emitWorklistDecrement(mlir::Value rc, mlir::Location loc,
                           mlir::PatternRewriter &rewriter,
                           mlir::SymbolTableCollection *symbols, bool run,
                           bool tailRecursiveDrops) {
  auto type = llvm::cast<RcType>(rc.getType());
  auto module = rewriter.getInsertionBlock()
                    ->getParentOp()
                    ->getParentOfType<mlir::ModuleOp>();
  auto target = module->getAttrOfType<mlir::StringAttr>(
      mlir::LLVM::LLVMDialect::getTargetTripleAttrName());
  if (auto record = llvm::dyn_cast<RecordType>(type.getElementType());
      record && tailRecursiveDrops && !record.hasObservableCleanup() &&
      (!target || !llvm::Triple(target.getValue()).isWasm()) &&
      emitTailDropRelease(rc, loc, module, rewriter, symbols))
    return;
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
  emitWorklistRelease(rc, loc, rewriter, symbols, run, tailRecursiveDrops);
}
} // namespace reussir
