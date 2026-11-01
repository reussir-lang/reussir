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

  mlir::func::FuncOp releaseFunction(RcType type);
  void release(mlir::Value rc, bool run);

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
};
} // namespace

} // namespace reussir
