// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include "Reussir/Analysis/PjrtDonationAnalysis.h"
#include "Reussir/Conversion/Blake3Symbol.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Transformation/Passes.h"
#include <algorithm>
#include <llvm/ADT/BitVector.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/BLAKE3.h>
#include <map>
#include <mlir/Bytecode/BytecodeWriter.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Parser/Parser.h>
#include <numeric>

namespace reussir {
#define GEN_PASS_DEF_REUSSIRPJRTDONATIONPASS
#include "Reussir/Transformation/Passes.h.inc"
namespace {
using llvm::SmallVector;
using Decision = PjrtDonationAnalysis::Decision;
using Indices = SmallVector<int32_t>;
struct CallPlan {
  ReussirPJRTJitCallOp call;
  SmallVector<mlir::Value> owners;
  SmallVector<Decision> decisions;
  Indices arguments, mapping;
};
struct Candidate {
  Indices arguments, mapping, donors;
  uint64_t benefit = 0;
  mlir::FlatSymbolRefAttr symbol;
};
struct Family {
  ReussirIFRTBytecodeOp code;
  mlir::OwningOpRef<mlir::ModuleOp> module;
  SmallVector<CallPlan, 0> calls;
  SmallVector<Candidate> candidates;
};

uint64_t tensorBytes(mlir::Type type) {
  auto tensor = llvm::dyn_cast<mlir::RankedTensorType>(type);
  if (!tensor || !tensor.hasStaticShape() ||
      !tensor.getElementType().isIntOrFloat())
    return 0;
  uint64_t bits = tensor.getElementTypeBitWidth();
  uint64_t bytes = (bits + 7) / 8;
  for (int64_t dim : tensor.getShape()) {
    if (dim < 0 || (dim && bytes > UINT64_MAX / uint64_t(dim)))
      return 0;
    bytes *= dim;
  }
  return bytes;
}

bool unsharedIterationValue(mlir::Value value) {
  if (!llvm::isa_and_nonnull<ReussirArrayFromIfrtOp, ReussirArrayToDeviceOp>(
          value.getDefiningOp()))
    return false;
  return llvm::all_of(value.getUsers(), [](mlir::Operation *op) {
    return llvm::isa<mlir::scf::YieldOp, ReussirArrayToIfrtOp,
                     ReussirArrayToHostOp, ReussirRcIsUniqueOp>(op);
  });
}

void carryLoopGuards(mlir::ModuleOp module) {
  SmallVector<mlir::Operation *> loops;
  module.walk<mlir::WalkOrder::PostOrder>([&](mlir::Operation *op) {
    if (llvm::isa<mlir::scf::ForOp, mlir::scf::WhileOp>(op))
      loops.push_back(op);
  });
  for (auto *operation : loops) {
    auto forOp = llvm::dyn_cast<mlir::scf::ForOp>(operation);
    auto whileOp = llvm::dyn_cast<mlir::scf::WhileOp>(operation);
    auto arguments =
        forOp ? forOp.getRegionIterArgs() : whileOp.getAfterArguments();
    auto yield = llvm::dyn_cast<mlir::scf::YieldOp>(
        forOp ? forOp.getBody()->getTerminator()
              : whileOp.getAfter().front().getTerminator());
    if (!yield)
      continue;
    SmallVector<unsigned> carried;
    SmallVector<SmallVector<ReussirRcIsUniqueOp, 2>, 2> checks;
    for (auto [i, arg] : llvm::enumerate(arguments)) {
      if (i >= yield.getNumOperands() ||
          !unsharedIterationValue(yield.getOperand(i)))
        continue;
      if (whileOp && (i >= whileOp.getBeforeArguments().size() ||
                      whileOp.getConditionOp().getArgs()[i] !=
                          whileOp.getBeforeArguments()[i]))
        continue;
      if (whileOp &&
          !llvm::all_of(
              whileOp.getBeforeArguments()[i].getUsers(),
              [&](mlir::Operation *user) {
                return user == whileOp.getConditionOp().getOperation() ||
                       llvm::isa<ReussirArrayToHostOp, ReussirRcIsUniqueOp>(
                           user);
              }))
        continue;
      SmallVector<ReussirRcIsUniqueOp, 2> found;
      bool safe = true;
      for (auto *user : arg.getUsers()) {
        if (auto check = llvm::dyn_cast<ReussirRcIsUniqueOp>(user);
            check && check->hasAttr("pjrt.donation_guard"))
          found.push_back(check);
        else if (!llvm::isa<ReussirArrayToIfrtOp, ReussirArrayToHostOp,
                            ReussirRcDecOp>(user))
          safe = false;
      }
      if (safe && !found.empty()) {
        carried.push_back(i);
        checks.push_back(std::move(found));
      }
    }
    if (carried.empty())
      continue;
    mlir::OpBuilder builder(operation);
    auto loc = operation->getLoc();
    SmallVector<mlir::Value> inits(forOp ? forOp.getInitArgs()
                                         : whileOp.getInits());
    for (unsigned i : carried)
      inits.push_back(ReussirRcIsUniqueOp::create(
          builder, loc, builder.getI1Type(), inits[i]));
    SmallVector<mlir::Value> replacements;
    mlir::Block *body;
    if (forOp) {
      auto next = mlir::scf::ForOp::create(builder, loc, forOp.getLowerBound(),
                                           forOp.getUpperBound(),
                                           forOp.getStep(), inits);
      next->setDiscardableAttrs(forOp->getDiscardableAttrDictionary());
      next.getRegion().takeBody(forOp.getRegion());
      body = next.getBody();
      llvm::append_range(replacements,
                         next.getResults().take_front(forOp.getNumResults()));
    } else {
      SmallVector<mlir::Type> types(whileOp.getResultTypes());
      types.append(carried.size(), builder.getI1Type());
      auto next = mlir::scf::WhileOp::create(builder, loc, types, inits);
      next->setDiscardableAttrs(whileOp->getDiscardableAttrDictionary());
      next.getBefore().takeBody(whileOp.getBefore());
      next.getAfter().takeBody(whileOp.getAfter());
      auto condition = next.getConditionOp();
      for (unsigned i = 0; i < carried.size(); ++i) {
        auto flag =
            next.getBefore().front().addArgument(builder.getI1Type(), loc);
        condition.getArgsMutable().append(flag);
      }
      body = &next.getAfter().front();
      llvm::append_range(replacements,
                         next.getResults().take_front(whileOp.getNumResults()));
    }
    for (auto &group : checks) {
      auto flag = body->addArgument(builder.getI1Type(), loc);
      for (auto check : group) {
        check.replaceAllUsesWith(flag);
        check.erase();
      }
      builder.setInsertionPoint(yield);
      auto next = mlir::arith::ConstantIntOp::create(builder, loc, 1, 1);
      yield.getResultsMutable().append(next.getResult());
    }
    operation->replaceAllUsesWith(replacements);
    operation->erase();
  }
  module.walk(
      [](ReussirRcIsUniqueOp op) { op->removeAttr("pjrt.donation_guard"); });
}

mlir::LogicalResult
materializeExplicitContracts(mlir::ArrayRef<ReussirPJRTJitCallOp> calls,
                             mlir::SymbolTableCollection &symbols) {
  llvm::DenseMap<mlir::Operation *, mlir::OwningOpRef<mlir::ModuleOp>> decoded;
  llvm::DenseMap<std::pair<mlir::Operation *, mlir::Attribute>,
                 mlir::FlatSymbolRefAttr>
      emitted;
  for (auto call : calls) {
    if ((call.getDonatedInputIndices().empty() &&
         call.getIoAliases().empty()) ||
        call->hasAttr("pjrt.donation_materialized"))
      continue;
    auto code = symbols.lookupNearestSymbolFrom<ReussirIFRTBytecodeOp>(
        call, call.getCalleeAttr());
    if (!code)
      return call.emitOpError("cannot resolve donation kernel");
    mlir::OpBuilder builder(call);
    auto key = builder.getDictionaryAttr(
        {builder.getNamedAttr("callee", call.getCalleeAttr()),
         builder.getNamedAttr("donors", call.getDonatedInputIndicesAttr()),
         builder.getNamedAttr("aliases", call.getIoAliasesAttr())});
    auto cacheKey = std::make_pair(code.getOperation(), mlir::Attribute(key));
    auto symbol = emitted.lookup(cacheKey);
    if (!symbol) {
      auto [it, inserted] = decoded.try_emplace(code);
      if (inserted)
        it->second = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                             call.getContext());
      if (!it->second)
        return mlir::failure();
      auto copy = mlir::OwningOpRef<mlir::ModuleOp>(it->second->clone());
      auto entry = copy->lookupSymbol<mlir::func::FuncOp>("main");
      for (int32_t i : call.getDonatedInputIndices())
        entry.setArgAttr(i, "jax.buffer_donor", builder.getBoolAttr(true));
      for (auto attribute : call.getIoAliases()) {
        auto alias = llvm::cast<mlir::DenseI32ArrayAttr>(attribute);
        entry.setArgAttr(alias[0], "tf.aliasing_output",
                         builder.getI32IntegerAttr(alias[1]));
      }
      std::string bytes;
      llvm::raw_string_ostream stream(bytes);
      if (mlir::failed(mlir::writeBytecodeToFile(*copy, stream)))
        return mlir::failure();
      auto name = mangledBlake3Symbol("REUSSIR_IFRT_BYTECODE", bytes);
      auto &table = symbols.getSymbolTable(
          mlir::SymbolTable::getNearestSymbolTable(code));
      auto target = table.lookup<ReussirIFRTBytecodeOp>(name);
      if (!target) {
        llvm::BLAKE3 hash;
        hash.update(bytes);
        builder.setInsertionPoint(code);
        target = ReussirIFRTBytecodeOp::create(
            builder, code.getLoc(), builder.getStringAttr(name),
            builder.getStringAttr("private"), builder.getStringAttr(bytes),
            builder.getStringAttr(llvm::toHex(hash.final(), true)));
        target->setAttr("kernel_type",
                        mlir::TypeAttr::get(entry.getFunctionType()));
        table.insert(target);
      }
      symbol = mlir::FlatSymbolRefAttr::get(target.getSymNameAttr());
      emitted[cacheKey] = symbol;
    }
    call.setCalleeAttr(symbol);
    call->setAttr("pjrt.donation_materialized", builder.getUnitAttr());
  }
  return mlir::success();
}

struct ReussirPjrtDonationPass
    : impl::ReussirPjrtDonationPassBase<ReussirPjrtDonationPass> {
  using Base::Base;
  void runOnOperation() override {
    auto module = getOperation();
    bool hasKernels = false;
    module.walk<mlir::WalkOrder::PreOrder>([&](mlir::Operation *op) {
      if (llvm::isa<ReussirIFRTBytecodeOp>(op)) {
        hasKernels = true;
        return mlir::WalkResult::interrupt();
      }
      return llvm::isa<mlir::func::FuncOp>(op) ? mlir::WalkResult::skip()
                                               : mlir::WalkResult::advance();
    });
    if (!hasKernels) {
      markAllAnalysesPreserved();
      return;
    }
    SmallVector<ReussirPJRTJitCallOp> calls;
    module.walk([&](ReussirPJRTJitCallOp call) {
      if (call.getDonationVariants().empty())
        calls.push_back(call);
    });
    if (calls.empty())
      return;
    mlir::SymbolTableCollection symbols;
    if (mlir::failed(materializeExplicitContracts(calls, symbols))) {
      signalPassFailure();
      return;
    }
    if (maxVariants <= 1 || maxExtraBytes == 0 || maxCandidates == 0)
      return;
    PjrtDonationAnalysis analysis(module, maxAnalysisWork);
    llvm::MapVector<mlir::Operation *, Family> families;
    for (auto call : calls) {
      if (!call.getIoAliases().empty() ||
          !call.getDonatedInputIndices().empty())
        continue;
      auto code = symbols.lookupNearestSymbolFrom<ReussirIFRTBytecodeOp>(
          call, call.getCalleeAttr());
      if (!code)
        continue;
      CallPlan plan;
      plan.call = call;
      llvm::DenseMap<mlir::Value, int32_t> representatives;
      for (auto [i, input] : llvm::enumerate(call.getInputs())) {
        auto view = input.getDefiningOp<ReussirArrayToIfrtOp>();
        mlir::Value owner = view ? view.getArray() : mlir::Value{};
        plan.owners.push_back(owner);
        plan.decisions.push_back(owner ? analysis.analyze(owner, call)
                                       : Decision::Forbidden);
        auto [it, inserted] =
            representatives.try_emplace(owner ? owner : input, i);
        bool keep =
            inserted ||
            call.getInputs()[it->second].getType() != input.getType() ||
            (call.getArgAttrs() &&
             (*call.getArgAttrs())[i] != (*call.getArgAttrs())[it->second]);
        if (keep)
          plan.arguments.push_back(i);
        plan.mapping.push_back(keep ? int32_t(i) : it->second);
      }
      if (llvm::all_of(plan.decisions,
                       [](auto d) { return d == Decision::Forbidden; }))
        continue;
      auto [it, inserted] = families.try_emplace(code.getOperation());
      auto &family = it->second;
      if (inserted) {
        family.code = code;
        family.module = mlir::parseSourceString<mlir::ModuleOp>(
            code.getBytecode(), &getContext());
        ++decodedKernels;
        if (!family.module) {
          signalPassFailure();
          return;
        }
      }
      family.calls.push_back(std::move(plan));
    }
    analysisWork += analysis.getWork();
    if (analysis.exhausted())
      ++exhaustedAnalyses;
    uint64_t remainingBytes = maxExtraBytes;
    for (auto &[key, family] : families) {
      auto main = family.module->lookupSymbol<mlir::func::FuncOp>("main");
      if (!main) {
        signalPassFailure();
        return;
      }
      SmallVector<uint64_t> inputBytes;
      for (auto type : main.getArgumentTypes())
        inputBytes.push_back(tensorBytes(type));
      llvm::SmallDenseSet<uint64_t, 8> resultSizes;
      uint64_t outputBytes = 0;
      for (auto type : main.getResultTypes()) {
        uint64_t bytes = tensorBytes(type);
        resultSizes.insert(bytes);
        outputBytes = std::min(UINT64_MAX - bytes, outputBytes) + bytes;
      }
      std::map<std::pair<Indices, Indices>, unsigned> interned;
      auto addCandidate = [&](Indices arguments, Indices mapping,
                              Indices donors, uint64_t benefit) {
        if (donors.empty() || !benefit)
          return;
        auto key = std::make_pair(mapping, donors);
        auto found = interned.find(key);
        if (found != interned.end()) {
          auto &score = family.candidates[found->second].benefit;
          score = std::min(UINT64_MAX - benefit, score) + benefit;
          return;
        }
        if (family.candidates.size() >= maxCandidates)
          return;
        interned.emplace(std::move(key), family.candidates.size());
        family.candidates.push_back({std::move(arguments),
                                     std::move(mapping),
                                     std::move(donors),
                                     benefit,
                                     {}});
      };
      for (auto &plan : family.calls) {
        Indices donors;
        uint64_t benefit = 0;
        SmallVector<std::pair<uint64_t, int32_t>> viable;
        for (int32_t i : plan.arguments) {
          if (plan.decisions[i] == Decision::Forbidden)
            continue;
          auto bytes = inputBytes[i];
          if (!bytes || !resultSizes.contains(bytes))
            continue;
          viable.emplace_back(bytes, i);
        }
        llvm::stable_sort(viable,
                          [](auto a, auto b) { return a.first > b.first; });
        for (auto [bytes, i] : viable) {
          if (uint64_t(plan.arguments.size()) * plan.call.getDevices().size() <=
              maxGuardComparisons)
            addCandidate(plan.arguments, plan.mapping, {i}, bytes);
          if (uint64_t(plan.arguments.size()) * plan.call.getDevices().size() *
                  (donors.size() + 1) >
              maxGuardComparisons)
            continue;
          donors.push_back(i);
          benefit = std::min(UINT64_MAX - bytes, benefit) + bytes;
        }
        llvm::sort(donors);
        if (donors.size() > 1 && !viable.empty() &&
            std::min(benefit, outputBytes) > viable.front().first)
          addCandidate(plan.arguments, plan.mapping, donors,
                       std::min(benefit, outputBytes));
      }
      llvm::stable_sort(family.candidates, [](const Candidate &a,
                                              const Candidate &b) {
        if (a.benefit != b.benefit)
          return a.benefit > b.benefit;
        if (a.donors.size() != b.donors.size())
          return a.donors.size() < b.donors.size();
        return std::tie(a.mapping, a.donors) < std::tie(b.mapping, b.donors);
      });
      SmallVector<Candidate> selected;
      unsigned attempts = 0;
      for (auto &candidate : family.candidates) {
        if (++attempts >= maxVariants || !remainingBytes)
          break;
        auto copy = mlir::OwningOpRef<mlir::ModuleOp>(family.module->clone());
        auto entry = copy->lookupSymbol<mlir::func::FuncOp>("main");
        llvm::BitVector erased(entry.getNumArguments(), true);
        for (int32_t i : candidate.arguments)
          erased.reset(i);
        for (unsigned i = 0; i < entry.getNumArguments(); ++i) {
          entry.removeArgAttr(i, "jax.buffer_donor");
          if (!erased.test(i))
            continue;
          entry.getArgument(i).replaceAllUsesWith(
              entry.getArgument(candidate.mapping[i]));
        }
        for (int32_t i : candidate.donors)
          entry.setArgAttr(i, "jax.buffer_donor",
                           mlir::BoolAttr::get(&getContext(), true));
        if (mlir::failed(entry.eraseArguments(erased))) {
          signalPassFailure();
          return;
        }
        std::string bytes;
        llvm::raw_string_ostream stream(bytes);
        if (mlir::failed(mlir::writeBytecodeToFile(*copy, stream))) {
          signalPassFailure();
          return;
        }
        if (bytes.size() > remainingBytes)
          continue;
        auto name = mangledBlake3Symbol("REUSSIR_IFRT_BYTECODE", bytes);
        auto *scope = mlir::SymbolTable::getNearestSymbolTable(family.code);
        auto &table = symbols.getSymbolTable(scope);
        auto emitted = table.lookup<ReussirIFRTBytecodeOp>(name);
        if (!emitted) {
          llvm::BLAKE3 hash;
          hash.update(bytes);
          mlir::OpBuilder builder(family.code);
          emitted = ReussirIFRTBytecodeOp::create(
              builder, family.code.getLoc(), builder.getStringAttr(name),
              builder.getStringAttr("private"), builder.getStringAttr(bytes),
              builder.getStringAttr(llvm::toHex(hash.final(), true)));
          emitted->setAttr("kernel_type",
                           mlir::TypeAttr::get(entry.getFunctionType()));
          table.insert(emitted);
          remainingBytes -= bytes.size();
          ++emittedVariants;
          extraBytes += bytes.size();
        }
        candidate.symbol =
            mlir::FlatSymbolRefAttr::get(emitted.getSymNameAttr());
        selected.push_back(candidate);
      }
      for (auto &plan : family.calls) {
        mlir::OpBuilder builder(plan.call);
        SmallVector<mlir::Attribute> variants;
        llvm::SmallBitVector needed(plan.owners.size());
        uint64_t comparisons = 0;
        for (auto &candidate : selected) {
          if (candidate.mapping != plan.mapping ||
              llvm::any_of(candidate.donors, [&](int32_t i) {
                return plan.decisions[i] == Decision::Forbidden;
              }))
            continue;
          uint64_t cost = candidate.donors.size() *
                          (candidate.arguments.size() - 1) *
                          plan.call.getDevices().size();
          if (cost > maxGuardComparisons - comparisons)
            continue;
          comparisons += cost;
          for (int32_t i : candidate.donors)
            needed.set(i);
          variants.push_back(builder.getDictionaryAttr(
              {builder.getNamedAttr("callee", candidate.symbol),
               builder.getNamedAttr(
                   "donors", builder.getDenseI32ArrayAttr(candidate.donors)),
               builder.getNamedAttr("arguments", builder.getDenseI32ArrayAttr(
                                                     candidate.arguments))}));
        }
        if (variants.empty())
          continue;
        SmallVector<mlir::Value> guards;
        llvm::DenseMap<mlir::Value, mlir::Value> ownerGuards;
        for (unsigned i = 0; i < plan.owners.size(); ++i) {
          mlir::Value guard;
          if (needed.test(i)) {
            auto owner = plan.owners[i];
            auto found = ownerGuards.find(owner);
            if (found != ownerGuards.end())
              guard = found->second;
            else if (plan.decisions[i] == Decision::Unconditional) {
              guard = mlir::arith::ConstantIntOp::create(
                  builder, plan.call.getLoc(), 1, 1);
              ++staticDonors;
            } else {
              guard = ReussirRcIsUniqueOp::create(builder, plan.call.getLoc(),
                                                  builder.getI1Type(), owner);
              guard.getDefiningOp()->setAttr("pjrt.donation_guard",
                                             builder.getUnitAttr());
              ++guardedDonors;
            }
            ownerGuards[owner] = guard;
            auto view = ReussirArrayToIfrtOp::create(
                builder, plan.call.getLoc(), plan.call.getInputs()[i].getType(),
                owner, builder.getUnitAttr());
            plan.call.getInputsMutable().slice(i, 1).assign(view.getView());
          } else
            guard = mlir::arith::ConstantIntOp::create(
                builder, plan.call.getLoc(), 0, 1);
          guards.push_back(guard);
        }
        plan.call.getDonationGuardsMutable().assign(guards);
        plan.call.setDonationVariantsAttr(builder.getArrayAttr(variants));
      }
    }
    carryLoopGuards(module);
  }
};
} // namespace
} // namespace reussir
