//===-- UniqueCarryingRecursionAnalysis.cpp ---------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Location.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRUNIQUECARRYINGRECURSIONANALYSISPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {

static constexpr llvm::StringLiteral kCarryingUniquenessAttr =
    "reussir.carrying_uniqueness";

struct UniqueCarryingValue {
  bool unknown = true;
  llvm::BitVector carriedArgs;
  bool fresh = false;

  static UniqueCarryingValue getUnknown() { return {}; }

  static UniqueCarryingValue getArgument(unsigned index) {
    UniqueCarryingValue value;
    value.unknown = false;
    value.carriedArgs.resize(index + 1);
    value.carriedArgs.set(index);
    return value;
  }

  static UniqueCarryingValue getFresh() {
    UniqueCarryingValue value;
    value.unknown = false;
    value.fresh = true;
    return value;
  }

  bool operator==(const UniqueCarryingValue &rhs) const {
    return unknown == rhs.unknown && fresh == rhs.fresh &&
           carriedArgs == rhs.carriedArgs;
  }

  static UniqueCarryingValue join(const UniqueCarryingValue &lhs,
                                  const UniqueCarryingValue &rhs) {
    if (lhs.unknown)
      return rhs;
    if (rhs.unknown)
      return lhs;

    UniqueCarryingValue joined;
    joined.unknown = false;
    joined.fresh = lhs.fresh || rhs.fresh;
    joined.carriedArgs.resize(
        std::max(lhs.carriedArgs.size(), rhs.carriedArgs.size()));
    joined.carriedArgs |= lhs.carriedArgs;
    joined.carriedArgs |= rhs.carriedArgs;
    return joined;
  }

  bool isCarrying() const { return !unknown && (fresh || carriedArgs.any()); }

  void print(llvm::raw_ostream &os) const {
    if (unknown) {
      os << "Unknown";
      return;
    }
    os << "{fresh=" << (fresh ? "true" : "false") << ", args=[";
    bool first = true;
    for (int bit = carriedArgs.find_first(); bit >= 0;
         bit = carriedArgs.find_next(bit)) {
      if (!first)
        os << ", ";
      os << bit;
      first = false;
    }
    os << "]}";
  }
};

static bool isRcType(mlir::Type type) { return llvm::isa<RcType>(type); }

static bool hasRcResults(mlir::TypeRange types) {
  return llvm::any_of(types, isRcType);
}

using FunctionSummaryMap =
    llvm::StringMap<llvm::SmallVector<UniqueCarryingValue>>;

class ProvenanceEvaluator {
public:
  explicit ProvenanceEvaluator(const FunctionSummaryMap &summaries)
      : summaries(summaries) {}

  UniqueCarryingValue evaluate(mlir::Value value) {
    if (!isRcType(value.getType()))
      return UniqueCarryingValue::getUnknown();
    if (auto it = cache.find(value); it != cache.end())
      return it->second;
    if (!active.insert(value).second)
      return UniqueCarryingValue::getUnknown();

    UniqueCarryingValue result = UniqueCarryingValue::getUnknown();
    if (auto blockArg = llvm::dyn_cast<mlir::BlockArgument>(value)) {
      auto *owner = blockArg.getOwner();
      if (owner && owner->isEntryBlock()) {
        if (llvm::isa<mlir::func::FuncOp>(owner->getParentOp()))
          result = UniqueCarryingValue::getArgument(blockArg.getArgNumber());
      }
    } else if (mlir::Operation *def = value.getDefiningOp()) {
      result = evaluateResult(
          def, llvm::cast<mlir::OpResult>(value).getResultNumber());
    }

    active.erase(value);
    cache[value] = result;
    return result;
  }

private:
  UniqueCarryingValue evaluateResult(mlir::Operation *op,
                                     unsigned resultIndex) {
    if (llvm::isa<ReussirRcCreateOp, ReussirRcCreateCompoundOp,
                  ReussirRcCreateVariantOp>(op))
      return UniqueCarryingValue::getFresh();

    if (auto callOp = llvm::dyn_cast<mlir::func::CallOp>(op)) {
      auto it = summaries.find(callOp.getCallee());
      if (it == summaries.end() || resultIndex >= it->second.size())
        return UniqueCarryingValue::getUnknown();
      const UniqueCarryingValue &summary = it->second[resultIndex];
      if (summary.unknown)
        return summary;
      UniqueCarryingValue mapped = UniqueCarryingValue::getUnknown();
      mapped.unknown = false;
      mapped.fresh = summary.fresh;
      for (int argIndex = summary.carriedArgs.find_first(); argIndex >= 0;
           argIndex = summary.carriedArgs.find_next(argIndex)) {
        if (static_cast<unsigned>(argIndex) >= callOp.getNumOperands())
          continue;
        mapped = UniqueCarryingValue::join(
            mapped, evaluate(callOp.getOperand(argIndex)));
      }
      return mapped;
    }

    auto *dialect = op->getDialect();
    if (dialect && dialect->getNamespace() == "scf") {
      UniqueCarryingValue joined = UniqueCarryingValue::getUnknown();
      for (mlir::Region &region : op->getRegions()) {
        if (region.empty())
          continue;
        auto yieldOp =
            llvm::dyn_cast<mlir::scf::YieldOp>(region.front().getTerminator());
        if (!yieldOp || resultIndex >= yieldOp.getNumOperands())
          continue;
        joined = UniqueCarryingValue::join(
            joined, evaluate(yieldOp.getOperand(resultIndex)));
      }
      return joined;
    }

    return UniqueCarryingValue::getUnknown();
  }

  const FunctionSummaryMap &summaries;
  llvm::DenseMap<mlir::Value, UniqueCarryingValue> cache;
  llvm::SmallDenseSet<mlir::Value, 16> active;
};

static void setCarryingUniquenessAttr(mlir::Operation *op, bool enabled) {
  if (enabled)
    op->setAttr(kCarryingUniquenessAttr, mlir::UnitAttr::get(op->getContext()));
  else
    op->removeAttr(kCarryingUniquenessAttr);
}

static bool areAllRcValuesCarrying(mlir::ValueRange values,
                                   ProvenanceEvaluator &evaluator) {
  bool foundRcValue = false;
  for (mlir::Value value : values) {
    if (!isRcType(value.getType()))
      continue;
    foundRcValue = true;
    if (!evaluator.evaluate(value).isCarrying())
      return false;
  }
  return foundRcValue;
}

static bool areEqualSummaries(const FunctionSummaryMap &lhs,
                              const FunctionSummaryMap &rhs) {
  if (lhs.size() != rhs.size())
    return false;
  for (const auto &entry : lhs) {
    auto rhsIt = rhs.find(entry.getKey());
    if (rhsIt == rhs.end() || rhsIt->second != entry.getValue())
      return false;
  }
  return true;
}

static llvm::SmallVector<UniqueCarryingValue>
computeFunctionSummary(mlir::func::FuncOp funcOp,
                       const FunctionSummaryMap &summaries) {
  ProvenanceEvaluator evaluator(summaries);
  llvm::SmallVector<UniqueCarryingValue> summary(
      funcOp.getNumResults(), UniqueCarryingValue::getUnknown());
  funcOp.walk([&](mlir::func::ReturnOp returnOp) {
    for (auto [index, operand] : llvm::enumerate(returnOp.getOperands())) {
      if (!isRcType(operand.getType()))
        continue;
      summary[index] = UniqueCarryingValue::join(summary[index],
                                                 evaluator.evaluate(operand));
    }
  });
  return summary;
}

static bool isCarryingUniquenessScfOp(mlir::Operation *op,
                                      const FunctionSummaryMap &summaries) {
  auto *dialect = op->getDialect();
  if (!dialect || dialect->getNamespace() != "scf" ||
      !hasRcResults(op->getResultTypes()))
    return false;
  ProvenanceEvaluator evaluator(summaries);
  return areAllRcValuesCarrying(op->getResults(), evaluator);
}

static bool isCarryingUniquenessFunction(mlir::func::FuncOp funcOp,
                                         const FunctionSummaryMap &summaries) {
  auto it = summaries.find(funcOp.getName());
  if (it == summaries.end() || funcOp.isDeclaration() ||
      !hasRcResults(funcOp.getResultTypes()))
    return false;
  for (auto [index, type] : llvm::enumerate(funcOp.getResultTypes())) {
    if (!isRcType(type))
      continue;
    if (index >= it->second.size() || !it->second[index].isCarrying())
      return false;
  }
  return true;
}

static bool hasSelfRecursiveCall(mlir::func::FuncOp funcOp) {
  bool found = false;
  llvm::StringRef name = funcOp.getName();
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() == name)
      found = true;
  });
  return found;
}

static void redirectDirectSelfCalls(mlir::func::FuncOp funcOp,
                                    llvm::StringRef fromName,
                                    llvm::StringRef toName) {
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() == fromName)
      callOp.setCallee(toName);
  });
}

static mlir::LocationAttr stripDebugMetadata(mlir::LocationAttr loc) {
  if (!loc)
    return loc;
  auto *context = loc.getContext();

  if (auto fusedLoc = llvm::dyn_cast<mlir::FusedLoc>(loc)) {
    llvm::SmallVector<mlir::Location> strippedLocations;
    strippedLocations.reserve(fusedLoc.getLocations().size());
    for (mlir::Location innerLoc : fusedLoc.getLocations())
      strippedLocations.push_back(stripDebugMetadata(innerLoc));
    return mlir::FusedLoc::get(context, strippedLocations);
  }

  if (auto nameLoc = llvm::dyn_cast<mlir::NameLoc>(loc))
    return mlir::NameLoc::get(nameLoc.getName(),
                              stripDebugMetadata(nameLoc.getChildLoc()));

  if (auto callSiteLoc = llvm::dyn_cast<mlir::CallSiteLoc>(loc))
    return mlir::CallSiteLoc::get(stripDebugMetadata(callSiteLoc.getCallee()),
                                  stripDebugMetadata(callSiteLoc.getCaller()));

  return loc;
}

static void stripCloneDebugInfo(mlir::func::FuncOp funcOp) {
  funcOp->removeAttr("reussir.dbg_func_args");
  funcOp->setLoc(stripDebugMetadata(funcOp.getLoc()));
  funcOp.walk([&](mlir::Operation *op) {
    if (op == funcOp)
      return;
    op->setLoc(stripDebugMetadata(op->getLoc()));
  });
}

static llvm::BitVector
collectCarriedRcArguments(mlir::func::FuncOp funcOp,
                          const FunctionSummaryMap &summaries) {
  llvm::BitVector carriedArgs(funcOp.getNumArguments());
  auto it = summaries.find(funcOp.getName());
  if (it == summaries.end())
    return carriedArgs;
  for (const UniqueCarryingValue &value : it->second) {
    if (value.unknown)
      continue;
    carriedArgs.resize(std::max(carriedArgs.size(), value.carriedArgs.size()));
    carriedArgs |= value.carriedArgs;
  }
  for (unsigned i = 0; i < funcOp.getNumArguments(); ++i)
    if (!isRcType(funcOp.getArgument(i).getType()))
      carriedArgs.reset(i);
  return carriedArgs;
}

static void insertAssumeUniqueAtEntry(mlir::func::FuncOp funcOp,
                                      const llvm::BitVector &carriedArgs) {
  if (funcOp.empty())
    return;

  mlir::OpBuilder builder(funcOp.getContext());
  builder.setInsertionPointToStart(&funcOp.getBody().front());
  for (int index = carriedArgs.find_first(); index >= 0;
       index = carriedArgs.find_next(index)) {
    if (static_cast<unsigned>(index) >= funcOp.getNumArguments())
      continue;
    mlir::BlockArgument arg = funcOp.getArgument(index);
    auto rcType = llvm::dyn_cast<RcType>(arg.getType());
    if (!rcType || rcType.isRegional())
      continue;
    builder.create<ReussirRcAssumeUniqueOp>(funcOp.getLoc(), arg);
  }
}

static mlir::func::FuncOp
getOrCreateUniqueClone(mlir::func::FuncOp funcOp,
                       mlir::SymbolTable &symbolTable,
                       const llvm::BitVector &carriedArgs) {
  std::string uniqueName = (funcOp.getName() + ".unique").str();
  if (auto existing = symbolTable.lookup<mlir::func::FuncOp>(uniqueName))
    return existing;

  auto uniqueFunc = llvm::cast<mlir::func::FuncOp>(funcOp->clone());
  uniqueFunc.setName(uniqueName);
  uniqueFunc.setPrivate();
  uniqueFunc->removeAttr("llvm.visibility");
  uniqueFunc->setAttr("llvm.linkage",
                      mlir::LLVM::LinkageAttr::get(
                          funcOp.getContext(), mlir::LLVM::Linkage::Internal));
  stripCloneDebugInfo(uniqueFunc);
  symbolTable.insert(uniqueFunc);

  insertAssumeUniqueAtEntry(uniqueFunc, carriedArgs);
  redirectDirectSelfCalls(uniqueFunc, funcOp.getName(), uniqueName);
  return uniqueFunc;
}

struct UniqueCarryingRecursionAnalysisPass
    : public impl::ReussirUniqueCarryingRecursionAnalysisPassBase<
          UniqueCarryingRecursionAnalysisPass> {
  using Base::Base;

  void runOnOperation() override {
    auto moduleOp = getOperation();

    moduleOp.walk([&](mlir::Operation *op) {
      if (llvm::isa<mlir::func::FuncOp>(op) ||
          (op->getDialect() && op->getDialect()->getNamespace() == "scf"))
        op->removeAttr(kCarryingUniquenessAttr);
    });

    FunctionSummaryMap summaries;

    while (true) {
      FunctionSummaryMap nextSummaries;
      moduleOp.walk([&](mlir::func::FuncOp funcOp) {
        nextSummaries[funcOp.getName()] =
            computeFunctionSummary(funcOp, summaries);
      });

      if (areEqualSummaries(summaries, nextSummaries)) {
        summaries = std::move(nextSummaries);
        break;
      }
      summaries = std::move(nextSummaries);
    }

    moduleOp.walk([&](mlir::func::FuncOp funcOp) {
      setCarryingUniquenessAttr(
          funcOp, isCarryingUniquenessFunction(funcOp, summaries));
    });
    moduleOp.walk([&](mlir::Operation *op) {
      if (llvm::isa<mlir::func::FuncOp>(op))
        return;
      setCarryingUniquenessAttr(op, isCarryingUniquenessScfOp(op, summaries));
    });

    mlir::SymbolTable symbolTable(moduleOp);
    llvm::SmallVector<mlir::func::FuncOp> recursiveFunctions;
    moduleOp.walk([&](mlir::func::FuncOp funcOp) {
      if (!funcOp->hasAttr(kCarryingUniquenessAttr) || funcOp.isDeclaration() ||
          funcOp.getName().ends_with(".unique") ||
          !hasSelfRecursiveCall(funcOp))
        return;
      if (!collectCarriedRcArguments(funcOp, summaries).any())
        return;
      recursiveFunctions.push_back(funcOp);
    });

    for (mlir::func::FuncOp funcOp : recursiveFunctions) {
      llvm::BitVector carriedArgs =
          collectCarriedRcArguments(funcOp, summaries);
      auto uniqueFunc =
          getOrCreateUniqueClone(funcOp, symbolTable, carriedArgs);
      redirectDirectSelfCalls(funcOp, funcOp.getName(), uniqueFunc.getName());
    }
  }
};

} // namespace
} // namespace reussir
