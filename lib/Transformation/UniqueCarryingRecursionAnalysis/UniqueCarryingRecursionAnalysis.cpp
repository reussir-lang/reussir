//===-- UniqueCarryingRecursionAnalysis.cpp ---------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Transformation/Passes.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Location.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Pass/Pass.h>

#include <optional>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRUNIQUECARRYINGRECURSIONANALYSISPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

static constexpr llvm::StringLiteral kCarryingUniquenessAttr =
    "reussir.carrying_uniqueness";

enum class AggregateProjectionKind : uint8_t {
  Array,
  Record,
};

struct AggregateProjection {
  AggregateProjectionKind kind;
  unsigned index;
};

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

  bool isProvenUniqueUnder(const llvm::BitVector &assumedArgs) const {
    if (!isCarrying())
      return false;
    for (int bit = carriedArgs.find_first(); bit >= 0;
         bit = carriedArgs.find_next(bit)) {
      if (static_cast<unsigned>(bit) >= assumedArgs.size() ||
          !assumedArgs.test(bit))
        return false;
    }
    return true;
  }

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

static bool isAssumableRcType(mlir::Type type) {
  auto rcType = llvm::dyn_cast<RcType>(type);
  return rcType && !rcType.isRegional();
}

static bool hasRcResults(mlir::TypeRange types) {
  return llvm::any_of(types, isRcType);
}

using FunctionSummaryMap =
    llvm::StringMap<llvm::SmallVector<UniqueCarryingValue>>;
using SpecializationEntry = std::pair<std::string, llvm::BitVector>;
using SpecializationMap = llvm::SmallVector<SpecializationEntry>;
using SpecializationNameEntry = std::pair<std::string, std::string>;
using SpecializationNameMap = llvm::SmallVector<SpecializationNameEntry>;
using SpecializationCloneEntry = std::pair<std::string, mlir::func::FuncOp>;
using SpecializationCloneMap = llvm::SmallVector<SpecializationCloneEntry>;

template <typename ValueT>
static ValueT *findSpecializationValue(
    llvm::SmallVectorImpl<std::pair<std::string, ValueT>> &entries,
    llvm::StringRef key) {
  for (auto &entry : entries)
    if (entry.first == key)
      return &entry.second;
  return nullptr;
}

template <typename ValueT>
static const ValueT *findSpecializationValue(
    const llvm::SmallVectorImpl<std::pair<std::string, ValueT>> &entries,
    llvm::StringRef key) {
  for (const auto &entry : entries)
    if (entry.first == key)
      return &entry.second;
  return nullptr;
}

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
  static std::optional<unsigned> getConstantProjectIndex(mlir::Value index) {
    auto constantOp =
        llvm::dyn_cast_if_present<mlir::arith::ConstantOp>(index.getDefiningOp());
    if (!constantOp)
      return std::nullopt;

    auto integerAttr = llvm::dyn_cast<mlir::IntegerAttr>(constantOp.getValue());
    if (!integerAttr)
      return std::nullopt;
    return static_cast<unsigned>(integerAttr.getUInt());
  }

  UniqueCarryingValue
  evaluateAggregateProjectionPath(mlir::Value aggregate,
                                  llvm::ArrayRef<AggregateProjection> path) {
    if (path.empty())
      return evaluate(aggregate);

    AggregateProjection projection = path.front();
    if (auto create =
            llvm::dyn_cast_if_present<ReussirArrayCreateOp>(
                aggregate.getDefiningOp())) {
      if (projection.kind != AggregateProjectionKind::Array ||
          projection.index >= create.getElements().size())
        return UniqueCarryingValue::getUnknown();
      return evaluateAggregateProjectionPath(create.getElements()[projection.index],
                                            path.drop_front());
    }

    if (auto insert =
            llvm::dyn_cast_if_present<ReussirArrayInsertOp>(
                aggregate.getDefiningOp())) {
      if (projection.kind != AggregateProjectionKind::Array)
        return UniqueCarryingValue::getUnknown();
      if (insert.getIndex().getZExtValue() == projection.index)
        return evaluateAggregateProjectionPath(insert.getValue(),
                                              path.drop_front());
      return evaluateAggregateProjectionPath(insert.getArray(), path);
    }

    if (auto create =
            llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(
                aggregate.getDefiningOp())) {
      if (projection.kind != AggregateProjectionKind::Record ||
          projection.index >= create.getFields().size())
        return UniqueCarryingValue::getUnknown();
      return evaluateAggregateProjectionPath(create.getFields()[projection.index],
                                            path.drop_front());
    }

    return UniqueCarryingValue::getUnknown();
  }

  UniqueCarryingValue evaluateLoadedReference(mlir::Value ref) {
    llvm::SmallVector<AggregateProjection> reversedPath;
    bool hasDynamicArrayProjection = false;

    while (true) {
      if (auto arrayProject =
              llvm::dyn_cast_if_present<ReussirArrayProjectOp>(
                  ref.getDefiningOp())) {
        if (!hasDynamicArrayProjection) {
          std::optional<unsigned> index =
              getConstantProjectIndex(arrayProject.getIndex());
          if (!index)
            hasDynamicArrayProjection = true;
          else
            reversedPath.push_back({AggregateProjectionKind::Array, *index});
        }
        ref = arrayProject.getArrayRef();
        continue;
      }

      if (auto recordProject =
              llvm::dyn_cast_if_present<ReussirRefProjectOp>(
                  ref.getDefiningOp())) {
        if (!hasDynamicArrayProjection) {
          reversedPath.push_back(
              {AggregateProjectionKind::Record,
               static_cast<unsigned>(recordProject.getIndex().getZExtValue())});
        }
        ref = recordProject.getRef();
        continue;
      }

      break;
    }

    if (auto borrow =
            llvm::dyn_cast_if_present<ReussirRcBorrowOp>(ref.getDefiningOp()))
      return evaluate(borrow.getRcPtr());

    if (hasDynamicArrayProjection)
      return UniqueCarryingValue::getUnknown();

    if (auto spilled =
            llvm::dyn_cast_if_present<ReussirRefSpilledOp>(ref.getDefiningOp())) {
      llvm::SmallVector<AggregateProjection> path(reversedPath.rbegin(),
                                                  reversedPath.rend());
      return evaluateAggregateProjectionPath(spilled.getValue(), path);
    }

    return UniqueCarryingValue::getUnknown();
  }

  UniqueCarryingValue evaluateAggregateElement(mlir::Value aggregate,
                                               AggregateProjectionKind kind,
                                               unsigned index) {
    AggregateProjection projection{kind, index};
    return evaluateAggregateProjectionPath(aggregate, {projection});
  }

  UniqueCarryingValue evaluateResult(mlir::Operation *op,
                                     unsigned resultIndex) {
    if (llvm::isa<ReussirRcCreateOp, ReussirRcCreateCompoundOp,
                  ReussirRcCreateVariantOp>(op))
      return UniqueCarryingValue::getFresh();

    if (auto loadOp = llvm::dyn_cast<ReussirRefLoadOp>(op))
      return evaluateLoadedReference(loadOp.getRef());

    if (auto extractOp = llvm::dyn_cast<ReussirArrayExtractOp>(op))
      return evaluateAggregateElement(extractOp.getArray(),
                                      AggregateProjectionKind::Array,
                                      extractOp.getIndex().getZExtValue());

    if (auto extractOp = llvm::dyn_cast<ReussirRecordExtractOp>(op))
      return evaluateAggregateElement(extractOp.getRecord(),
                                      AggregateProjectionKind::Record,
                                      extractOp.getIndex().getZExtValue());

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

static bool isUniqueCloneName(llvm::StringRef name) {
  return name.contains(".unique");
}

static bool isUniqueCloneOf(llvm::StringRef baseName, llvm::StringRef callee) {
  if (callee == baseName)
    return true;
  return callee.starts_with((baseName + ".unique").str());
}

static void resetDirectSelfCallsToBase(mlir::func::FuncOp funcOp,
                                       llvm::StringRef baseName) {
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (isUniqueCloneOf(baseName, callOp.getCallee()))
      callOp.setCallee(baseName);
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
    if (!isAssumableRcType(funcOp.getArgument(i).getType()))
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
    if (!isAssumableRcType(arg.getType()))
      continue;
    builder.create<ReussirRcAssumeUniqueOp>(funcOp.getLoc(), arg);
  }
}

static std::string encodeSpecializationKey(const llvm::BitVector &carriedArgs) {
  std::string key;
  llvm::raw_string_ostream os(key);
  bool first = true;
  for (int bit = carriedArgs.find_first(); bit >= 0;
       bit = carriedArgs.find_next(bit)) {
    if (!first)
      os << '_';
    os << bit;
    first = false;
  }
  return key;
}

static llvm::BitVector computeUniqueCallArguments(
    mlir::func::CallOp callOp, ProvenanceEvaluator &evaluator,
    const llvm::BitVector &assumedArgs, const llvm::BitVector &candidateArgs) {
  llvm::BitVector provenArgs(candidateArgs.size());
  for (int argIndex = candidateArgs.find_first(); argIndex >= 0;
       argIndex = candidateArgs.find_next(argIndex)) {
    if (static_cast<unsigned>(argIndex) >= callOp.getNumOperands())
      continue;
    UniqueCarryingValue value = evaluator.evaluate(callOp.getOperand(argIndex));
    if (value.isCarrying() || value.isProvenUniqueUnder(assumedArgs))
      provenArgs.set(argIndex);
  }
  return provenArgs;
}

static SpecializationMap
collectUniqueSpecializations(mlir::func::FuncOp funcOp,
                             const FunctionSummaryMap &summaries,
                             const llvm::BitVector &candidateArgs) {
  SpecializationMap specializations;
  llvm::SmallVector<llvm::BitVector> worklist;

  auto enqueue = [&](const llvm::BitVector &assumedArgs) {
    if (!assumedArgs.any())
      return;
    std::string key = encodeSpecializationKey(assumedArgs);
    if (findSpecializationValue(specializations, key))
      return;
    specializations.push_back({std::move(key), assumedArgs});
    worklist.push_back(assumedArgs);
  };

  auto collectFromAssumedArgs = [&](const llvm::BitVector &assumedArgs) {
    ProvenanceEvaluator evaluator(summaries);
    funcOp.walk([&](mlir::func::CallOp callOp) {
      if (callOp.getCallee() != funcOp.getName())
        return;
      enqueue(computeUniqueCallArguments(callOp, evaluator, assumedArgs,
                                         candidateArgs));
    });
  };

  llvm::BitVector emptyAssumptions(candidateArgs.size());
  collectFromAssumedArgs(emptyAssumptions);
  while (!worklist.empty())
    collectFromAssumedArgs(worklist.pop_back_val());
  return specializations;
}

static SpecializationNameMap
buildSpecializationNames(llvm::StringRef baseName,
                         const SpecializationMap &specializations) {
  SpecializationNameMap names;
  bool usePlainUniqueName = specializations.size() == 1;
  for (const auto &entry : specializations) {
    std::string cloneName = usePlainUniqueName
                                ? (baseName + ".unique").str()
                                : (baseName + ".unique_" + entry.first).str();
    names.push_back({entry.first, std::move(cloneName)});
  }
  return names;
}

static mlir::func::FuncOp getOrCreateUniqueClone(
    mlir::func::FuncOp funcOp, mlir::SymbolTable &symbolTable,
    llvm::StringRef uniqueName, const llvm::BitVector &carriedArgs) {
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
  return uniqueFunc;
}

static void rewriteSelfCallsForSpecialization(
    mlir::func::FuncOp funcOp, llvm::StringRef baseName,
    const FunctionSummaryMap &summaries, const llvm::BitVector &assumedArgs,
    const llvm::BitVector &candidateArgs,
    const SpecializationNameMap &specializationNames) {
  ProvenanceEvaluator evaluator(summaries);
  funcOp.walk([&](mlir::func::CallOp callOp) {
    if (callOp.getCallee() != baseName)
      return;
    llvm::BitVector targetArgs = computeUniqueCallArguments(
        callOp, evaluator, assumedArgs, candidateArgs);
    if (!targetArgs.any())
      return;
    const std::string *targetName = findSpecializationValue(
        specializationNames, encodeSpecializationKey(targetArgs));
    if (!targetName)
      return;
    callOp.setCallee(*targetName);
  });
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
          isUniqueCloneName(funcOp.getName()) || !hasSelfRecursiveCall(funcOp))
        return;
      if (!collectCarriedRcArguments(funcOp, summaries).any())
        return;
      recursiveFunctions.push_back(funcOp);
    });

    for (mlir::func::FuncOp funcOp : recursiveFunctions) {
      llvm::BitVector carriedArgs =
          collectCarriedRcArguments(funcOp, summaries);
      SpecializationMap specializations =
          collectUniqueSpecializations(funcOp, summaries, carriedArgs);
      if (specializations.empty())
        continue;

      SpecializationNameMap specializationNames =
          buildSpecializationNames(funcOp.getName(), specializations);
      SpecializationCloneMap clones;

      resetDirectSelfCallsToBase(funcOp, funcOp.getName());
      for (const auto &entry : specializations) {
        const std::string *cloneName =
            findSpecializationValue(specializationNames, entry.first);
        if (!cloneName)
          continue;
        clones.push_back(
            {entry.first, getOrCreateUniqueClone(funcOp, symbolTable,
                                                 *cloneName, entry.second)});
      }

      llvm::BitVector emptyAssumptions(funcOp.getNumArguments());
      rewriteSelfCallsForSpecialization(funcOp, funcOp.getName(), summaries,
                                        emptyAssumptions, carriedArgs,
                                        specializationNames);
      for (const auto &entry : specializations) {
        mlir::func::FuncOp *cloneFunc =
            findSpecializationValue(clones, entry.first);
        if (!cloneFunc)
          continue;
        resetDirectSelfCallsToBase(*cloneFunc, funcOp.getName());
        rewriteSelfCallsForSpecialization(*cloneFunc, funcOp.getName(),
                                          summaries, entry.second, carriedArgs,
                                          specializationNames);
      }
    }
  }
};

} // namespace
} // namespace reussir
