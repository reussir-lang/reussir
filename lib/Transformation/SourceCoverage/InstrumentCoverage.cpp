//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements `reussir-instrument-coverage`. The pass adds
/// `llvm.instrprof.increment` counters to functions. It writes the source
/// span of each counter into the `reussir.coverage.map` attribute.
///
//===----------------------------------------------------------------------===//

#include "Reussir/IR/Coverage.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Transformation/Passes.h"

#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/ProfileData/InstrProf.h>
#include <llvm/Support/EndianStream.h>
#include <llvm/Support/xxhash.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRINSTRUMENTCOVERAGEPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

constexpr llvm::StringLiteral coverageMapAttr = "reussir.coverage.map";

/// Returns the smallest span that contains `lhs` and `rhs`.
mlir::FileLineColRange unite(mlir::FileLineColRange lhs,
                             mlir::FileLineColRange rhs) {
  auto start = std::min(std::pair(lhs.getStartLine(), lhs.getStartColumn()),
                        std::pair(rhs.getStartLine(), rhs.getStartColumn()));
  auto end = std::max(std::pair(lhs.getEndLine(), lhs.getEndColumn()),
                      std::pair(rhs.getEndLine(), rhs.getEndColumn()));
  return mlir::FileLineColRange::get(lhs.getFilename(), start.first,
                                     start.second, end.first, end.second);
}

/// Returns the union of the spans of the ops in `region` that are in `file`.
/// Returns a null range when no such op has a span.
mlir::FileLineColRange regionSpan(mlir::Region &region, mlir::StringAttr file) {
  mlir::FileLineColRange result;
  region.walk([&](mlir::Operation *op) {
    auto span = op->getLoc()->findInstanceOf<mlir::FileLineColRange>();
    if (!span || span.getFilename() != file)
      return;
    result = result ? unite(result, span) : span;
  });
  return result;
}

/// Builds the increment of counter `index` of the function that `name`
/// names.
void buildIncrement(mlir::OpBuilder &builder, mlir::Location loc,
                    mlir::LLVM::GlobalOp name, uint64_t hash,
                    uint32_t numCounters, uint32_t index) {
  mlir::Value operands[] = {
      mlir::LLVM::AddressOfOp::create(builder, loc, name),
      mlir::LLVM::ConstantOp::create(builder, loc,
                                     builder.getI64IntegerAttr(hash)),
      mlir::LLVM::ConstantOp::create(builder, loc,
                                     builder.getI32IntegerAttr(numCounters)),
      mlir::LLVM::ConstantOp::create(builder, loc,
                                     builder.getI32IntegerAttr(index))};
  mlir::LLVM::CallIntrinsicOp::create(
      builder, loc,
      builder.getStringAttr(
          llvm::Intrinsic::getBaseName(llvm::Intrinsic::instrprof_increment)),
      operands);
}

/// A source region of a function and the counters that count it. A code
/// region has the counter `counter`.
struct MappingRegion {
  mlir::FileLineColRange span;
  llvm::SmallVector<std::pair<llvm::StringRef, uint32_t>, 2> counters;
};

/// The counters and the source regions of one function. Counter 0 counts
/// the function entry. `emit` puts the increments into the function.
class CoverageMap {
public:
  CoverageMap(mlir::func::FuncOp function, mlir::FileLineColRange span)
      : function(function), file(span.getFilename()) {
    addCodeRegion(addCounter(function.getBody().front(), span), span);
  }

  /// Adds a counter at the start of `block`, and returns its index. The
  /// increment gets the location `loc`.
  uint32_t addCounter(mlir::Block &block, mlir::Location loc) {
    counters.push_back({&block, loc});
    return counters.size() - 1;
  }

  /// Adds a code region that counter `counter` counts.
  void addCodeRegion(uint32_t counter, mlir::FileLineColRange span) {
    regions.push_back({span, {{"counter", counter}}});
  }

  /// Adds a branch region for the condition at `span`. `trueCounter` counts
  /// the true results, and `falseCounter` counts the false results.
  void addBranchRegion(uint32_t trueCounter, uint32_t falseCounter,
                       mlir::FileLineColRange span) {
    regions.push_back(
        {span,
         {{"true_counter", trueCounter}, {"false_counter", falseCounter}}});
  }

  /// Returns the source file of the function.
  mlir::StringAttr getFile() const { return file; }

  /// Adds a counter at the start of the entry block of `region`. Its code
  /// region is the span of the ops in `region`. Returns the counter, or
  /// nothing when no op in `region` has a span in the file of the function.
  std::optional<uint32_t> addRegion(mlir::Region &region) {
    mlir::FileLineColRange span = regionSpan(region, file);
    if (!span)
      return std::nullopt;
    uint32_t counter = addCounter(region.front(), span);
    addCodeRegion(counter, span);
    return counter;
  }

  /// Puts the increments into the function and writes the
  /// `reussir.coverage.map` attribute. The global `name` holds `pgoName`.
  void emit(mlir::LLVM::GlobalOp name, llvm::StringRef pgoName) {
    mlir::OpBuilder builder(function.getContext());
    uint64_t functionHash = hash();
    for (auto [index, counter] : llvm::enumerate(counters)) {
      builder.setInsertionPointToStart(counter.block);
      buildIncrement(builder, counter.loc, name, functionHash, counters.size(),
                     index);
    }
    function->setAttr(
        coverageMapAttr,
        builder.getDictionaryAttr(
            {builder.getNamedAttr("hash",
                                  builder.getI64IntegerAttr(functionHash)),
             builder.getNamedAttr("name", builder.getStringAttr(pgoName)),
             builder.getNamedAttr("counters",
                                  builder.getI32IntegerAttr(counters.size())),
             builder.getNamedAttr("regions", regionsAttr(builder))}));
  }

private:
  struct Counter {
    mlir::Block *block;
    mlir::Location loc;
  };

  /// Returns a nonzero `xxh3_64bits` of the number of counters and of each
  /// region.
  uint64_t hash() {
    llvm::SmallString<256> bytes;
    llvm::raw_svector_ostream os(bytes);
    llvm::support::endian::Writer writer(os, llvm::endianness::little);
    auto writeString = [&](llvm::StringRef string) {
      writer.write<uint32_t>(string.size());
      os << string;
    };
    writer.write<uint32_t>(counters.size());
    for (auto &region : regions) {
      writer.write<uint32_t>(region.counters.size());
      for (auto [name, counter] : region.counters) {
        writeString(name);
        writer.write<uint32_t>(counter);
      }
      writeString(region.span.getFilename());
      writer.write<uint32_t>(region.span.getStartLine());
      writer.write<uint32_t>(region.span.getStartColumn());
      writer.write<uint32_t>(region.span.getEndLine());
      writer.write<uint32_t>(region.span.getEndColumn());
    }
    // llvm-cov reads a zero hash as the record of a function with no code.
    return std::max<uint64_t>(llvm::xxh3_64bits(bytes.str()), 1);
  }

  mlir::ArrayAttr regionsAttr(mlir::Builder &builder) {
    auto regionAttr = [&](MappingRegion &region) -> mlir::Attribute {
      llvm::SmallVector<mlir::NamedAttribute> entries;
      for (auto [name, counter] : region.counters)
        entries.push_back(
            builder.getNamedAttr(name, builder.getI32IntegerAttr(counter)));
      entries.push_back(builder.getNamedAttr("span", region.span));
      return builder.getDictionaryAttr(entries);
    };
    return builder.getArrayAttr(llvm::map_to_vector(regions, regionAttr));
  }

  mlir::func::FuncOp function;
  mlir::StringAttr file;
  llvm::SmallVector<Counter> counters;
  llvm::SmallVector<MappingRegion> regions;
};

/// Gives a counter to each region of `op` that can run a different number of
/// times than `op`. A region that runs exactly once each time `op` runs has
/// the count of the parent region.
void instrumentRegionBranch(CoverageMap &map,
                            mlir::RegionBranchOpInterface op) {
  // scf.execute_region runs its region exactly once, but it gives no bounds.
  if (llvm::isa<mlir::scf::ExecuteRegionOp>(op))
    return;
  llvm::SmallVector<mlir::InvocationBounds> bounds;
  op.getRegionInvocationBounds(
      llvm::SmallVector<mlir::Attribute>(op->getNumOperands()), bounds);
  for (auto [region, bound] : llvm::zip_equal(op->getRegions(), bounds))
    if (bound.getLowerBound() != 1 || bound.getUpperBound() != 1u)
      map.addRegion(region);
}

/// Gives a counter to the body of `op`. The body runs each time that the
/// program calls the closure, not when the program makes the closure.
void instrumentClosure(CoverageMap &map, ReussirClosureCreateOp op) {
  map.addRegion(op.getBody());
}

/// Returns true when `outer` contains `inner`.
bool contains(mlir::FileLineColRange outer, mlir::FileLineColRange inner) {
  return outer.getFilename() == inner.getFilename() &&
         std::pair(outer.getStartLine(), outer.getStartColumn()) <=
             std::pair(inner.getStartLine(), inner.getStartColumn()) &&
         std::pair(inner.getEndLine(), inner.getEndColumn()) <=
             std::pair(outer.getEndLine(), outer.getEndColumn());
}

/// Returns the span of the condition of `op` in `file`, or a null range. The
/// op that makes the condition must be in the span of `op`. If the condition
/// is in a variable, it has no span.
mlir::FileLineColRange conditionSpan(mlir::scf::IfOp op,
                                     mlir::StringAttr file) {
  auto span = op.getLoc()->findInstanceOf<mlir::FileLineColRange>();
  mlir::Operation *condition = op.getCondition().getDefiningOp();
  if (!span || span.getFilename() != file || !condition ||
      condition->getLoc() == op.getLoc())
    return {};
  auto result = condition->getLoc()->findInstanceOf<mlir::FileLineColRange>();
  if (!result || !contains(span, result))
    return {};
  return result;
}

/// Returns the span of the ops in `branch` of `op` that are in `file`. An op
/// with the location of `op` is a part of `op`, such as the `false` of
/// `a && b` or an empty `else`. Such an op is not in the span.
mlir::FileLineColRange branchSpan(mlir::scf::IfOp op, mlir::Region &branch,
                                  mlir::StringAttr file) {
  mlir::FileLineColRange result;
  branch.walk([&](mlir::Operation *inner) {
    auto span = inner->getLoc()->findInstanceOf<mlir::FileLineColRange>();
    if (!span || span.getFilename() != file || inner->getLoc() == op.getLoc())
      return;
    result = result ? unite(result, span) : span;
  });
  return result;
}

/// Gives a counter to each branch of `op`. When the condition has a span, a
/// branch region counts it. Then each branch gets a counter, also a branch
/// with no span. A branch with no span gets no code region.
void instrumentIf(CoverageMap &map, mlir::scf::IfOp op) {
  mlir::FileLineColRange condition = conditionSpan(op, map.getFile());
  if (!condition || op.getElseRegion().empty()) {
    instrumentRegionBranch(map, op);
    return;
  }
  auto addBranch = [&](mlir::Region &branch) {
    mlir::FileLineColRange span = branchSpan(op, branch, map.getFile());
    uint32_t counter = map.addCounter(
        branch.front(), span ? mlir::Location(span) : op.getLoc());
    if (span)
      map.addCodeRegion(counter, span);
    return counter;
  };
  uint32_t trueCounter = addBranch(op.getThenRegion());
  uint32_t falseCounter = addBranch(op.getElseRegion());
  map.addBranchRegion(trueCounter, falseCounter, condition);
}

/// Adds the counters of `op` to `map`. Add a case here for an op that needs
/// different counters. A case returns `skip` to keep the walk out of the
/// regions of the op.
mlir::WalkResult instrumentOp(CoverageMap &map, mlir::Operation *op) {
  return llvm::TypeSwitch<mlir::Operation *, mlir::WalkResult>(op)
      .Case<mlir::scf::IfOp>([&](auto ifOp) {
        instrumentIf(map, ifOp);
        return mlir::WalkResult::advance();
      })
      .Case<mlir::RegionBranchOpInterface>([&](auto branch) {
        instrumentRegionBranch(map, branch);
        return mlir::WalkResult::advance();
      })
      .Case<ReussirClosureCreateOp>([&](auto closure) {
        instrumentClosure(map, closure);
        return mlir::WalkResult::advance();
      })
      .Default([](mlir::Operation *) { return mlir::WalkResult::advance(); });
}

/// Returns the PGO name of `function`, as `llvm::getPGOFuncName` makes it.
/// `file` is the source file of the function.
std::string pgoFuncName(mlir::func::FuncOp function, llvm::StringRef file) {
  auto linkage =
      function->getAttrOfType<mlir::LLVM::LinkageAttr>("llvm.linkage");
  bool local =
      linkage && (linkage.getLinkage() == mlir::LLVM::Linkage::Internal ||
                  linkage.getLinkage() == mlir::LLVM::Linkage::Private);
  return llvm::getPGOFuncName(function.getSymName(),
                              local ? llvm::GlobalValue::InternalLinkage
                                    : llvm::GlobalValue::ExternalLinkage,
                              file);
}

/// Returns the private constant global that holds `pgoName`, and makes it
/// when the module does not have it.
mlir::LLVM::GlobalOp profileNameGlobal(mlir::SymbolTable &symbols,
                                       llvm::StringRef pgoName) {
  // Clang gives private linkage to the names of external and internal
  // functions.
  std::string symbol =
      llvm::getPGOFuncNameVarName(pgoName, llvm::GlobalValue::PrivateLinkage);
  if (auto global = symbols.lookup<mlir::LLVM::GlobalOp>(symbol))
    return global;
  auto module = llvm::cast<mlir::ModuleOp>(symbols.getOp());
  auto builder = mlir::OpBuilder::atBlockBegin(module.getBody());
  auto type =
      mlir::LLVM::LLVMArrayType::get(builder.getI8Type(), pgoName.size());
  auto global = mlir::LLVM::GlobalOp::create(
      builder, module.getLoc(), type, /*isConstant=*/true,
      mlir::LLVM::Linkage::Private, symbol, builder.getStringAttr(pgoName));
  symbols.insert(global);
  return global;
}

/// Instruments `function` when it has a body, a source span and coverage.
void instrumentFunction(mlir::func::FuncOp function,
                        mlir::SymbolTable &symbols) {
  // A function with the attribute has its counters already.
  if (function.isExternal() || function->hasAttr(coverageMapAttr) ||
      !coverageEnabled(function))
    return;
  // A kernel is StableHLO code for a device, so it cannot hold counters.
  if (function->hasAttr("reussir.kernel_function"))
    return;
  auto span = function.getLoc()->findInstanceOf<mlir::FileLineColRange>();
  if (!span)
    return;
  CoverageMap map(function, span);
  function.walk<mlir::WalkOrder::PreOrder>(
      [&](mlir::Operation *op) { return instrumentOp(map, op); });
  std::string pgoName = pgoFuncName(function, span.getFilename().getValue());
  map.emit(profileNameGlobal(symbols, pgoName), pgoName);
}

struct ReussirInstrumentCoveragePass
    : public impl::ReussirInstrumentCoveragePassBase<
          ReussirInstrumentCoveragePass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    mlir::SymbolTable symbols(module);
    for (auto function : llvm::to_vector(module.getOps<mlir::func::FuncOp>()))
      instrumentFunction(function, symbols);
  }
};

} // namespace
} // namespace reussir
