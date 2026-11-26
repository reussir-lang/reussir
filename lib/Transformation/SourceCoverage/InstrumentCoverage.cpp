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
#include "Reussir/Transformation/Passes.h"

#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/ProfileData/InstrProf.h>
#include <llvm/Support/EndianStream.h>
#include <llvm/Support/xxhash.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/ControlFlowInterfaces.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRINSTRUMENTCOVERAGEPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

constexpr llvm::StringLiteral coverageMapAttr = "reussir.coverage.map";

/// A line and a column in a source file.
using Position = std::pair<unsigned, unsigned>;

Position startOf(mlir::FileLineColRange span) {
  return {span.getStartLine(), span.getStartColumn()};
}

Position endOf(mlir::FileLineColRange span) {
  return {span.getEndLine(), span.getEndColumn()};
}

mlir::FileLineColRange makeSpan(mlir::StringAttr file, Position start,
                                Position end) {
  return mlir::FileLineColRange::get(file, start.first, start.second, end.first,
                                     end.second);
}

/// Returns the smallest span that contains `lhs` and `rhs`.
mlir::FileLineColRange unite(mlir::FileLineColRange lhs,
                             mlir::FileLineColRange rhs) {
  return makeSpan(lhs.getFilename(), std::min(startOf(lhs), startOf(rhs)),
                  std::max(endOf(lhs), endOf(rhs)));
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
/// region has the counter `counter`. A gap region is a code region that
/// gives its count only to the lines that start in it.
struct MappingRegion {
  mlir::FileLineColRange span;
  llvm::SmallVector<std::pair<llvm::StringRef, uint32_t>, 2> counters;
  bool gap = false;
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

  /// Adds a gap region that counter `counter` counts.
  void addGapRegion(uint32_t counter, mlir::FileLineColRange span) {
    regions.push_back({span, {{"counter", counter}}, /*gap=*/true});
  }

  /// Adds a counter at the start of the entry block of `region`. Its code
  /// region is the span of the ops in `region`. Returns the counter, or
  /// nothing when no op in `region` has a span in the file of the function.
  std::optional<uint32_t> addRegion(mlir::Region &region) {
    return addRegion(region, regionSpan(region, file));
  }

  /// Adds a counter at the start of the entry block of `region`, with the
  /// code region `span`. Returns nothing when `span` is null.
  std::optional<uint32_t> addRegion(mlir::Region &region,
                                    mlir::FileLineColRange span) {
    if (!span)
      return std::nullopt;
    uint32_t counter = addCounter(region.front(), span);
    addCodeRegion(counter, span);
    return counter;
  }

  mlir::StringAttr getFile() const { return file; }

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
      writer.write<uint8_t>(region.gap);
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
      if (region.gap)
        entries.push_back(
            builder.getNamedAttr("kind", builder.getStringAttr("gap")));
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

/// Returns the end of the operands of `op`, such as the condition of an
/// `if`. It is the last end of the spans of the ops that give the operands
/// and that are in `span`. Returns nothing when no such op has a span.
std::optional<Position> operandsEnd(mlir::Operation *op,
                                    mlir::FileLineColRange span) {
  std::optional<Position> result;
  for (mlir::Value operand : op->getOperands()) {
    mlir::Operation *def = operand.getDefiningOp();
    auto operandSpan =
        def ? def->getLoc()->findInstanceOf<mlir::FileLineColRange>() : nullptr;
    if (operandSpan && operandSpan.getFilename() == span.getFilename() &&
        startOf(span) <= startOf(operandSpan) &&
        endOf(operandSpan) <= endOf(span))
      result = std::max(result, std::optional(endOf(operandSpan)));
  }
  return result;
}

/// Fills the space before each code region in `counted` with a gap region
/// of its counter, as clang does before `then` and `else`. Then llvm-cov
/// gives a line that starts in the space, such as `else {` or a `match`
/// arm, the count of the next region. The first gap starts at the end of
/// the operands of `op`. It cannot start at the start of `op`: the region
/// that contains `op` can start there too, and llvm-cov then drops the
/// start of that region.
void addGapRegions(
    CoverageMap &map, mlir::Operation *op,
    llvm::MutableArrayRef<std::pair<mlir::FileLineColRange, uint32_t>>
        counted) {
  auto span = op->getLoc()->findInstanceOf<mlir::FileLineColRange>();
  if (!span || span.getFilename() != map.getFile())
    return;
  // The regions of an op can be in a different order in the source.
  llvm::sort(counted, [](auto lhs, auto rhs) {
    return startOf(lhs.first) < startOf(rhs.first);
  });
  std::optional<Position> from = operandsEnd(op, span);
  for (auto [codeSpan, counter] : counted) {
    if (from && *from < startOf(codeSpan))
      map.addGapRegion(counter,
                       makeSpan(map.getFile(), *from, startOf(codeSpan)));
    from = std::max(from, std::optional(endOf(codeSpan)));
  }
}

/// Gives a counter to each region of `op`.
void instrumentRegionBranch(CoverageMap &map,
                            mlir::RegionBranchOpInterface op) {
  llvm::SmallVector<std::pair<mlir::FileLineColRange, uint32_t>> counted;
  for (mlir::Region &region : op->getRegions()) {
    mlir::FileLineColRange span = regionSpan(region, map.getFile());
    if (auto counter = map.addRegion(region, span))
      counted.emplace_back(span, *counter);
  }
  addGapRegions(map, op, counted);
}

/// Adds the counters of `op` to `map`. Add a case here for an op that needs
/// different counters.
void instrumentOp(CoverageMap &map, mlir::Operation *op) {
  llvm::TypeSwitch<mlir::Operation *>(op).Case<mlir::RegionBranchOpInterface>(
      [&](auto branch) { instrumentRegionBranch(map, branch); });
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
  auto span = function.getLoc()->findInstanceOf<mlir::FileLineColRange>();
  if (!span)
    return;
  CoverageMap map(function, span);
  function.walk<mlir::WalkOrder::PreOrder>(
      [&](mlir::Operation *op) { instrumentOp(map, op); });
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
