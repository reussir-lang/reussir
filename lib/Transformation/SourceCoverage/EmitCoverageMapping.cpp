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
/// `reussir-emit-coverage-mapping` writes the `reussir.coverage.map`
/// attributes as the coverage mapping globals that clang writes
/// (`CoverageMappingModuleGen` in `clang/lib/CodeGen/CoverageMappingGen.cpp`).
///
//===----------------------------------------------------------------------===//

#include "Reussir/Transformation/Passes.h"

#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/UniqueVector.h>
#include <llvm/ProfileData/Coverage/CoverageMappingWriter.h>
#include <llvm/ProfileData/InstrProf.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>

namespace reussir {
#define GEN_PASS_DEF_REUSSIREMITCOVERAGEMAPPINGPASS
#define GEN_PASS_DEF_REUSSIREMITCOVERAGEUNUSEDNAMESPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {
using llvm::coverage::Counter;
using llvm::coverage::CounterMappingRegion;
namespace LLVM = mlir::LLVM;

constexpr llvm::StringLiteral kCoverageMap = "reussir.coverage.map";

// The encoded coverage mapping of one function.
struct FunctionRecord {
  uint64_t nameHash;
  uint64_t functionHash;
  std::string mapping;
};

// Removes the dot components of a path, as clang does.
std::string normalizePath(llvm::StringRef path) {
  llvm::SmallString<256> result(path);
  llvm::sys::path::remove_dots(result, /*remove_dot_dot=*/true);
  return std::string(result);
}

// Encodes the coverage map of `function`. The ID of a file in `files` is its
// index in the file table of the module.
mlir::FailureOr<FunctionRecord>
encodeFunction(mlir::Operation *function, mlir::Attribute attribute,
               llvm::UniqueVector<std::string> &files) {
  auto fail = [&](llvm::StringRef message) {
    function->emitError() << "invalid '" << kCoverageMap << "': " << message;
    return mlir::failure();
  };
  auto map = llvm::dyn_cast<mlir::DictionaryAttr>(attribute);
  auto name = map ? map.getAs<mlir::StringAttr>("name") : nullptr;
  auto hash = map ? map.getAs<mlir::IntegerAttr>("hash") : nullptr;
  auto counters = map ? map.getAs<mlir::IntegerAttr>("counters") : nullptr;
  auto regions = map ? map.getAs<mlir::ArrayAttr>("regions") : nullptr;
  if (!name || !hash || !counters || !regions)
    return fail("expected 'name', 'hash', 'counters' and 'regions'");
  auto counter = [&](mlir::DictionaryAttr entry,
                     llvm::StringRef key) -> std::optional<Counter> {
    auto index = entry.getAs<mlir::IntegerAttr>(key);
    if (!index || index.getValue().uge(counters.getValue().getZExtValue()))
      return std::nullopt;
    return Counter::getCounter(index.getValue().getZExtValue());
  };

  // The files of a function have the IDs 1, 2, ... in `local`.
  llvm::UniqueVector<unsigned> local;
  llvm::SmallVector<CounterMappingRegion> mapping;
  for (mlir::Attribute element : regions) {
    auto entry = llvm::dyn_cast<mlir::DictionaryAttr>(element);
    auto span = entry ? entry.getAs<mlir::FileLineColRange>("span") : nullptr;
    if (!span)
      return fail("expected a 'span' location in each region");
    unsigned startLine = span.getStartLine();
    unsigned startColumn = span.getStartColumn();
    unsigned endLine = span.getEndLine();
    unsigned endColumn = span.getEndColumn();
    if (std::pair(startLine, startColumn) > std::pair(endLine, endColumn))
      return fail("a region ends before it starts");
    std::string filename = normalizePath(span.getFilename().getValue());
    unsigned file = local.insert(files.insert(filename)) - 1;
    if (entry.contains("counter")) {
      std::optional<Counter> count = counter(entry, "counter");
      if (!count)
        return fail("a counter is not less than 'counters'");
      mapping.push_back(CounterMappingRegion::makeRegion(
          *count, file, startLine, startColumn, endLine, endColumn));
      continue;
    }
    std::optional<Counter> trueCount = counter(entry, "true_counter");
    std::optional<Counter> falseCount = counter(entry, "false_counter");
    if (!trueCount || !falseCount)
      return fail("expected a 'counter', or a 'true_counter' and a "
                  "'false_counter', less than 'counters'");
    mapping.push_back(CounterMappingRegion::makeBranchRegion(
        *trueCount, *falseCount, file, startLine, startColumn, endLine,
        endColumn));
  }

  FunctionRecord record{llvm::IndexedInstrProf::ComputeHash(name.getValue()),
                        hash.getValue().getZExtValue(),
                        {}};
  llvm::SmallVector<unsigned> virtualFiles(local.begin(), local.end());
  llvm::raw_string_ostream os(record.mapping);
  llvm::coverage::CoverageMappingWriter(virtualFiles, /*Expressions=*/{},
                                        mapping)
      .write(os);
  return record;
}

// Gives `global` an initializer with one constant for each struct member.
void setInitializer(mlir::OpBuilder &builder, LLVM::GlobalOp global,
                    llvm::ArrayRef<mlir::Attribute> fields) {
  mlir::OpBuilder::InsertionGuard guard(builder);
  mlir::Location loc = global.getLoc();
  auto type = llvm::cast<LLVM::LLVMStructType>(global.getGlobalType());
  builder.createBlock(&global.getInitializerRegion());
  mlir::Value value = LLVM::PoisonOp::create(builder, loc, type);
  for (auto [index, member, field] : llvm::enumerate(type.getBody(), fields)) {
    mlir::Value constant =
        LLVM::ConstantOp::create(builder, loc, member, field);
    value = LLVM::InsertValueOp::create(builder, loc, value, constant,
                                        int64_t(index));
  }
  LLVM::ReturnOp::create(builder, loc, value);
}

// Adds `globals` to `llvm.used`, as clang does for the coverage globals.
mlir::LogicalResult appendToUsed(mlir::OpBuilder &builder,
                                 mlir::ModuleOp module,
                                 llvm::ArrayRef<LLVM::GlobalOp> globals) {
  mlir::OpBuilder::InsertionGuard guard(builder);
  mlir::Location loc = module.getLoc();
  auto ptr = LLVM::LLVMPointerType::get(builder.getContext());
  auto used = module.lookupSymbol<LLVM::GlobalOp>("llvm.used");
  // Copy the elements of an existing `llvm.used` first.
  mlir::Value old;
  uint64_t count = 0;
  if (used) {
    mlir::Block *body = used.getInitializerBlock();
    if (!body)
      return used.emitError("expected an initializer region");
    auto ret = llvm::cast<LLVM::ReturnOp>(body->getTerminator());
    old = ret.getArg();
    count =
        llvm::cast<LLVM::LLVMArrayType>(used.getGlobalType()).getNumElements();
    ret.erase();
    builder.setInsertionPointToEnd(body);
  }
  auto type = LLVM::LLVMArrayType::get(ptr, count + globals.size());
  if (used) {
    used.setGlobalType(type);
  } else {
    used = LLVM::GlobalOp::create(builder, loc, type, /*isConstant=*/false,
                                  LLVM::Linkage::Appending, "llvm.used",
                                  mlir::Attribute());
    used.setSection(llvm::StringRef("llvm.metadata"));
    builder.createBlock(&used.getInitializerRegion());
  }
  mlir::Value array = LLVM::PoisonOp::create(builder, loc, type);
  for (uint64_t index = 0; index < count; ++index) {
    mlir::Value element =
        LLVM::ExtractValueOp::create(builder, loc, old, int64_t(index));
    array = LLVM::InsertValueOp::create(builder, loc, array, element,
                                        int64_t(index));
  }
  for (auto [index, global] : llvm::enumerate(globals)) {
    mlir::Value element = LLVM::AddressOfOp::create(builder, loc, global);
    array = LLVM::InsertValueOp::create(builder, loc, array, element,
                                        int64_t(count + index));
  }
  LLVM::ReturnOp::create(builder, loc, array);
  return mlir::success();
}

struct EmitCoverageMappingPass
    : public impl::ReussirEmitCoverageMappingPassBase<EmitCoverageMappingPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    llvm::UniqueVector<std::string> files;
    llvm::SmallVector<std::pair<mlir::Location, FunctionRecord>> records;
    for (mlir::Operation &function : module.getBody()->getOperations()) {
      mlir::Attribute map = function.removeDiscardableAttr(kCoverageMap);
      if (!map)
        continue;
      mlir::FailureOr<FunctionRecord> record =
          encodeFunction(&function, map, files);
      if (mlir::failed(record))
        return signalPassFailure();
      records.emplace_back(function.getLoc(), std::move(*record));
    }
    if (records.empty())
      return;

    // Entry 0 of the file table is the compilation directory, as in clang.
    llvm::SmallString<256> directory(compilationDir);
    if (directory.empty())
      (void)llvm::sys::fs::current_path(directory);
    llvm::SmallVector<std::string> filenames{normalizePath(directory)};
    llvm::append_range(filenames, files);
    std::string encodedFiles;
    llvm::raw_string_ostream os(encodedFiles);
    llvm::coverage::CoverageFilenamesSectionWriter(filenames).write(os);
    uint64_t filenamesRef = llvm::IndexedInstrProf::ComputeHash(encodedFiles);

    auto triple = module->getAttrOfType<mlir::StringAttr>(
        LLVM::LLVMDialect::getTargetTripleAttrName());
    llvm::Triple target(triple ? triple.getValue().str()
                               : llvm::sys::getDefaultTargetTriple());
    llvm::Triple::ObjectFormatType format = target.getObjectFormat();

    auto builder = mlir::OpBuilder::atBlockEnd(module.getBody());
    mlir::MLIRContext *context = &getContext();
    mlir::Type i8 = builder.getI8Type();
    mlir::Type i32 = builder.getI32Type();
    mlir::Type i64 = builder.getI64Type();
    auto u64 = [&](uint64_t value) {
      return builder.getI64IntegerAttr(static_cast<int64_t>(value));
    };
    LLVM::ComdatOp comdat;
    if (target.supportsCOMDAT()) {
      auto existing = module.getOps<LLVM::ComdatOp>();
      comdat = existing.empty()
                   ? LLVM::ComdatOp::create(builder, module.getLoc(),
                                            "__llvm_global_comdat")
                   : *existing.begin();
    }

    llvm::SmallVector<LLVM::GlobalOp> used;
    for (auto &[loc, record] : records) {
      std::string name = "__covrec_" + llvm::utohexstr(record.nameHash) + "u";
      mlir::SymbolRefAttr comdatRef;
      if (comdat) {
        mlir::OpBuilder::InsertionGuard guard(builder);
        builder.setInsertionPointToEnd(&comdat.getBody().front());
        LLVM::ComdatSelectorOp::create(builder, loc, name,
                                       LLVM::comdat::Comdat::Any);
        comdatRef = mlir::SymbolRefAttr::get(
            comdat.getSymNameAttr(),
            mlir::FlatSymbolRefAttr::get(mlir::StringAttr::get(context, name)));
      }
      auto bytes = LLVM::LLVMArrayType::get(i8, record.mapping.size());
      auto type = LLVM::LLVMStructType::getLiteral(
          context, {i64, i32, i64, i64, bytes}, /*isPacked=*/true);
      auto global = LLVM::GlobalOp::create(
          builder, loc, type, /*isConstant=*/true, LLVM::Linkage::LinkonceODR,
          name, mlir::Attribute(), /*alignment=*/8, /*addrSpace=*/0,
          /*dsoLocal=*/false, /*thread_local_=*/false, comdatRef);
      global.setVisibility_(LLVM::Visibility::Hidden);
      global.setSection(
          llvm::getInstrProfSectionName(llvm::IPSK_covfun, format));
      setInitializer(builder, global,
                     {u64(record.nameHash),
                      builder.getI32IntegerAttr(record.mapping.size()),
                      u64(record.functionHash), u64(filenamesRef),
                      builder.getStringAttr(record.mapping)});
      used.push_back(global);
    }

    auto header =
        LLVM::LLVMStructType::getLiteral(context, {i32, i32, i32, i32});
    auto fileBytes = LLVM::LLVMArrayType::get(i8, encodedFiles.size());
    auto mapping = LLVM::GlobalOp::create(
        builder, module.getLoc(),
        LLVM::LLVMStructType::getLiteral(context, {header, fileBytes}),
        /*isConstant=*/true, LLVM::Linkage::Private,
        llvm::getCoverageMappingVarName(), mlir::Attribute(), /*alignment=*/8);
    mapping.setSection(
        llvm::getInstrProfSectionName(llvm::IPSK_covmap, format));
    // The header has no records and no mappings since format version 4.
    setInitializer(builder, mapping,
                   {builder.getI32ArrayAttr(
                        {0, static_cast<int32_t>(encodedFiles.size()), 0,
                         llvm::coverage::CovMapVersion::CurrentVersion}),
                    builder.getStringAttr(encodedFiles)});
    used.push_back(mapping);
    if (mlir::failed(appendToUsed(builder, module, used)))
      signalPassFailure();
  }
};

struct EmitCoverageUnusedNamesPass
    : public impl::ReussirEmitCoverageUnusedNamesPassBase<
          EmitCoverageUnusedNamesPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::ModuleOp module = getOperation();
    mlir::SymbolTableCollection symbols;
    mlir::SymbolUserMap users(symbols, module);
    // The lowering fails when an increment uses a name in the list.
    llvm::SmallVector<LLVM::GlobalOp> names;
    for (auto global : module.getOps<LLVM::GlobalOp>())
      if (global.getSymName().starts_with(llvm::getInstrProfNameVarPrefix()) &&
          users.useEmpty(global))
        names.push_back(global);
    if (names.empty())
      return;

    auto builder = mlir::OpBuilder::atBlockEnd(module.getBody());
    mlir::Location loc = module.getLoc();
    auto type = LLVM::LLVMArrayType::get(
        LLVM::LLVMPointerType::get(&getContext()), names.size());
    auto list = LLVM::GlobalOp::create(
        builder, loc, type, /*isConstant=*/true, LLVM::Linkage::Internal,
        llvm::getCoverageUnusedNamesVarName(), mlir::Attribute());
    builder.createBlock(&list.getInitializerRegion());
    mlir::Value array = LLVM::PoisonOp::create(builder, loc, type);
    for (auto [index, name] : llvm::enumerate(names)) {
      mlir::Value element = LLVM::AddressOfOp::create(builder, loc, name);
      array = LLVM::InsertValueOp::create(builder, loc, array, element,
                                          int64_t(index));
    }
    LLVM::ReturnOp::create(builder, loc, array);
  }
};
} // namespace
} // namespace reussir
