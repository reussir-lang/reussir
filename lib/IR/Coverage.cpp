#include "Reussir/IR/Coverage.h"

#include <llvm/Demangle/Demangle.h>
#include <llvm/Support/JSON.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/SymbolTable.h>

namespace reussir {
namespace {
constexpr llvm::StringLiteral originAttr = "reussir.coverage_origin";
constexpr llvm::StringLiteral enabledAttr = "reussir.coverage";

bool matches(mlir::ArrayAttr scopes, llvm::StringRef name) {
  return scopes && llvm::any_of(scopes, [&](mlir::Attribute attr) {
           auto prefix = llvm::cast<mlir::StringAttr>(attr).getValue();
           return name == prefix || name.starts_with((prefix + "::").str()) ||
                  name.starts_with((prefix + "<").str());
         });
}

void annotate(mlir::Operation *op, bool enabled, mlir::StringAttr origin) {
  mlir::Builder builder(op->getContext());
  op->setAttr(enabledAttr, builder.getBoolAttr(enabled));
  if (origin)
    op->setAttr(originAttr, origin);
  if (!llvm::isa<mlir::func::FuncOp, mlir::LLVM::LLVMFuncOp>(op))
    return;
  auto name = llvm::isa<mlir::LLVM::LLVMFuncOp>(op) ? "passthrough"
                                                    : "llvm.passthrough";
  llvm::SmallVector<mlir::Attribute> attrs;
  if (auto existing = op->getAttrOfType<mlir::ArrayAttr>(name))
    for (auto attr : existing) {
      auto key = llvm::dyn_cast<mlir::StringAttr>(attr);
      if (auto pair = llvm::dyn_cast<mlir::ArrayAttr>(attr);
          pair && !pair.empty())
        key = llvm::dyn_cast<mlir::StringAttr>(pair[0]);
      if (!key || !key.getValue().starts_with("reussir-coverage"))
        attrs.push_back(attr);
    }
  attrs.push_back(
      builder.getArrayAttr({builder.getStringAttr("reussir-coverage"),
                            builder.getStringAttr(enabled ? "on" : "off")}));
  auto module = op->getParentOfType<mlir::ModuleOp>();
  auto strings = [&](llvm::StringRef attr) {
    llvm::SmallVector<llvm::StringRef> values;
    if (auto array = module->getAttrOfType<mlir::ArrayAttr>(attr))
      for (auto value : array)
        values.push_back(llvm::cast<mlir::StringAttr>(value).getValue());
    return values;
  };
  attrs.push_back(
      builder.getArrayAttr({builder.getStringAttr("reussir-coverage-policy"),
                            builder.getStringAttr(coverageScopeKey(
                                strings("reussir.coverage.include"),
                                strings("reussir.coverage.exclude")))}));
  if (origin)
    attrs.push_back(builder.getArrayAttr(
        {builder.getStringAttr("reussir-coverage-origin"), origin}));
  if (!enabled &&
      !llvm::is_contained(attrs, builder.getStringAttr("nosanitize_coverage")))
    attrs.push_back(builder.getStringAttr("nosanitize_coverage"));
  op->setAttr(name, builder.getArrayAttr(attrs));
}
} // namespace

std::string coverageScopeKey(llvm::ArrayRef<llvm::StringRef> includes,
                             llvm::ArrayRef<llvm::StringRef> excludes) {
  std::string result;
  llvm::raw_string_ostream os(result);
  os << llvm::json::Value(
      llvm::json::Object{{"include", llvm::json::Array(includes)},
                         {"exclude", llvm::json::Array(excludes)}});
  return result;
}

bool coverageEnabled(mlir::Operation *op) {
  auto module = op->getParentOfType<mlir::ModuleOp>();
  if (!module || !module->hasAttr("reussir.coverage.include"))
    return true;
  for (auto *parent = op; parent; parent = parent->getParentOp()) {
    if (auto enabled = parent->getAttrOfType<mlir::BoolAttr>(enabledAttr))
      return enabled.getValue();
    auto origin = parent->getAttrOfType<mlir::StringAttr>(originAttr);
    if (!origin && llvm::isa<mlir::func::FuncOp>(parent))
      origin = parent->getAttrOfType<mlir::StringAttr>(
          mlir::SymbolTable::getSymbolAttrName());
    if (!origin)
      continue;
    auto name = llvm::demangle(origin.getValue().str());
    auto includes =
        module->getAttrOfType<mlir::ArrayAttr>("reussir.coverage.include");
    auto excludes =
        module->getAttrOfType<mlir::ArrayAttr>("reussir.coverage.exclude");
    return (includes.empty() || matches(includes, name)) &&
           !matches(excludes, name);
  }
  return false;
}

bool sameCoverageScope(mlir::Operation *lhs, mlir::Operation *rhs) {
  return coverageEnabled(lhs) == coverageEnabled(rhs);
}

void prepareCoverage(mlir::ModuleOp module) {
  module.walk<mlir::WalkOrder::PreOrder>([](mlir::Operation *op) {
    if (llvm::isa<mlir::func::FuncOp>(op) || op->hasAttr(originAttr))
      annotate(op, coverageEnabled(op),
               op->getAttrOfType<mlir::StringAttr>(originAttr));
  });
}

void inheritCoverage(mlir::Operation *source, mlir::Operation *function) {
  auto module = source->getParentOfType<mlir::ModuleOp>();
  if (module && module->hasAttr("reussir.coverage.include")) {
    // Outlined evaluations replace the generated-helper default with their
    // body.
    if (auto attrs =
            function->getAttrOfType<mlir::ArrayAttr>("llvm.passthrough")) {
      llvm::SmallVector<mlir::Attribute> inherited;
      llvm::copy_if(attrs, std::back_inserter(inherited),
                    [](mlir::Attribute attr) {
                      auto name = llvm::dyn_cast<mlir::StringAttr>(attr);
                      return !name || name.getValue() != "nosanitize_coverage";
                    });
      function->setAttr(
          "llvm.passthrough",
          mlir::ArrayAttr::get(function->getContext(), inherited));
    }
    annotate(function, coverageEnabled(source),
             source->getAttrOfType<mlir::StringAttr>(originAttr));
  }
}

void excludeCoverage(mlir::Operation *function) {
  auto module = function->getParentOfType<mlir::ModuleOp>();
  if (module && module->hasAttr("reussir.coverage.include"))
    annotate(function, false, {});
}
} // namespace reussir
