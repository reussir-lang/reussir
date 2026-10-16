// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include "specialize.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>
#include <mlir/Bytecode/BytecodeWriter.h>
#include <mlir/Dialect/Func/Extensions/AllExtensions.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Diagnostics.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Transforms/Passes.h>
#include <stablehlo/dialect/Register.h>
#include <stablehlo/dialect/StablehloOps.h>
#include <stablehlo/transforms/Passes.h>

// Includes SDY attributes in outlined kernels; ordinary archive extraction,
// without a dependency on the Reussir compiler or JIT.
void registerIfrtDialects(mlir::DialectRegistry &);

extern "C" uint32_t reussir_stablehlo_specialize(
    const uint8_t *program, size_t programSize,
    const ReussirStablehloArgument *arguments, size_t argumentCount,
    ReussirStablehloBytes output, ReussirStablehloShape outputShape,
    ReussirStablehloBytes diagnostic, void *userData) {
  auto report = [&](llvm::StringRef message) {
    diagnostic(reinterpret_cast<const uint8_t *>(message.data()),
               message.size(), userData);
  };
  if ((!program && programSize) || (!arguments && argumentCount)) {
    report("null nonempty specialization input");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  mlir::DialectRegistry registry;
  mlir::stablehlo::registerAllDialects(registry);
  registry.insert<mlir::func::FuncDialect>();
  mlir::func::registerAllExtensions(registry);
  registerIfrtDialects(registry);
  mlir::MLIRContext context(registry, mlir::MLIRContext::Threading::DISABLED);
  mlir::ScopedDiagnosticHandler handler(&context, [&](mlir::Diagnostic &diag) {
    std::string message;
    llvm::raw_string_ostream stream(message);
    diag.print(stream);
    stream << '\n';
    report(message);
    return mlir::success();
  });
  auto module = mlir::parseSourceString<mlir::ModuleOp>(
      llvm::StringRef(reinterpret_cast<const char *>(program), programSize),
      &context);
  if (!module || mlir::failed(mlir::verify(*module)))
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  auto entry = module->lookupSymbol<mlir::func::FuncOp>("main");
  if (!entry || entry.isExternal() || entry.getNumArguments() != argumentCount) {
    report("specialization requires a defined @main with matching argument count");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  llvm::SmallVector<mlir::Type> types;
  for (size_t i = 0; i < argumentCount; ++i) {
    const auto &arg = arguments[i];
    auto type = llvm::dyn_cast<mlir::RankedTensorType>(entry.getArgument(i).getType());
    if (!type || size_t(type.getRank()) != arg.rank ||
        (arg.rank && !arg.dimensions) || (arg.value_count && !arg.values)) {
      report("invalid ranked tensor argument descriptor");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    llvm::ArrayRef<int64_t> dims(arg.dimensions, arg.rank);
    for (auto [original, actual] : llvm::zip(type.getShape(), dims)) {
      if (actual < 0 || (!mlir::ShapedType::isDynamic(original) && original != actual)) {
        report("logical dimensions must be nonnegative and match static dimensions");
        return REUSSIR_STABLEHLO_INVALID_INPUT;
      }
    }
    // Bounds disappear once all dimensions become concrete.
    types.push_back(mlir::RankedTensorType::get(dims, type.getElementType()));
    if (!arg.has_values)
      continue;
    auto integer = llvm::dyn_cast<mlir::IntegerType>(type.getElementType());
    if (arg.rank > 1 || !type.hasStaticShape() || !integer ||
        !integer.isSignless() || (integer.getWidth() != 32 && integer.getWidth() != 64) ||
        uint64_t(type.getNumElements()) != arg.value_count) {
      report("shape values require a static scalar/vector of i32 or i64 with matching length");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
    llvm::SmallVector<llvm::APInt> values;
    for (size_t j = 0; j < arg.value_count; ++j) {
      if (integer.getWidth() == 32 &&
          (arg.values[j] < INT32_MIN || arg.values[j] > INT32_MAX)) {
        report("shape value does not fit i32");
        return REUSSIR_STABLEHLO_INVALID_INPUT;
      }
      values.emplace_back(integer.getWidth(), arg.values[j], true);
    }
    mlir::OpBuilder builder(&context);
    builder.setInsertionPointToStart(&entry.getBody().front());
    auto constant = mlir::stablehlo::ConstantOp::create(
        builder, entry.getLoc(), mlir::DenseIntElementsAttr::get(type, values));
    entry.getArgument(i).replaceAllUsesWith(constant);
  }
  // Pinned upstream pipeline, following XLA's polymorphic-shape refinement.
  mlir::PassManager pm(&context);
  pm.addPass(mlir::createInlinerPass());
  pm.addPass(mlir::createCSEPass());
  mlir::stablehlo::createStablehloRemoveDynamismPipeline(pm, types);
  pm.addPass(mlir::createCanonicalizerPass());
  pm.addPass(mlir::createSymbolDCEPass());
  if (mlir::failed(pm.run(*module)))
    return REUSSIR_STABLEHLO_REFINEMENT_FAILED;
  bool dynamic = false;
  auto check = [&](mlir::Type type) {
    if (auto shaped = llvm::dyn_cast<mlir::ShapedType>(type))
      dynamic |= !shaped.hasStaticShape();
  };
  module->walk([&](mlir::Operation *op) {
    for (auto type : op->getResultTypes())
      check(type);
    for (auto &region : op->getRegions())
      for (auto &block : region)
        for (auto arg : block.getArguments())
          check(arg.getType());
    if (auto function = llvm::dyn_cast<mlir::func::FuncOp>(op))
      for (auto type : function.getResultTypes())
        check(type);
    if (auto call = llvm::dyn_cast<mlir::stablehlo::CustomCallOp>(op))
      dynamic |= call.getCallTargetName() == "shape_assertion" ||
                 call.getCallTargetName() == "stablehlo.shape_refinement_operand_wrapper";
  });
  if (dynamic) {
    report("unsupported residual dynamism or unresolved shape assertion after specialization");
    return REUSSIR_STABLEHLO_RESIDUAL_DYNAMISM;
  }
  if (mlir::failed(mlir::verify(*module)))
    return REUSSIR_STABLEHLO_REFINEMENT_FAILED;
  entry = module->lookupSymbol<mlir::func::FuncOp>("main");
  for (auto type : entry.getResultTypes()) {
    if (!llvm::isa<mlir::RankedTensorType>(type)) {
      report("specialization requires ranked tensor results");
      return REUSSIR_STABLEHLO_INVALID_INPUT;
    }
  }
  std::string bytes;
  llvm::raw_string_ostream stream(bytes);
  if (mlir::failed(mlir::writeBytecodeToFile(*module, stream)))
    return REUSSIR_STABLEHLO_SERIALIZATION_FAILED;
  output(reinterpret_cast<const uint8_t *>(bytes.data()), bytes.size(), userData);
  for (auto type : entry.getResultTypes()) {
    auto shape = llvm::cast<mlir::RankedTensorType>(type).getShape();
    outputShape(shape.data(), shape.size(), userData);
  }
  return REUSSIR_STABLEHLO_OK;
}
