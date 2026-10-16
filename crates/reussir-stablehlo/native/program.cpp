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
/// This file implements parsing and serialization of runtime programs.
///
//===----------------------------------------------------------------------===//

#include "program.h"

#include <llvm/Support/raw_ostream.h>
#include <mlir/Bytecode/BytecodeWriter.h>
#include <mlir/Dialect/Func/Extensions/AllExtensions.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>
#include <stablehlo/dialect/Register.h>

// Defined in the ReussirIfrt archive. Outlined kernels carry IFRT and SDY
// attributes, and registering them does not pull in the Reussir compiler.
void registerIfrtDialects(mlir::DialectRegistry &);

namespace reussir::stablehlo {
namespace {
mlir::DialectRegistry makeRegistry() {
  mlir::DialectRegistry registry;
  mlir::stablehlo::registerAllDialects(registry);
  registry.insert<mlir::func::FuncDialect>();
  mlir::func::registerAllExtensions(registry);
  registerIfrtDialects(registry);
  return registry;
}
} // namespace

Program::Program(ReussirStablehloBytes diagnostic, void *userData)
    : diagnostic(diagnostic), userData(userData),
      context(makeRegistry(), mlir::MLIRContext::Threading::DISABLED),
      handler(&context, [this](mlir::Diagnostic &diag) {
        std::string message;
        llvm::raw_string_ostream stream(message);
        diag.print(stream);
        stream << '\n';
        report(message);
        return mlir::success();
      }) {}

ReussirStablehloStatus Program::parse(const uint8_t *data, size_t size,
                                      size_t argumentCount) {
  if (!data && size) {
    report("null program with nonzero size");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  module = mlir::parseSourceString<mlir::ModuleOp>(
      llvm::StringRef(reinterpret_cast<const char *>(data), size), &context);
  if (!module || mlir::failed(mlir::verify(*module)))
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  auto entry = getEntry();
  if (!entry || entry.isExternal() ||
      entry.getNumArguments() != argumentCount) {
    report("program must define @main with one argument per descriptor");
    return REUSSIR_STABLEHLO_INVALID_INPUT;
  }
  return REUSSIR_STABLEHLO_OK;
}

mlir::func::FuncOp Program::getEntry() {
  return module->lookupSymbol<mlir::func::FuncOp>("main");
}

void Program::report(llvm::StringRef message) const {
  diagnostic(reinterpret_cast<const uint8_t *>(message.data()), message.size(),
             userData);
}

ReussirStablehloStatus Program::write(ReussirStablehloBytes output) {
  std::string bytes;
  llvm::raw_string_ostream stream(bytes);
  if (mlir::failed(mlir::writeBytecodeToFile(*module, stream)))
    return REUSSIR_STABLEHLO_SERIALIZATION_FAILED;
  output(reinterpret_cast<const uint8_t *>(bytes.data()), bytes.size(),
         userData);
  return REUSSIR_STABLEHLO_OK;
}

} // namespace reussir::stablehlo
