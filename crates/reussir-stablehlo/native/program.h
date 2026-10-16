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
/// This file declares the parsed-program state shared by every entry point.
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_STABLEHLO_PROGRAM_H
#define REUSSIR_STABLEHLO_PROGRAM_H

#include "reussir_stablehlo.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/Diagnostics.h>
#include <mlir/IR/MLIRContext.h>
#include <mlir/IR/OwningOpRef.h>

namespace reussir::stablehlo {

/// A program parsed into its own single-threaded context. Diagnostics emitted
/// while it is alive go to the caller's callback.
class Program {
public:
  Program(ReussirStablehloBytes diagnostic, void *userData);

  /// Parse and verify `data`, which must define `@main` with
  /// `argumentCount` arguments.
  ReussirStablehloStatus parse(const uint8_t *data, size_t size,
                               size_t argumentCount);

  mlir::MLIRContext &getContext() { return context; }
  mlir::ModuleOp getModule() { return *module; }
  mlir::func::FuncOp getEntry();

  void report(llvm::StringRef message) const;

  /// Pass the module's bytecode to `output`.
  ReussirStablehloStatus write(ReussirStablehloBytes output);

private:
  ReussirStablehloBytes diagnostic;
  void *userData;
  mlir::MLIRContext context;
  mlir::ScopedDiagnosticHandler handler;
  mlir::OwningOpRef<mlir::ModuleOp> module;
};

} // namespace reussir::stablehlo

#endif
