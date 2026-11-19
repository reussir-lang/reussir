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
/// This file declares helpers for locating and invoking the Rust compiler.
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_RUSTCOMPILER_H
#define REUSSIR_RUSTCOMPILER_H
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <memory>
#include <string>

struct ReussirPolyffiCache;

namespace reussir {
struct PolyffiCacheCloser {
  void operator()(ReussirPolyffiCache *cache) const;
};
/// The polyffi bitcode store; empty when it cannot be opened.
using PolyffiCache = std::unique_ptr<ReussirPolyffiCache, PolyffiCacheCloser>;
/// Open the store. A failure is reported on stderr.
PolyffiCache openPolyffiCache();
/// Locate the `rustc` used to compile FFI textures. A non-empty `preferred`
/// path (an explicit `--polyffi-rust-path` threaded down from the driver) is
/// returned verbatim; otherwise the `REUSSIR_RUSTC` environment variable and
/// then a list of conventional locations are probed. Empty when nothing is
/// found.
llvm::StringRef findRustCompiler(llvm::StringRef preferred = {});
/// Locate the directories holding the Rust packages (`libreussir_rt` and
/// friends) FFI textures link against (one `rustc -L` each). Non-empty
/// `preferred` directories (explicit `--polyffi-libdir` flags) are returned
/// verbatim; otherwise the `REUSSIR_RUSTC_DEPS` environment variable (which
/// may list several directories separated by the platform's environment path
/// separator) and then conventional locations are probed. Empty when nothing
/// is found.
llvm::SmallVector<std::string>
findRustCompilerDeps(llvm::ArrayRef<llvm::StringRef> preferred = {});
std::unique_ptr<llvm::Module>
compileRustSource(llvm::LLVMContext &context, llvm::StringRef sourceCode,
                  llvm::ArrayRef<llvm::StringRef> additionalArgs = {},
                  llvm::StringRef rustcPath = {},
                  llvm::ArrayRef<llvm::StringRef> rustcDepsDirs = {});
/// Compile a texture to bitcode through `cache`, directly when it is empty.
/// Without `cache`, a store is opened for this call.
std::unique_ptr<llvm::MemoryBuffer>
compileRustSourceToBitcode(llvm::LLVMContext &context,
                           llvm::StringRef sourceCode,
                           llvm::ArrayRef<llvm::StringRef> additionalArgs = {},
                           llvm::StringRef rustcPath = {},
                           llvm::ArrayRef<llvm::StringRef> rustcDepsDirs = {},
                           const PolyffiCache *cache = nullptr);
} // namespace reussir
#endif // REUSSIR_RUSTCOMPILER_H
