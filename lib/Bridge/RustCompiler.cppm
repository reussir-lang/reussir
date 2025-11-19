module;

#include <array>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/Program.h>

export module Reussir.RustCompiler;

namespace reussir {
namespace {
constexpr std::array<llvm::StringRef, 7> RUSTC_HINTS = {
    "/usr/bin/reussir-rustc",         "/usr/local/bin/reussir-rustc",
    "/opt/reussir/bin/reussir-rustc", "reussir-rustc",
    "build/bin/reussir-rustc",        "bin/reussir-rustc",
    "../bin/reussir-rustc",
};
constexpr std::array<llvm::StringRef, 10> RUSTC_DEPS_HINTS = {
    "/usr/lib/reussir_rt_deps",
    "/usr/local/lib/reussir_rt_deps",
    "/opt/reussir/lib/reussir_rt_deps",
    "reussir_rt_deps",
    "build/lib/reussir_rt_deps",
    "lib/reussir_rt_deps",
    "../lib/reussir_rt_deps",
    "build/bin/reussir_rt_deps",
    "bin/reussir_rt_deps",
    "../bin/reussir_rt_deps"};
} // namespace

export llvm::StringRef findRustCompiler() {
  // first check if RUESSIR_RUSTC is set
  if (const char *env_p = std::getenv("REUSSIR_RUSTC"))
    return env_p;
  // locate reussir-rustc in known paths
  for (const auto &path : RUSTC_HINTS) {
    if (llvm::sys::fs::exists(path))
      return path;
  }

  return "";
}

export llvm::StringRef findRustCompilerDeps() {
  // first check if RUESSIR_RUSTC_DEPS is set
  if (const char *env_p = std::getenv("REUSSIR_RUSTC_DEPS"))
    return env_p;
  for (const auto &path : RUSTC_DEPS_HINTS) {
    if (llvm::sys::fs::exists(path))
      return path;
  }
  return "";
}

export std::unique_ptr<llvm::Module>
compileRustSource(llvm::LLVMContext &context, llvm::StringRef sourceCode,
                  llvm::ArrayRef<llvm::StringRef> additionalArgs = {}) {
  llvm::StringRef rustcPath = findRustCompiler();
  llvm::StringRef rustcDepsPath = findRustCompilerDeps();
  if (rustcPath.empty() || rustcDepsPath.empty()) {
    llvm::errs() << "Could not find reussir-rustc or its dependencies\n";
    return nullptr;
  }
  // Create a temporary file for the source code
  auto srcFile =
      llvm::sys::fs::TempFile::create("reussir_rust_module_%%%%%%.rs");
  auto resultBitcodeFile =
      llvm::sys::fs::TempFile::create("reussir_rust_module_%%%%%%.bc");
  if (!srcFile || !resultBitcodeFile) {
    llvm::errs() << "Could not create temporary files for Rust compilation\n";
    return nullptr;
  }
  llvm::raw_fd_ostream srcStream(srcFile->FD, /*shouldClose=*/true);
  srcStream << sourceCode;
  srcStream.close();
  // Prepare rustc command
  llvm::SmallVector<llvm::StringRef, 16> args = {"reussir-rustc",
                                                 srcFile->TmpName,
                                                 "--crate-type",
                                                 "cdylib",
                                                 "--emit=llvm-bc",
                                                 "-L",
                                                 rustcDepsPath,
                                                 "-o",
                                                 resultBitcodeFile->TmpName};
  for (const auto &arg : additionalArgs)
    args.push_back(arg);
  // Execute rustc
  int code = llvm::sys::ExecuteAndWait(rustcPath, args);
  if (code != 0) {
    llvm::errs() << "Rust compilation failed with exit code " << code << "\n";
    return nullptr;
  }
  // Load the bitcode file into a buffer
  auto bufferOrErr = llvm::MemoryBuffer::getFile(resultBitcodeFile->TmpName);
  if (!bufferOrErr) {
    llvm::errs() << "Failed to read bitcode file: "
                 << bufferOrErr.getError().message() << "\n";
    return nullptr;
  }

  llvm::Expected<std::unique_ptr<llvm::Module>> moduleOrErr =
      llvm::parseBitcodeFile(bufferOrErr.get()->getMemBufferRef(), context);

  if (!moduleOrErr) {
    llvm::errs() << "Failed to parse bitcode file: "
                 << llvm::toString(moduleOrErr.takeError()) << "\n";
    return nullptr;
  }
  return std::move(*moduleOrErr);
}
} // namespace reussir