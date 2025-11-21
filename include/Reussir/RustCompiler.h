#pragma once
#ifndef REUSSIR_RUSTCOMPILER_H
#define REUSSIR_RUSTCOMPILER_H
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <memory>

namespace reussir {
llvm::StringRef findRustCompiler();
llvm::StringRef findRustCompilerDeps();
std::unique_ptr<llvm::Module>
compileRustSource(llvm::LLVMContext &context, llvm::StringRef sourceCode,
                  llvm::ArrayRef<llvm::StringRef> additionalArgs = {});
std::unique_ptr<llvm::MemoryBuffer>
compileRustSourceToBitcode(llvm::LLVMContext &context,
                           llvm::StringRef sourceCode,
                           llvm::ArrayRef<llvm::StringRef> additionalArgs = {});
} // namespace reussir
#endif // REUSSIR_RUSTCOMPILER_H
