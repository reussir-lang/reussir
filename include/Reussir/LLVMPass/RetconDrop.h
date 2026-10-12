//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#pragma once
#include <llvm/IR/PassManager.h>

namespace reussir::llvmpass {
void lowerRetconDrops(llvm::Module &module);

class RetconDropABIPass : public llvm::PassInfoMixin<RetconDropABIPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};

class RetconDropPass : public llvm::PassInfoMixin<RetconDropPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace reussir::llvmpass
