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
void populateRetconDropPasses(llvm::ModulePassManager &passes);
void lowerRetconDrops(llvm::Module &module);

class RetconDropABIPass : public llvm::PassInfoMixin<RetconDropABIPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
/// Replace free-only continuation callbacks after ordinary LLVM optimization
/// has exposed their frame loads and completion returns.
class RetconDropDeferredFreePass
    : public llvm::PassInfoMixin<RetconDropDeferredFreePass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &, llvm::ModuleAnalysisManager &);
};
} // namespace reussir::llvmpass
