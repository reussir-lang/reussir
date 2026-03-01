//===- RuntimeFunctionAttributor.h - Reussir runtime attrib pass *- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This pass applies canonical LLVM function attributes to Reussir runtime
// allocation/deallocation entry points.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <llvm/IR/PassManager.h>

namespace reussir::llvmpass {

class RuntimeFunctionAttributorPass
    : public llvm::PassInfoMixin<RuntimeFunctionAttributorPass> {
public:
  llvm::PreservedAnalyses run(llvm::Module &module,
                              llvm::ModuleAnalysisManager &);
};

} // namespace reussir::llvmpass
