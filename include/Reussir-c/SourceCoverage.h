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
/// Source-based code coverage C API for the AOT compiler (`rrc`).
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_C_SOURCECOVERAGE_H
#define REUSSIR_C_SOURCECOVERAGE_H

#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

// Runs LLVM's `InstrProfilingLoweringPass` over `module`. The pass changes the
// `llvm.instrprof.*` intrinsics into counter updates and adds the profile data
// that the profile runtime writes. Call AFTER `reussirRunBackendLLVMPipeline`.
void reussirRunInstrProfilingLoweringPass(LLVMModuleRef module);

#ifdef __cplusplus
}
#endif

#endif // REUSSIR_C_SOURCECOVERAGE_H
