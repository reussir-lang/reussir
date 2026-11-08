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
/// SanitizerCoverage and fuzzing instrumentation C API for the AOT compiler
/// (`rrc`).
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_C_SANCOV_H
#define REUSSIR_C_SANCOV_H

#include "mlir-c/IR.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Types.h"

#ifdef __cplusplus
extern "C" {
#endif

// SanitizerCoverage granularity, mirroring
// `llvm::SanitizerCoverageOptions::Type`.
typedef enum ReussirSanCovType {
  ReussirSanCovNone = 0,
  ReussirSanCovFunction = 1,
  ReussirSanCovBB = 2,
  ReussirSanCovEdge = 3,
} ReussirSanCovType;

// SanitizerCoverage instrumentation options, mirroring
// `llvm::SanitizerCoverageOptions`.
typedef struct ReussirSanCovOptions {
  ReussirSanCovType coverageType;
  int indirectCalls;
  int traceBB;
  int traceCmp;
  int traceDiv;
  int traceGep;
  int use8bitCounters;
  int tracePC;
  int tracePCGuard;
  int inline8bitCounters;
  int inlineBoolFlag;
  int pcTable;
  int noPrune;
  int stackDepth;
  int traceLoads;
  int traceStores;
  int collectControlFlow;
} ReussirSanCovOptions;

void reussirPrepareCoverage(MlirModule module);
int reussirPrepareCoverageLLVM(LLVMModuleRef module,
                               const MlirStringRef *includes,
                               size_t includeCount,
                               const MlirStringRef *excludes,
                               size_t excludeCount, int explicitScopes);

// Stamps fuzzing attributes (`optforfuzzing` and `no-builtin-*` string/memory
// comparison exclusions, matching clang `-fsanitize=fuzzer-no-link`) onto every
// function definition in `module`. Call BEFORE `reussirRunBackendLLVMPipeline`
// so optimization passes preserve comparison structure and call sites for
// value-profile hooks.
void reussirPrepareModuleForFuzzing(LLVMModuleRef module);

// Runs LLVM's `SanitizerCoveragePass` over `module` with `options`, and ensures
// `machine` (when non-null) emits `llvm.global_ctors` into `.init_array` on ELF
// targets that do not use legacy `.ctors`. Call AFTER
// `reussirRunBackendLLVMPipeline`. A no-op when `options` is NULL or
// `options->coverageType == ReussirSanCovNone`.
void reussirRunSanitizerCoveragePass(LLVMModuleRef module,
                                     LLVMTargetMachineRef machine,
                                     const ReussirSanCovOptions *options);

#ifdef __cplusplus
}
#endif

#endif // REUSSIR_C_SANCOV_H
