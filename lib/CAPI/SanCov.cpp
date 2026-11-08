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
/// This file implements the Reussir SanitizerCoverage and fuzzing
/// instrumentation C API.
///
//===----------------------------------------------------------------------===//

#include "Reussir-c/SanCov.h"

#include <llvm-c/Core.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Instrumentation/SanitizerCoverage.h>

void reussirPrepareModuleForFuzzing(LLVMModuleRef module) {
  static constexpr llvm::StringLiteral kNoBuiltinAttrs[] = {
      "no-builtin-bcmp",   "no-builtin-memcmp",      "no-builtin-strcasecmp",
      "no-builtin-strcmp", "no-builtin-strncasecmp", "no-builtin-strncmp",
      "no-builtin-strstr",
  };
  llvm::Module &m = *llvm::unwrap(module);
  for (llvm::Function &f : m) {
    if (f.isDeclaration())
      continue;
    f.addFnAttr(llvm::Attribute::OptForFuzzing);
    for (llvm::StringRef attr : kNoBuiltinAttrs)
      f.addFnAttr(attr);
  }
}

void reussirRunSanitizerCoveragePass(LLVMModuleRef module,
                                     LLVMTargetMachineRef machine,
                                     const ReussirSanCovOptions *options) {
  if (!options || options->coverageType == ReussirSanCovNone)
    return;

  if (auto *tm = reinterpret_cast<llvm::TargetMachine *>(machine))
    tm->Options.UseInitArray = 1;

  llvm::SanitizerCoverageOptions covOpts;
  switch (options->coverageType) {
  case ReussirSanCovFunction:
    covOpts.CoverageType = llvm::SanitizerCoverageOptions::SCK_Function;
    break;
  case ReussirSanCovBB:
    covOpts.CoverageType = llvm::SanitizerCoverageOptions::SCK_BB;
    break;
  case ReussirSanCovEdge:
    covOpts.CoverageType = llvm::SanitizerCoverageOptions::SCK_Edge;
    break;
  case ReussirSanCovNone:
    return;
  }
  covOpts.IndirectCalls = options->indirectCalls != 0;
  covOpts.TraceBB = options->traceBB != 0;
  covOpts.TraceCmp = options->traceCmp != 0;
  covOpts.TraceDiv = options->traceDiv != 0;
  covOpts.TraceGep = options->traceGep != 0;
  covOpts.Use8bitCounters = options->use8bitCounters != 0;
  covOpts.TracePC = options->tracePC != 0;
  covOpts.TracePCGuard = options->tracePCGuard != 0;
  covOpts.Inline8bitCounters = options->inline8bitCounters != 0;
  covOpts.InlineBoolFlag = options->inlineBoolFlag != 0;
  covOpts.PCTable = options->pcTable != 0;
  covOpts.NoPrune = options->noPrune != 0;
  covOpts.StackDepth = options->stackDepth != 0;
  covOpts.TraceLoads = options->traceLoads != 0;
  covOpts.TraceStores = options->traceStores != 0;
  covOpts.CollectControlFlow = options->collectControlFlow != 0;

  llvm::Module &m = *llvm::unwrap(module);
  llvm::PassBuilder pb;
  llvm::LoopAnalysisManager lam;
  llvm::FunctionAnalysisManager fam;
  llvm::CGSCCAnalysisManager cgam;
  llvm::ModuleAnalysisManager mam;
  pb.registerModuleAnalyses(mam);
  pb.registerCGSCCAnalyses(cgam);
  pb.registerFunctionAnalyses(fam);
  pb.registerLoopAnalyses(lam);
  pb.crossRegisterProxies(lam, fam, cgam, mam);

  llvm::ModulePassManager mpm;
  mpm.addPass(llvm::SanitizerCoveragePass(covOpts));
  mpm.run(m, mam);
}
