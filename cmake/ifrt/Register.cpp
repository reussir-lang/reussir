//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//

#include "mlir/IR/DialectRegistry.h"
#include "mlir/Pass/PassRegistry.h"
#include "shardy/dialect/sdy/ir/dialect.h"
#include "xla/python/ifrt/ir/ifrt_dialect.h"
#include "xla/python/ifrt/ir/transforms/passes.h"
#include "xla/python/ifrt/ir/vifrt_dialect.h"

void registerIfrtDialectsAndPasses(mlir::DialectRegistry &registry) {
  registry.insert<xla::ifrt::IfrtDialect, xla::ifrt::VifrtDialect,
                  mlir::sdy::SdyDialect>();
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtAddCtrlDependenciesPass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtDuplicatedCalleeEliminationPass(); });
  mlir::registerPass([] {
    return xla::ifrt::createIfrtInsertCopyArraysForReturnedManyTimesPass();
  });
  mlir::registerPass([] { return xla::ifrt::createIfrtVerifyDonationPass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtVerifyShardingSpecifiedPass(); });
  mlir::registerPass([] { return xla::ifrt::createIfrtRemoveIfrtAttrsPass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtRemoveAttrsFromOtherDialectsPass(); });
  mlir::registerPass([] { return xla::ifrt::createIfrtLegalizeToVifrtPass(); });
  mlir::registerPass([] { return xla::ifrt::createVifrtLegalizeToIfrtPass(); });
  mlir::registerPass([] { return xla::ifrt::createVifrtToVersionPass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtOutlineAtomProgramToModulePass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtReshardToCopyArraysPass(); });
  mlir::registerPass(
      [] { return xla::ifrt::createIfrtMergeCopiesAndReshardsPass(); });
  mlir::PassPipelineRegistration<>(
      "ifrt-to-outlined-atom-programs-pipeline",
      "Prepare IFRT orchestration with outlined StableHLO atom programs",
      xla::ifrt::createIfrtToOutlinedAtomProgramsPipeline);
}
