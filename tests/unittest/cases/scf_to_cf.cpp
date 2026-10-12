//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir-c/Passes.h"

#include <gtest/gtest.h>
#include <llvm/Support/raw_ostream.h>
#include <mlir/CAPI/Pass.h>
#include <mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Pass/PassManager.h>

#include <memory>
#include <string>

import reussir.test;

namespace reussir {
namespace {

std::unique_ptr<mlir::Pass> createReussirSCFToCFPass() {
  return std::unique_ptr<mlir::Pass>(
      unwrap(reussirCreateSCFToControlFlowPass()));
}

TEST(SCFToCF, PipelineDisablesRollback) {
  // Small functional examples pass in either mode. Guard the configuration
  // responsible for avoiding quadratic rollback storage without a timing test.
  auto pass = createReussirSCFToCFPass();
  std::string pipeline;
  llvm::raw_string_ostream stream(pipeline);
  pass->printAsTextualPipeline(stream);
  EXPECT_NE(pipeline.find("allow-pattern-rollback=false"), std::string::npos);
}

TEST_F(ReussirTest, SCFToCFMatchesRollbackOnChainedAndNestedControlFlow) {
  std::string source = R"mlir(
    module {
      func.func @test(%condition: i1, %selector: index, %x0: i64) -> i64 {
        %zero = arith.constant 0 : index
        %one = arith.constant 1 : index
  )mlir";
  // Every result feeds the next split. Keep the block modest: correctness is
  // tested here; historical large-input memory measurements are not a CI limit.
  for (unsigned i = 0; i < 32; ++i) {
    source += "%x" + std::to_string(i + 1) +
              " = scf.if %condition -> (i64) {\n";
    source += "%value = scf.index_switch %selector -> i64\n"
              "case 0 { scf.yield %x" +
              std::to_string(i) + " : i64 }\n";
    source += "default {\n"
              "%loop = scf.for %iv = %zero to %selector step %one "
              "iter_args(%acc = %x" +
              std::to_string(i) + ") -> (i64) {\n"
              "%next = arith.addi %acc, %acc : i64\n"
              "scf.yield %next : i64\n"
              "}\n"
              "scf.yield %loop : i64\n"
              "}\n"
              "scf.yield %value : i64\n"
              "} else { scf.yield %x" +
              std::to_string(i) + " : i64 }\n";
  }
  source += "return %x32 : i64\n}}";
  auto module = parse(source);
  ASSERT_TRUE(module);
  mlir::OwningOpRef<mlir::ModuleOp> reference(module->clone());

  mlir::PassManager actualPM(context.get());
  actualPM.addPass(createReussirSCFToCFPass());
  ASSERT_TRUE(mlir::succeeded(actualPM.run(*module)));
  ASSERT_TRUE(mlir::succeeded(mlir::verify(*module)));

  mlir::SCFToControlFlowPassOptions options;
  options.allowPatternRollback = true;
  mlir::PassManager referencePM(context.get());
  referencePM.addPass(mlir::createSCFToControlFlowPass(options));
  ASSERT_TRUE(mlir::succeeded(referencePM.run(*reference)));
  ASSERT_TRUE(mlir::succeeded(mlir::verify(*reference)));

  std::string actualText, referenceText;
  llvm::raw_string_ostream actualStream(actualText);
  llvm::raw_string_ostream referenceStream(referenceText);
  module->print(actualStream);
  reference->print(referenceStream);
  EXPECT_EQ(actualText.find("scf."), std::string::npos);
  EXPECT_EQ(actualText, referenceText);
}

} // namespace
} // namespace reussir
