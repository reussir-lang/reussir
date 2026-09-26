//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//

#include <mlir/IR/DialectRegistry.h>

#include "stablehlo/dialect/Register.h"
#include "stablehlo/transforms/Passes.h"
#include "stablehlo/transforms/optimization/Passes.h"

void registerStablehloDialectsAndPasses(mlir::DialectRegistry &registry) {
  mlir::stablehlo::registerAllDialects(registry);
  mlir::stablehlo::registerPasses();
  mlir::stablehlo::registerPassPipelines();
  mlir::stablehlo::registerOptimizationPasses();
}
