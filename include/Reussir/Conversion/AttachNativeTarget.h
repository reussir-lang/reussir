//===-- AttachNativeTarget.h - Attach native target pass --------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_CONVERSION_ATTACHNATIVETARGET_H
#define REUSSIR_CONVERSION_ATTACHNATIVETARGET_H

#include <memory>

namespace mlir {
class Pass;
} // namespace mlir

namespace reussir {
std::unique_ptr<mlir::Pass> createReussirAttachNativeTargetPass();
} // namespace reussir

#endif // REUSSIR_CONVERSION_ATTACHNATIVETARGET_H
