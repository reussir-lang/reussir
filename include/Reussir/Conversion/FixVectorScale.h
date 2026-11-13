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
/// This file declares the pass that gives scalable vectors a scale of one.
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_CONVERSION_FIXVECTORSCALE_H
#define REUSSIR_CONVERSION_FIXVECTORSCALE_H

#include <memory>

namespace mlir {
class Pass;
} // namespace mlir

namespace reussir {
std::unique_ptr<mlir::Pass> createReussirFixVectorScalePass();
} // namespace reussir

#endif // REUSSIR_CONVERSION_FIXVECTORSCALE_H
