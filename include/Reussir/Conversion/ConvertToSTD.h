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
/// This header file provides patterns that lower high-level Reussir operations
/// to standard MLIR dialects.
///
//===----------------------------------------------------------------------===//

#pragma once
#ifndef REUSSIR_CONVERSION_CONVERTTOSTD_H
#define REUSSIR_CONVERSION_CONVERTTOSTD_H

#include "Reussir/Conversion/Passes.h"
#include <mlir/Transforms/DialectConversion.h>

namespace reussir {

void populateConvertToSTDConversionPatterns(mlir::RewritePatternSet &patterns);

} // namespace reussir

#endif // REUSSIR_CONVERSION_CONVERTTOSTD_H
