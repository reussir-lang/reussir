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
/// This file implements the Reussir feature C API.
///
//===----------------------------------------------------------------------===//

#include "Reussir-c/Features.h"

#include <cassert>
#include <iterator>

namespace {
constexpr ReussirFeature features[] = {
#define REUSSIR_FEATURE(name, enabled, description)                            \
  {#name, description, enabled},
#include "Reussir/Features.def"
#undef REUSSIR_FEATURE
};
} // namespace

size_t reussirFeatureCount(void) { return std::size(features); }

ReussirFeature reussirFeatureGet(size_t index) {
  assert(index < std::size(features) && "feature index out of range");
  return features[index];
}
