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
/// The optional compiler features this build was configured with, declared in
/// cmake/ReussirFeatures.cmake.
///
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_C_FEATURES_H
#define REUSSIR_C_FEATURES_H

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// One optional feature. The strings are static and NUL-terminated.
typedef struct ReussirFeature {
  const char *name;
  const char *description;
  bool enabled;
} ReussirFeature;

// Returns the number of optional features, enabled or not.
size_t reussirFeatureCount(void);

// Returns the feature at `index`, which must be below reussirFeatureCount().
ReussirFeature reussirFeatureGet(size_t index);

#ifdef __cplusplus
}
#endif

#endif // REUSSIR_C_FEATURES_H
