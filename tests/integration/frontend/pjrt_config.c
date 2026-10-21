//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for pjrt_config.rr: each code packs one marker's placement or
// launch settings, overridden values and defaults alike.

#include <stdint.h>

extern int64_t reussir_pjrt_mesh_placement(void);
extern int64_t reussir_pjrt_pinned_placement(void);
extern int64_t reussir_pjrt_plain_placement(void);
extern int64_t reussir_pjrt_mesh_launch(void);
extern int64_t reussir_pjrt_plain_launch(void);
extern int32_t reussir_pjrt_mesh_first_device(void);

int main(void) {
  if (reussir_pjrt_mesh_placement() != 43040242)
    return 1;
  if (reussir_pjrt_pinned_placement() != 2000)
    return 2;
  if (reussir_pjrt_plain_placement() != 0)
    return 3;
  if (reussir_pjrt_mesh_launch() != 301)
    return 4;
  if (reussir_pjrt_plain_launch() != 10)
    return 5;
  return reussir_pjrt_mesh_first_device() == 3 ? 0 : 6;
}
