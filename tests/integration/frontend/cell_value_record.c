//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for cell_value_record.rr.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t test_variant_ffi(uint64_t);
extern uint64_t test_drop_with_big_ffi(uint64_t);
extern uint64_t test_compound_ffi(uint64_t);
extern uint64_t test_nested_ffi(uint64_t);

#define EXPECT(what, got, want)                                                \
  do {                                                                         \
    unsigned long long g = (got), w = (want);                                  \
    if (g != w) {                                                              \
      fprintf(stderr, "FAIL %s: got %llu want %llu\n", what, g, w);            \
      abort();                                                                 \
    }                                                                          \
  } while (0)

int main(void) {
  /* Repeat to surface use-after-free and double-free in the count paths. */
  for (uint64_t i = 0; i < 3; ++i) {
    EXPECT("variant", test_variant_ffi(10 + i), 3 * (10 + i) + 3);
    EXPECT("drop_with_big", test_drop_with_big_ffi(7 + i), 7 + i);
    EXPECT("compound", test_compound_ffi(4 + i), 11 * (4 + i) + 3);
    EXPECT("nested", test_nested_ffi(2 + i), 31 * (2 + i) + 6);
  }
  return 0;
}
