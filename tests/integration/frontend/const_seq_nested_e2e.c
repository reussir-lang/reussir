//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for const_seq_nested_e2e.rr: prints the reads of the nested
// constant sequences.

#include <stdint.h>
#include <stdio.h>

extern uint64_t reussir_row_count(void);
extern int64_t reussir_row_sum(uint64_t index);
extern int64_t reussir_layout_code(uint64_t index);
extern uint64_t reussir_forest_sum(void);

int main(void) {
  uint64_t rows = reussir_row_count();
  printf("rows %llu:", (unsigned long long)rows);
  for (uint64_t i = 0; i < rows; ++i)
    printf(" %lld", (long long)reussir_row_sum(i));
  printf("\nlayouts");
  for (uint64_t i = 0; i < 3; ++i)
    printf(" %lld", (long long)reussir_layout_code(i));
  printf("\nforest %llu\n", (unsigned long long)reussir_forest_sum());
  return 0;
}
