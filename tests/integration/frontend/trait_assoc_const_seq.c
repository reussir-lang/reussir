//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_const_seq.rr: prints every read of the associated
// const. With an argument, read past the end of `Wide::TEST`, which panics.

#include <stdint.h>
#include <stdio.h>

extern uint64_t reussir_count_wide(void);
extern uint64_t reussir_count_empty(void);
extern int64_t reussir_sum_wide(void);
extern int64_t reussir_sum_empty(void);
extern int32_t reussir_nth_wide(uint64_t index);
extern int32_t reussir_last(void);
extern int32_t reussir_stored(void);

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1)
    return reussir_nth_wide(5);
  printf("count %llu %llu\n", (unsigned long long)reussir_count_wide(),
         (unsigned long long)reussir_count_empty());
  printf("sum %lld %lld\n", (long long)reussir_sum_wide(),
         (long long)reussir_sum_empty());
  printf("elems");
  for (uint64_t i = 0; i < reussir_count_wide(); ++i)
    printf(" %d", (int)reussir_nth_wide(i));
  printf("\nlast %d\n", (int)reussir_last());
  printf("stored %d\n", (int)reussir_stored());
  return 0;
}
