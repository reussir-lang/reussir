//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for driver_stack_address_limit.rr.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t driver_stack_address_limit(uint64_t x);

int main(void) {
  uint64_t result = driver_stack_address_limit(7);
  if (result != 42) {
    fprintf(stderr, "FAIL: expected 42, got %llu\n", (unsigned long long)result);
    abort();
  }
  return 0;
}
