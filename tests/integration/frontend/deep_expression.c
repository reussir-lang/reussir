//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for deep_expression.rr (program from Inputs/deep_expression.py).

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t deep_expression_check(uint64_t z);

int main(void) {
  uint64_t result = deep_expression_check(1);
  if (result != 32004007) {
    fprintf(stderr, "FAIL: expected 32004007, got %llu\n",
            (unsigned long long)result);
    abort();
  }
  return 0;
}
