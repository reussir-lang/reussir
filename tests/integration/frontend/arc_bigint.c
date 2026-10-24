//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for arc_bigint.rr: each test returns 1 when its checks hold.

#include <stdio.h>
#include <stdlib.h>

extern unsigned long long test_arith_ffi(void);
extern unsigned long long test_match_ffi(void);
extern unsigned long long test_ctors_ffi(void);
extern unsigned long long test_records_ffi(void);

static void expect(const char *name, unsigned long long got) {
  if (got != 1) {
    fprintf(stderr, "%s: got %llu, want 1\n", name, got);
    exit(1);
  }
}

int main(void) {
  expect("arith", test_arith_ffi());
  expect("match", test_match_ffi());
  expect("ctors", test_ctors_ffi());
  expect("records", test_records_ffi());
  return 0;
}
