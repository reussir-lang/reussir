//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_aggregates.rr: each test returns 1 when its checks hold.

#include <stdio.h>
#include <stdlib.h>

extern unsigned long long test_list_ffi(void);
extern unsigned long long test_tree_ffi(void);
extern unsigned long long test_records_ffi(void);
extern unsigned long long test_closure_ffi(void);
extern unsigned long long test_array_ffi(void);
extern unsigned long long test_cell_ffi(void);

static void expect(const char *name, unsigned long long got) {
  if (got != 1) {
    fprintf(stderr, "%s: got %llu, want 1\n", name, got);
    exit(1);
  }
}

int main(void) {
  expect("list", test_list_ffi());
  expect("tree", test_tree_ffi());
  expect("records", test_records_ffi());
  expect("closure", test_closure_ffi());
  expect("array", test_array_ffi());
  expect("cell", test_cell_ffi());
  return 0;
}
