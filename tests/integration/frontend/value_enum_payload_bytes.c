//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for value_enum_payload_bytes.rr: nine independent properties, one
// decimal digit each (1 = the property holds).

#include <stdio.h>

extern long long reussir_main(void);

int main(void) {
  long long got = reussir_main();
  if (got != 111111111LL) {
    fprintf(stderr, "got %lld, want 111111111\n", got);
    return 1;
  }
  return 0;
}
