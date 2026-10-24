//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_floats.rr: three independent properties, one decimal digit
// each; with an argument, truncates a NaN instead.

#include <math.h>

extern long long reussir_main(void);
extern long long truncate(double value);

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1)
    return (int)truncate(NAN);
  return reussir_main() == 111 ? 0 : 1;
}
