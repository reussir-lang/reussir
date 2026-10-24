//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_division.rr: four independent properties, one decimal
// digit each; with an argument, divides by zero instead.

extern long long reussir_main(void);
extern long long divide_by(unsigned long long divisor);

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1)
    return (int)divide_by(0);
  return reussir_main() == 1111 ? 0 : 1;
}
