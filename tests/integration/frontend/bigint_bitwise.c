//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_bitwise.rr: four independent properties, one decimal
// digit each; with an argument, shifts by a negative amount instead.

extern long long reussir_main(void);
extern long long shift_by(long long amount);

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1)
    return (int)shift_by(-1);
  return reussir_main() == 1111 ? 0 : 1;
}
