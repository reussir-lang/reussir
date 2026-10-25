//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_match.rr: nine independent properties, one decimal digit
// each, and four more of the arms' ownership.

extern long long reussir_main(void);

extern long long gap_of(long long x);
extern long long owning(void);

int main(void) {
  // gap_of reads its argument at run time, so no arm is folded away.
  static const long long expect[] = {2, 2, 0, 0, 0, 1, 1};
  for (long long x = -4; x <= 2; ++x)
    if (gap_of(x) != expect[x + 4])
      return 1;
  if (owning() != 1111)
    return 1;
  return reussir_main() == 111111111 ? 0 : 1;
}
