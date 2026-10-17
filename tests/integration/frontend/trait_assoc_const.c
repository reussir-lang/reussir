//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_const.rr: the checksum and the scaled-area sum read
// every associated const through a concrete type and through a generic.

extern long long reussir_assoc_checksum(void);
extern void reussir_assoc_area_sum(double *ret);

int main(void) {
  if (reussir_assoc_checksum() != 4313)
    return 1;
  double area = 0.0;
  reussir_assoc_area_sum(&area);
  return area == 4.0 ? 0 : 2;
}
