//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_const_default.rr: each digit of the checksum reads an
// associated const that is either overridden or left to its default value.

extern long long reussir_default_checksum(void);
extern unsigned long long reussir_default_concrete(void);

int main(void) {
  if (reussir_default_checksum() != 2010)
    return 1;
  return reussir_default_concrete() == 4 ? 0 : 2;
}
