//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_const_record.rr: the checksum packs one summary of a
// record-valued associated const per impl, the default included.

extern unsigned long long reussir_record_checksum(void);
extern int reussir_record_first_device(void);

int main(void) {
  if (reussir_record_checksum() != 202101421091ULL)
    return 1;
  return reussir_record_first_device() == 3 ? 0 : 2;
}
