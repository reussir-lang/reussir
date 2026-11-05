//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_ffi_glue.rr: the lengths and the marker its textures
// return, one per pair of decimal digits. The one it passes is unknown to the
// compiled code, so its heap integers are built at run time.

#include <stdint.h>

extern void __reussir_rcgmp_ffi_acquire(void *value);
extern void __reussir_rcgmp_ffi_release(void *value);
extern uint8_t __reussir_rcgmp_ffi_is_unique(void *value);
extern void *rcgmp_mpz_from_word(uintptr_t magnitude, uint32_t negative,
                                 uint32_t atomic);

extern unsigned long long reussir_main(unsigned long long one);

int main(void) {
  void *value = rcgmp_mpz_from_word(UINTPTR_MAX, 0, 0);
  if (!__reussir_rcgmp_ffi_is_unique(value))
    return 2;
  __reussir_rcgmp_ffi_acquire(value);
  if (__reussir_rcgmp_ffi_is_unique(value))
    return 3;
  __reussir_rcgmp_ffi_release(value);
  if (!__reussir_rcgmp_ffi_is_unique(value))
    return 4;
  __reussir_rcgmp_ffi_release(value);
  return reussir_main(1) == 720909 ? 0 : 1;
}
