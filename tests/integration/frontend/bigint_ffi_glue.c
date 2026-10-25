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

extern unsigned long long reussir_main(unsigned long long one);

int main(void) { return reussir_main(1) == 720909 ? 0 : 1; }
