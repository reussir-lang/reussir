//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_type_impl_self.rr: 14 * 3 + 5 + 9.

extern long long reussir_impl_self_checksum(void);

int main(void) { return reussir_impl_self_checksum() == 56 ? 0 : 1; }
