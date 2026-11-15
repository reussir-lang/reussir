//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for trait_assoc_type.rr: each term of the checksum shifts by an
// amount whose type an impl binds or leaves to the trait's default.

extern long long reussir_assoc_type_checksum(void);

int main(void) { return reussir_assoc_type_checksum() == 107 ? 0 : 1; }
