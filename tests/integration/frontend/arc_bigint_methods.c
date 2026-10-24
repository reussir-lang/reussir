//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for arc_bigint_methods.rr: four independent properties, one decimal
// digit each.

extern long long reussir_main(void);

int main(void) { return reussir_main() == 1111 ? 0 : 1; }
