//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_floormod2exp.rr: six independent properties, one decimal
// digit each. The one and the largest amount it passes are unknown to the
// compiled code, so its reductions run rather than fold.

extern long long reussir_main(unsigned long long one, unsigned long long all);

int main(void) { return reussir_main(1, ~0ull) == 111111 ? 0 : 1; }
