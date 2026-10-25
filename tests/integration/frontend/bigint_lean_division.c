//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for bigint_lean_division.rr: nine independent properties, one
// decimal digit each. The zero and the one it passes are unknown to the
// compiled code, so its divisions run rather than fold.

extern long long reussir_main(long long zero, long long one);

int main(void) { return reussir_main(0, 1) == 111111111 ? 0 : 1; }
