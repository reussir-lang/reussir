//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for closure_wpd_nested_records.rr: (n + 1) + 2n + (n + 3).

#include <stdint.h>

extern uint64_t run_ffi(uint64_t);

int main(void) { return run_ffi(5) == 6 + 10 + 8 ? 0 : 1; }
