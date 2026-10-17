//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for const_seq_e2e.rr: eight independent properties, one decimal digit
// each. With an argument, read past the end of a sequence, which panics.

extern long long reussir_main(void);
extern unsigned short reussir_read_past_end(void);

int main(int argc, char **argv) {
  (void)argv;
  if (argc > 1)
    return reussir_read_past_end();
  return reussir_main() == 11111111 ? 0 : 1;
}
