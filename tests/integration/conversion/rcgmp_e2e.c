#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

bool is_factorial_of_25(int64_t n);
int64_t distance(int64_t a, int64_t b);

static int failures = 0;

static void expect(bool ok, const char *what) {
  if (!ok) {
    fprintf(stderr, "FAILED: %s\n", what);
    ++failures;
  }
}

int main(void) {
  expect(is_factorial_of_25(25), "25! matches its heap constant");
  expect(!is_factorial_of_25(24), "24! differs from 25!");
  expect(distance(3, 10) == 7, "immediate distance");
  // 2^64 - 1 needs a heap integer; its low 64 bits are all ones.
  expect(distance(INT64_MIN, INT64_MAX) == -1, "heap distance truncates");
  return failures;
}
