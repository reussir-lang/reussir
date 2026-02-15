#include <stdint.h>
#include <stdlib.h>

typedef struct {
  uint64_t f0;
  uint64_t f1;
  uint64_t f2;
  uint64_t f3;
  uint64_t f4;
  uint64_t f5;
  uint64_t f6;
  uint64_t f7;
} HugeValue;

extern HugeValue transform_huge_value_ffi(HugeValue input);

static void assert_huge_value_eq(HugeValue actual, HugeValue expected) {
  if (actual.f0 != expected.f0 || actual.f1 != expected.f1 ||
      actual.f2 != expected.f2 || actual.f3 != expected.f3 ||
      actual.f4 != expected.f4 || actual.f5 != expected.f5 ||
      actual.f6 != expected.f6 || actual.f7 != expected.f7) {
    abort();
  }
}

int main() {
  HugeValue input = {3, 5, 8, 13, 21, 34, 55, 89};
  HugeValue expected = {92, 115, 110, 97, 86, 82, 90, 113};
  HugeValue output = transform_huge_value_ffi(input);
  assert_huge_value_eq(output, expected);

  HugeValue expected_second = {205, 295, 356, 441, 571, 742, 895, 849};
  HugeValue output_second = transform_huge_value_ffi(output);
  assert_huge_value_eq(output_second, expected_second);

  return 0;
}
