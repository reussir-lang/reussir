#include <assert.h>
#include <stdint.h>
struct MemRef {
  float *allocated, *aligned;
  intptr_t offset, sizes[2], strides[2];
};
extern void _mlir_ciface_reverse(struct MemRef *, struct MemRef *);
extern void _mlir_ciface_small(struct MemRef *, struct MemRef *);
int main(void) {
  float source[15], output[17];
  for (int i = 0; i < 15; ++i)
    source[i] = (float)(i + 1);
  for (int i = 0; i < 17; ++i)
    output[i] = -100;
  struct MemRef input = {source, source, 0, {3, 5}, {5, 1}};
  struct MemRef destination = {output, output, 1, {3, 5}, {5, 1}};
  _mlir_ciface_reverse(&input, &destination);
  for (int i = 0; i < 15; ++i)
    assert(output[i + 1] == source[14 - i]);
  assert(output[0] == -100 && output[16] == -100);
  input.sizes[0] = input.sizes[1] = input.strides[0] = 1;
  destination.sizes[0] = destination.sizes[1] = destination.strides[0] = 1;
  _mlir_ciface_small(&input, &destination);
  assert(output[1] == source[0] + 3);
  assert(output[0] == -100 && output[2] == source[13]);
  return 0;
}
