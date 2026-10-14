#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

struct Box {
  uint32_t count;
  struct {
    void *shards[2];
    intptr_t offset;
  } array;
};
struct MemRef {
  float *allocated, *aligned;
  intptr_t offset, size, stride;
};
extern struct Box *_mlir_ciface_upload(struct MemRef *);
extern void _mlir_ciface_download(struct Box *, struct MemRef *);
extern struct Box *sharded(struct Box *);
extern void release(struct Box *);

int main(void) {
  float input[] = {1, 2, 3, 4, 5, 6, 7, 8};
  struct MemRef source = {input, input, 0, 8, 1};
  struct Box *box = _mlir_ciface_upload(&source);
  assert(box->count == 1);
  for (size_t invocation = 0; invocation < 2; ++invocation) {
    struct Box *result = sharded(box);
    assert(result->count == 1 && result->array.offset == 0);
    float output[8];
    struct MemRef destination = {output, output, 0, 8, 1};
    _mlir_ciface_download(result, &destination);
    for (size_t i = 0; i < 8; ++i)
      assert(output[i] == input[7 - i]);
    release(result);
  }
  assert(box->count == 1);
  release(box);
  puts("Two-device reversal with device order [1, 0] passed");
  return 0;
}
