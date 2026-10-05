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
extern void *__reussir_pjrt_array_from_host(size_t, uint32_t, const int64_t *,
                                           size_t, const void *, const int64_t *,
                                           const void *);
extern void __reussir_pjrt_array_to_host(void *, void *, size_t, const void *);
extern void __reussir_pjrt_array_deallocate(void *);
extern struct Box *sharded(struct Box *);
extern void release(struct Box *);

int main(void) {
  const int64_t dims[] = {4};
  const float input[] = {1, 2, 3, 4, 5, 6, 7, 8};
  struct Box box = {7, {{NULL, NULL}, 0}};
  for (size_t i = 0; i < 2; ++i)
    box.array.shards[i] = __reussir_pjrt_array_from_host(
        1 - i, 11, dims, 1, input + 4 * i, NULL, NULL);
  for (size_t invocation = 0; invocation < 2; ++invocation) {
    struct Box *result = sharded(&box);
    assert(result->count == 1 && result->array.offset == 0);
    for (size_t i = 0; i < 2; ++i) {
      float output[4];
      __reussir_pjrt_array_to_host(result->array.shards[i], output, sizeof(output), NULL);
      for (size_t j = 0; j < 4; ++j)
        assert(output[j] == input[7 - (4 * i + j)]);
    }
    release(result);
  }
  assert(box.count == 7);
  for (size_t i = 0; i < 2; ++i)
    __reussir_pjrt_array_deallocate(box.array.shards[i]);
  puts("Two-device reversal with device order [1, 0] passed");
  return 0;
}
