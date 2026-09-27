#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct AllocationOptions {
  const char *memory_kind;
  size_t memory_kind_size;
  const int64_t *minor_to_major;
  const int64_t *tile_dims;
  const size_t *tile_dim_sizes;
  size_t num_tiles;
};
struct Buffer {
  size_t device;
  int64_t rows;
};
struct Array {
  uint32_t count;
  struct Buffer *buffers[4];
  size_t offset, rows;
};
static const size_t devices[] = {3, 1, 2, 0};
static size_t allocations, releases;
static int bad_input;

void *__reussir_pjrt_array_allocate(size_t device, uint32_t type,
                                    const int64_t *dims, size_t rank,
                                    const struct AllocationOptions *options) {
  // An invalid global extent must be rejected before any backend allocation.
  if (bad_input)
    _Exit(0);
  assert(device == devices[allocations % 4]);
  assert(type == 11 && rank == 2 && dims[1] == 4);
  assert(options && options->memory_kind_size == 6);
  assert(memcmp(options->memory_kind, "device", 6) == 0);
  assert(options->minor_to_major[0] == 0 && options->minor_to_major[1] == 1);
  assert(options->num_tiles == 2);
  assert(options->tile_dim_sizes[0] == 2 && options->tile_dim_sizes[1] == 2);
  const int64_t tiles[] = {8, 128, 2, 1};
  assert(memcmp(options->tile_dims, tiles, sizeof(tiles)) == 0);
  struct Buffer *buffer = malloc(sizeof(*buffer));
  assert(buffer);
  *buffer = (struct Buffer){device, dims[0]};
  ++allocations;
  return buffer;
}
void __reussir_pjrt_array_deallocate(struct Buffer *buffer) {
  assert(buffer->device == devices[releases % 4]);
  ++releases;
  free(buffer);
}
extern struct Array *create(size_t);
extern void retain(struct Array *), release(struct Array *);

int main(int argc, char **argv) {
  if (argc > 1) {
    bad_input = 1;
    create(3);
    return 0;
  }
  struct Array *array = create(12);
  assert(array->count == 1 && array->offset == 0 && array->rows == 12);
  for (size_t i = 0; i < 4; ++i)
    assert(array->buffers[i]->device == devices[i] &&
           array->buffers[i]->rows == 6);
  retain(array);
  release(array);
  assert(releases == 0);
  release(array);
  assert(allocations == 4 && releases == 4);
  array = create(0);
  for (size_t i = 0; i < 4; ++i)
    assert(array->buffers[i]->rows == 0);
  release(array);
  assert(allocations == 8 && releases == 8);
  return 0;
}
