#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static void *original;
void remember(void *pointer) { original = pointer; }
void check_storage(void *pointer, int32_t shared) {
  assert(shared ? pointer != original : pointer == original);
}

#ifndef REAL_PJRT
struct HostLayout {
  size_t rank;
  const int64_t *byte_strides, *minor_to_major, *tile_dims;
  const size_t *tile_dim_sizes;
  size_t num_tiles;
};
struct Buffer {
  size_t rank, count;
  float data[];
};
static size_t uploads, downloads, releases;
void *__reussir_pjrt_array_from_host(size_t device, uint32_t type,
                                     const int64_t *dims, size_t rank,
                                     const char *data, const int64_t *strides,
                                     const void *options) {
  assert(device == 0 && type == 11 && (rank == 1 || rank == 2));
  assert(options == NULL && dims && strides);
  size_t count = 1;
  for (size_t i = 0; i < rank; ++i) {
    assert(dims[i] >= 0);
    count *= dims[i];
  }
  struct Buffer *buffer = malloc(sizeof(*buffer) + count * sizeof(float));
  assert(buffer);
  buffer->rank = rank;
  buffer->count = count;
  for (size_t i = 0; i < count; ++i) {
    int64_t offset = rank == 1 ? (int64_t)i * strides[0]
                               : ((int64_t)i / dims[1]) * strides[0] +
                                     ((int64_t)i % dims[1]) * strides[1];
    memcpy(&buffer->data[i], data + offset, sizeof(float));
  }
  ++uploads;
  return buffer;
}
size_t __reussir_pjrt_array_host_size(struct Buffer *buffer,
                                      const struct HostLayout *layout) {
  assert(layout && layout->rank == buffer->rank);
  assert(layout->byte_strides == NULL && layout->tile_dims == NULL &&
         layout->tile_dim_sizes == NULL && layout->num_tiles == 0);
  for (size_t i = 0; i < buffer->rank; ++i)
    assert(layout->minor_to_major[i] == buffer->rank - i - 1);
  return buffer->count * sizeof(float);
}
void __reussir_pjrt_array_to_host(struct Buffer *buffer, void *data,
                                  size_t bytes,
                                  const struct HostLayout *layout) {
  assert(bytes == __reussir_pjrt_array_host_size(buffer, layout));
  memcpy(data, buffer->data, bytes);
  ++downloads;
}
void __reussir_pjrt_array_deallocate(struct Buffer *buffer) {
  free(buffer);
  ++releases;
}
#endif

extern void roundtrip(size_t, bool), strided_roundtrip(void);
int main(void) {
  roundtrip(6, false);
  roundtrip(6, true);
  roundtrip(0, false);
  roundtrip(0, true);
  strided_roundtrip();
#ifndef REAL_PJRT
  assert(uploads == 5 && downloads == 5 && releases == 5);
#endif
  return 0;
}
