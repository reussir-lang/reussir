#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct MemRef {
  float *allocated, *aligned;
  intptr_t offset, sizes[2], strides[2];
};
extern void _mlir_ciface_tiled(struct MemRef *, struct MemRef *);
extern void _mlir_ciface_replicated(struct MemRef *, struct MemRef *);

#ifndef REAL_PJRT
struct Buffer {
  size_t device;
  int64_t dims[2], coordinates[2];
  float values[];
};
static size_t uploads, downloads, releases;
void *__reussir_pjrt_array_from_host_shard(
    size_t device, uint32_t type, const int64_t *global, size_t rank,
    const char *data, const int64_t *strides, const void *options,
    const int64_t *counts, const int64_t *coordinates) {
  const int devices[] = {3, 1, 2, 0};
  size_t position = uploads++ % 4;
  assert(device == (size_t)devices[position] && type == 11 && rank == 2);
  assert(options == NULL && counts[0] == 2);
  assert(coordinates[0] == (int64_t)(position % 2));
  assert(coordinates[1] == (counts[1] == 2 ? (int64_t)(position / 2) : 0));
  int64_t rows = global[0] / counts[0], cols = global[1] / counts[1];
  struct Buffer *buffer = malloc(sizeof(*buffer) + rows * cols * sizeof(float));
  assert(buffer);
  *buffer =
      (struct Buffer){device, {rows, cols}, {coordinates[0], coordinates[1]}};
  for (int64_t i = 0; i < rows; ++i)
    for (int64_t j = 0; j < cols; ++j) {
      intptr_t offset = (i + rows * coordinates[0]) * strides[0] +
                        (j + cols * coordinates[1]) * strides[1];
      memcpy(&buffer->values[i * cols + j], data + offset, sizeof(float));
    }
  return buffer;
}
void __reussir_pjrt_array_to_host_shard(struct Buffer *buffer, float *data,
                                        size_t element_bytes, size_t rank,
                                        const int64_t *global,
                                        const int64_t *counts,
                                        const int64_t *coordinates) {
  assert(element_bytes == sizeof(float) && rank == 2);
  for (int i = 0; i < 2; ++i) {
    assert(buffer->dims[i] == global[i] / counts[i]);
    assert(buffer->coordinates[i] == coordinates[i]);
  }
  for (int64_t i = 0; i < buffer->dims[0]; ++i)
    for (int64_t j = 0; j < buffer->dims[1]; ++j)
      data[(i + buffer->dims[0] * coordinates[0]) * global[1] + j +
           buffer->dims[1] * coordinates[1]] =
          buffer->values[i * buffer->dims[1] + j];
  ++downloads;
}
void __reussir_pjrt_array_deallocate(struct Buffer *buffer) {
  free(buffer);
  ++releases;
}
#endif

int main(void) {
  float source[32], destination[32];
  for (int i = 0; i < 32; ++i)
    source[i] = (float)i;
  for (int replicate = 0; replicate < 2; ++replicate)
    for (int empty = 0; empty < 2; ++empty) {
      for (int i = 0; i < 32; ++i)
        destination[i] = -1;
      // A reversed, strided 4x6 source with a nonzero offset. Destination also
      // has an offset, with sentinel storage on both sides of the logical view.
      struct MemRef input = {source, source, 25, {empty ? 0 : 4, 6}, {-8, 1}};
      struct MemRef output = {
          destination, destination, 3, {empty ? 0 : 4, 6}, {6, 1}};
      if (replicate)
        _mlir_ciface_replicated(&input, &output);
      else
        _mlir_ciface_tiled(&input, &output);
      for (int i = 0; i < 32; ++i) {
        float expected = -1;
        if (!empty && i >= 3 && i < 27)
          expected = source[25 - ((i - 3) / 6) * 8 + (i - 3) % 6];
        assert(destination[i] == expected);
      }
    }
#ifndef REAL_PJRT
  assert(uploads == 16 && downloads == 12 && releases == 16);
#endif
  return 0;
}
