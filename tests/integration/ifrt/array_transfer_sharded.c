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
    size_t element_bytes, const int64_t *counts, const int64_t *coordinates) {
  const int devices[] = {3, 1, 2, 0};
  size_t position = uploads++ % 4;
  assert(device == (size_t)devices[position] && type == 11 && rank == 2);
  assert(options == NULL && counts[0] == 2 && element_bytes == sizeof(float));
  assert(coordinates[0] == (int64_t)(position % 2));
  assert(coordinates[1] == (counts[1] == 2 ? (int64_t)(position / 2) : 0));
  int64_t rows = global[0] / counts[0] + (global[0] % counts[0] != 0);
  int64_t cols = global[1] / counts[1] + (global[1] % counts[1] != 0);
  struct Buffer *buffer =
      calloc(1, sizeof(*buffer) + rows * cols * sizeof(float));
  assert(buffer);
  *buffer =
      (struct Buffer){device, {rows, cols}, {coordinates[0], coordinates[1]}};
  for (int64_t i = 0; i < rows; ++i)
    for (int64_t j = 0; j < cols; ++j) {
      if (i + rows * coordinates[0] >= global[0] ||
          j + cols * coordinates[1] >= global[1])
        continue;
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
    assert(buffer->dims[i] ==
           global[i] / counts[i] + (global[i] % counts[i] != 0));
    assert(buffer->coordinates[i] == coordinates[i]);
  }
  for (int64_t i = 0; i < buffer->dims[0]; ++i)
    for (int64_t j = 0; j < buffer->dims[1]; ++j) {
      if (i + buffer->dims[0] * coordinates[0] >= global[0] ||
          j + buffer->dims[1] * coordinates[1] >= global[1])
        continue;
      data[(i + buffer->dims[0] * coordinates[0]) * global[1] + j +
           buffer->dims[1] * coordinates[1]] =
          buffer->values[i * buffer->dims[1] + j];
    }
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
  const intptr_t shapes[][2] = {{4, 6}, {3, 5}, {1, 1}, {0, 5}, {3, 0}};
  for (int replicate = 0; replicate < 2; ++replicate)
    for (size_t shape = 0; shape < 5; ++shape) {
      intptr_t rows = shapes[shape][0], cols = shapes[shape][1];
      for (int i = 0; i < 32; ++i)
        destination[i] = -1;
      intptr_t offset = 1 + (rows ? rows - 1 : 0) * 8 + (cols ? cols - 1 : 0);
      struct MemRef input = {source, source, offset, {rows, cols}, {-8, -1}};
      struct MemRef output = {
          destination, destination, 3, {rows, cols}, {cols, 1}};
      if (replicate)
        _mlir_ciface_replicated(&input, &output);
      else
        _mlir_ciface_tiled(&input, &output);
      for (int i = 0; i < 32; ++i) {
        float expected = -1;
        if (i >= 3 && i < 3 + rows * cols)
          expected = source[offset - ((i - 3) / cols) * 8 - (i - 3) % cols];
        assert(destination[i] == expected);
      }
    }
#ifndef REAL_PJRT
  assert(uploads == 40 && downloads == 30 && releases == 40);
#endif
  return 0;
}
