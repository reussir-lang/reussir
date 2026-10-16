#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
struct Buffer {
  int64_t dims[3];
};
struct Box {
  uint32_t count;
  struct Buffer *handles[4];
  intptr_t offset, extents[2];
};
static int64_t shape[] = {3, 2, 7};
static int unequal;
static size_t released;
struct LaunchRequest {
  const void *code;
  size_t code_size;
  const void *checksum;
  const void *options;
  size_t options_size;
  const void *arguments;
  size_t input_count;
  const void *inputs;
  const int32_t *devices;
  size_t device_count;
  const void *permissions, *mapping, *donors;
  size_t donor_count;
  const void *aliases;
  size_t alias_count;
  void ***outputs;
  size_t output_count;
};
void __reussir_pjrt_launch(const struct LaunchRequest *r) {
  const int32_t expected[] = {3, 1, 2, 0};
  assert(r->code && r->code_size && r->checksum && r->options &&
         r->options_size);
  assert(r->device_count == 4 &&
         memcmp(r->devices, expected, sizeof(expected)) == 0);
  assert(r->input_count == 0 && r->output_count == 1 && r->donor_count == 0);
  for (size_t i = 0; i < 4; ++i) {
    struct Buffer *buffer = malloc(sizeof(*buffer));
    assert(buffer);
    memcpy(buffer->dims, shape, sizeof(shape));
    if (unequal && i == 3)
      ++buffer->dims[0];
    r->outputs[0][i] = buffer;
  }
}
void __reussir_pjrt_array_dimensions(struct Buffer *buffer, int64_t *dims,
                                     size_t rank) {
  assert(rank == 3);
  memcpy(dims, buffer->dims, sizeof(buffer->dims));
}
void __reussir_pjrt_array_deallocate(struct Buffer *buffer) {
  free(buffer);
  ++released;
}
extern struct Box *adopt(intptr_t, intptr_t);
extern void release(struct Box *);
int main(int argc, char **argv) {
  setvbuf(stdout, NULL, _IONBF, 0);
  if (argc > 1) {
    intptr_t rows = 5;
    if (strcmp(argv[1], "negative") == 0)
      rows = -1;
    else if (strcmp(argv[1], "dynamic") == 0)
      rows = 7;
    else if (strcmp(argv[1], "static") == 0)
      shape[1] = 3;
    else {
      assert(strcmp(argv[1], "unequal") == 0);
      unequal = 1;
    }
    release(adopt(rows, 7));
    return 0;
  }
  // Both logical sizes have physical extent three; the supplied size must be
  // retained, not reconstructed from the padded buffers as six in both cases.
  for (intptr_t rows = 5; rows <= 6; ++rows) {
    struct Box *box = adopt(rows, 7);
    assert(box->count == 1 && box->offset == 0);
    assert(box->extents[0] == rows && box->extents[1] == 7);
    release(box);
  }
  shape[0] = shape[2] = 0;
  struct Box *empty = adopt(0, 0);
  assert(empty->extents[0] == 0 && empty->extents[1] == 0);
  release(empty);
  assert(released == 12);
  return 0;
}
