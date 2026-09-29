#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Array {
  uint32_t count;
  void *handle;
  intptr_t offset, sizes[2];
};
struct MemRef {
  float *allocated, *aligned;
  intptr_t offset, sizes[2], strides[2];
};
extern void _mlir_ciface_download(struct Array *, struct MemRef *);
extern struct Array *_mlir_ciface_upload(struct MemRef *);

static int check_bytes;
size_t __reussir_pjrt_array_host_size(void *buffer, void *layout) {
  // Invalid descriptors must be rejected before consulting the backend.
  if (!check_bytes)
    _Exit(0);
  return 28;
}
void __reussir_pjrt_array_to_host(void *buffer, void *data, size_t bytes,
                                  void *layout) {
  _Exit(0);
}
void *__reussir_pjrt_array_from_host(size_t device, uint32_t dtype,
                                     const int64_t *dims, size_t rank,
                                     const void *data, const int64_t *strides,
                                     void *options) {
  _Exit(0);
}
int main(int argc, char **argv) {
  assert(argc == 2);
  // cf.assert prints before aborting; keep the diagnostic visible through pipes.
  setvbuf(stdout, NULL, _IONBF, 0);
  float data[6] = {0};
  struct Array array = {1, NULL, 0, {2, 3}};
  struct MemRef view = {data, data, 0, {2, 3}, {3, 1}};
  if (strcmp(argv[1], "shape") == 0)
    view.sizes[0] = 3;
  else if (strcmp(argv[1], "stride") == 0)
    view.strides[0] = 4;
  else if (strcmp(argv[1], "overflow") == 0) {
    array.sizes[0] = view.sizes[0] = INTPTR_MAX / 4 + 1;
    array.sizes[1] = view.sizes[1] = 4;
    view.strides[0] = 4;
  } else if (strcmp(argv[1], "negative") == 0)
    array.sizes[0] = view.sizes[0] = -1;
  else if (strcmp(argv[1], "offset") == 0)
    array.offset = 1;
  else if (strcmp(argv[1], "bytes") == 0)
    check_bytes = 1;
  else {
    if (strcmp(argv[1], "upload_stride") == 0)
      view.strides[0] = INTPTR_MAX;
    else {
      assert(strcmp(argv[1], "upload_negative") == 0);
      view.sizes[0] = -1;
    }
    _mlir_ciface_upload(&view);
    return 0;
  }
  _mlir_ciface_download(&array, &view);
  return 0;
}
