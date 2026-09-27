// Check the generated runtime ABI and ownership without requiring a device
// plugin. The returned handle is deliberately separate from the RC descriptor.
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

static size_t allocations, releases;
struct Allocation {
  int64_t rows;
};
struct Array {
  uint32_t count;
  struct Allocation *allocation;
  size_t offset;
  size_t rows; // Only the dynamic descriptor carries this field.
};

void *__reussir_pjrt_array_allocate(size_t device, uint32_t type,
                                    const int64_t *dims, size_t rank,
                                    const void *options) {
  assert(options == NULL);
  assert(device == 0 && type == 11 && rank == 2 && dims[1] == 4);
  struct Allocation *allocation = malloc(sizeof(*allocation));
  assert(allocation);
  allocation->rows = dims[0];
  ++allocations;
  return allocation;
}
void __reussir_pjrt_array_deallocate(void *allocation) {
  assert(allocation);
  ++releases;
  free(allocation);
}

extern struct Array *create(size_t rows);
extern void retain(struct Array *), release(struct Array *),
    nested(struct Array *);
extern struct Array *create_atomic(void);
extern void retain_atomic(struct Array *), release_atomic(struct Array *);

int main(void) {
  struct Array *a = create(3);
  assert(a->count == 1 && a->offset == 0 && a->rows == 3);
  assert(a->allocation->rows == 3);
  retain(a);
  release(a);
  assert(a->count == 1 && releases == 0);
  nested(a);
  assert(a->count == 1 && releases == 0);
  release(a);
  assert(releases == 1);

  a = create_atomic();
  assert(a->count == 1 && a->offset == 0 && a->allocation->rows == 2);
  retain_atomic(a);
  release_atomic(a);
  assert(a->count == 1 && releases == 1);
  release_atomic(a);
  assert(releases == 2);

  // An empty logical array still owns its backend handle.
  a = create(0);
  assert(a->rows == 0 && a->allocation->rows == 0);
  release(a);
  assert(allocations == 3 && releases == 3);
  return 0;
}
