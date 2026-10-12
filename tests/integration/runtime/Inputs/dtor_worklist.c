#include "Reussir/Runtime/DtorWorklist.h"
#include <assert.h>
#include <stdint.h>
#include <string.h>
#ifdef _MSC_VER
#include <malloc.h>
#define alloca _alloca
#else
#include <alloca.h>
#endif

static unsigned char *align_storage(unsigned char *raw, size_t align) {
  uintptr_t address = (uintptr_t)raw;
  size_t padding = (align - (address & (align - 1))) & (align - 1);
  return raw + padding;
}

typedef struct {
  size_t id;
  size_t *output;
  size_t *length;
} Event;

static void record(void *object, ReussirDtorWorklist *context) {
  Event *event = (Event *)object;
  (void)context;
  event->output[(*event->length)++] = event->id;
}

typedef struct {
  Event *events;
  size_t count;
} Batch;

static void expand(void *object, ReussirDtorWorklist *context) {
  Batch *batch = (Batch *)object;
  for (size_t i = 0; i < batch->count; ++i)
    __reussir_enqueue_dtor(context, &batch->events[i], record);
}

int main(void) {
  /* The same opaque ABI is exercised from C and C++. Both allocations use the
     linked runtime's layout, with guards around the exact-sized first one. */
  size_t size = __reussir_dtor_worklist_size();
  size_t align = __reussir_dtor_worklist_align();
  assert(size > 0 && align > 1 && (align & (align - 1)) == 0);
  unsigned char *raw = (unsigned char *)alloca(size + align + 2);
  unsigned char *storage = align_storage(raw + 1, align);
  memset(raw, 0xa5, size + align + 2);
  unsigned char *other_raw = (unsigned char *)alloca(size + align - 1);
  ReussirDtorWorklist *work = __reussir_create_dtor_worklist(storage);
  ReussirDtorWorklist *other =
      __reussir_create_dtor_worklist(align_storage(other_raw, align));
  size_t output[1024], length = 0;
  Event events[1024];
  for (size_t i = 0; i < 1024; ++i) {
    events[i].id = i;
    events[i].output = output;
    events[i].length = &length;
  }
  __reussir_enqueue_dtor(other, &events[0], record);
  __reussir_enqueue_dtor(work, &events[0], record);
  Batch batch = {events + 1, 1023};
  __reussir_enqueue_dtor(work, &batch, expand);
  __reussir_drain_dtor_worklist(work);
  assert(length == 1024);
  for (size_t i = 0; i < 1024; ++i)
    assert(output[i] == 1023 - i);
  length = 0;
  __reussir_drain_dtor_worklist(other);
  assert(length == 1 && output[0] == 0);
  __reussir_destroy_dtor_worklist(other);
  __reussir_destroy_dtor_worklist(work);
  assert(storage[-1] == 0xa5 && storage[size] == 0xa5);
  work = __reussir_create_dtor_worklist(storage);
  __reussir_drain_dtor_worklist(work);
  __reussir_destroy_dtor_worklist(work);
  return 0;
}
