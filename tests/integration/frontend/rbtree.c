#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct rc_list {
    int64_t refcount;
    int64_t tag;
    int32_t head;
    int32_t _pad;
    struct rc_list *tail;
} rc_list_t;

#define LIST_NIL 0
#define LIST_CONS 1

extern rc_list_t *make_tree_to_list_ffi(int32_t size);

int main(void) {
    const int32_t n = 256;
    rc_list_t *p = make_tree_to_list_ffi(n);

    for (int32_t i = 0; i < n; ++i) {
        if (p->tag != LIST_CONS) {
            fprintf(stderr, "FAIL: expected Cons at index %d, got Nil\n", (int)i);
            abort();
        }
        if (p->head != i) {
            fprintf(stderr, "FAIL: expected %d at index %d, got %d\n", (int)i, (int)i, (int)p->head);
            abort();
        }
        p = p->tail;
    }

    if (p->tag != LIST_NIL) {
        fprintf(stderr, "FAIL: expected Nil terminator, got tag=%ld\n", (long)p->tag);
        abort();
    }

    puts("PASS: make_tree_to_list_ffi(256) returned [0..255]");
    return 0;
}
