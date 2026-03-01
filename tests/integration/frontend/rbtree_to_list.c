#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Memory layout for RC<List> (shared enum with Nil / Cons variants).
 *
 * The Reussir runtime allocates an RC box whose first word is the
 * reference count, followed by the variant record:
 *
 *   offset  0:  int64_t  refcount
 *   offset  8:  int64_t  tag         (0 = Nil, 1 = Cons)
 *   offset 16:  int32_t  head        (valid when tag == 1)
 *   offset 20:  (padding, 4 bytes)
 *   offset 24:  void*    tail        (valid when tag == 1, next RC<List>)
 *
 * Total allocation: 32 bytes  (__reussir_allocate(align=8, size=32))
 *
 * The C trampoline returns the RC pointer directly, keeping the
 * structure live (refcount >= 1) so we can safely inspect it here.
 */
typedef struct rc_list {
    int64_t          refcount;
    int64_t          tag;
    int32_t          head;
    int32_t          _pad;
    struct rc_list  *tail;
} rc_list_t;

#define LIST_NIL  0
#define LIST_CONS 1

extern rc_list_t *test_to_list_ffi(void);

int main(void) {
    rc_list_t *list = test_to_list_ffi();

    /* ---- inspect: print the raw RC structure ---- */
    printf("List structure (RC layout):\n");
    rc_list_t *p = list;
    int idx = 0;
    while (p->tag == LIST_CONS) {
        printf("  [%d] @%p  rc=%ld  tag=%ld (Cons)  head=%d  tail=%p\n",
               idx, (void *)p, (long)p->refcount, (long)p->tag,
               p->head, (void *)p->tail);
        p = p->tail;
        idx++;
    }
    printf("  [%d] @%p  rc=%ld  tag=%ld (Nil)\n",
           idx, (void *)p, (long)p->refcount, (long)p->tag);

    /* ---- verify: expect [1, 2, 3, 4, 5, 6, 7, 8, 9] ---- */
    int expected[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    int n = (int)(sizeof(expected) / sizeof(expected[0]));

    p = list;
    for (int i = 0; i < n; i++) {
        if (p->tag != LIST_CONS) {
            fprintf(stderr, "FAIL: expected Cons at index %d, got Nil\n", i);
            abort();
        }
        if (p->head != expected[i]) {
            fprintf(stderr, "FAIL: at index %d: expected %d, got %d\n",
                    i, expected[i], p->head);
            abort();
        }
        p = p->tail;
    }

    if (p->tag != LIST_NIL) {
        fprintf(stderr, "FAIL: expected Nil at end, got tag %ld\n",
                (long)p->tag);
        abort();
    }

    printf("PASS: list = [1, 2, 3, 4, 5, 6, 7, 8, 9]\n");
    return 0;
}
