//===----------------------------------------------------------------------===//
// Part of the Reussir Project, dual licensed under Apache-2.0 or MIT.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_RUNTIME_DTOR_WORKLIST_H
#define REUSSIR_RUNTIME_DTOR_WORKLIST_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ReussirDtorWorklist ReussirDtorWorklist;
typedef void (*ReussirDtorCallback)(void *object, ReussirDtorWorklist *context);

/* Context layout is private to the runtime. Query both values before reserving
 * stack storage; no SmallVec or Rust layout is part of the C ABI. */
size_t __reussir_dtor_worklist_size(void);
size_t __reussir_dtor_worklist_align(void);

/* Placement-initialize caller-owned storage without allocating; return the
 * opaque context at that address. The caller must provide non-null storage with
 * at least the queried size and alignment, keep it alive until destroy returns,
 * and not overwrite a live context. These preconditions are not checked. */
ReussirDtorWorklist *__reussir_create_dtor_worklist(void *storage);

/* Transfer one piece of work to context. Does not invoke callback. object is an
 * opaque pointer (possibly null); callback must be non-null and return
 * normally. Callbacks may enqueue more work on the same context, but must not
 * recursively drain/destroy it, retain it after returning, or access it
 * concurrently. */
void __reussir_enqueue_dtor(ReussirDtorWorklist *context, void *object,
                            ReussirDtorCallback callback);

/* The outer owner executes callbacks last-in first-out until empty, including
 * newly enqueued work. Pop happens before invocation. A callback can enqueue
 * its continuation before children so the children complete before it resumes.
 */
void __reussir_drain_dtor_worklist(ReussirDtorWorklist *context);

/* Require an empty, non-draining context; release its overflow storage. Does
 * not invoke callbacks. The storage may then leave scope or be initialized
 * again. */
void __reussir_destroy_dtor_worklist(ReussirDtorWorklist *context);

#ifdef __cplusplus
}
#endif
#endif
