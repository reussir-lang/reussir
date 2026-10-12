#ifndef REUSSIR_RUNTIME_DTOR_EXECUTOR_H
#define REUSSIR_RUNTIME_DTOR_EXECUTOR_H
#include <stdbool.h>
#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef struct ReussirDtorExecutor ReussirDtorExecutor;
typedef struct ReussirDtorTask ReussirDtorTask;
typedef struct ReussirDtorStep ReussirDtorStep;
typedef ReussirDtorStep (*ReussirDtorStart)(void *buffer, void *object);
typedef ReussirDtorStep (*ReussirDtorResume)(void *buffer, bool cancel);
typedef struct ReussirDtorRoutine {
  ReussirDtorStart start;
  size_t size;
  size_t align;
} ReussirDtorRoutine;
struct ReussirDtorTask {
  const ReussirDtorRoutine *routine;
  void *object;
};
struct ReussirDtorStep {
  ReussirDtorResume resume;
  const ReussirDtorTask *child;
};
size_t __reussir_dtor_executor_size(void);
size_t __reussir_dtor_executor_align(void);
ReussirDtorExecutor *__reussir_create_dtor_executor(void *storage);
void __reussir_destroy_dtor_executor(ReussirDtorExecutor *context);
void *__reussir_dtor_conti_alloc(ReussirDtorExecutor *context, size_t size,
                                 size_t align);
void __reussir_dtor_conti_free(ReussirDtorExecutor *context);
void __reussir_run_dtor(ReussirDtorExecutor *context, void *object,
                        const ReussirDtorRoutine *routine);
#ifdef __cplusplus
}
#endif
#endif
