//===-- Bridge.h - Reussir backend bridge -----------------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the bridge between rust frontend and C++ backend.
//
//===----------------------------------------------------------------------===//

#pragma once
#include <cstddef>
#ifndef REUSSIR_BRIDGE_H
#define REUSSIR_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// C-style enums for FFI compatibility
typedef enum ReussirOutputTarget {
  REUSSIR_OUTPUT_LLVMIR = 0,
  REUSSIR_OUTPUT_ASM = 1,
  REUSSIR_OUTPUT_OBJECT = 2
} ReussirOutputTarget;

typedef enum ReussirOptOption {
  REUSSIR_OPT_NONE = 0,
  REUSSIR_OPT_DEFAULT = 1,
  REUSSIR_OPT_AGGRESSIVE = 2,
  REUSSIR_OPT_SIZE = 3,
  REUSSIR_OPT_TPDE = 4
} ReussirOptOption;

typedef enum ReussirLogLevel {
  REUSSIR_LOG_ERROR = 0,
  REUSSIR_LOG_WARNING = 1,
  REUSSIR_LOG_INFO = 2,
  REUSSIR_LOG_DEBUG = 3,
  REUSSIR_LOG_TRACE = 4
} ReussirLogLevel;

typedef enum ReussirCodeModel {
  REUSSIR_CODE_MODEL_TINY = 0,
  REUSSIR_CODE_MODEL_SMALL = 1,
  REUSSIR_CODE_MODEL_KERNEL = 2,
  REUSSIR_CODE_MODEL_MEDIUM = 3,
  REUSSIR_CODE_MODEL_LARGE = 4,
  REUSSIR_CODE_MODEL_DEFAULT = 5
} ReussirCodeModel;

typedef enum ReussirRelocationModel {
  REUSSIR_RELOC_MODEL_STATIC = 0,
  REUSSIR_RELOC_MODEL_PIC = 1,
  REUSSIR_RELOC_MODEL_DYNAMIC = 2,
  REUSSIR_RELOC_MODEL_ROPI = 3,
  REUSSIR_RELOC_MODEL_RWPI = 4,
  REUSSIR_RELOC_MODEL_ROPI_RWPI = 5,
  REUSSIR_RELOC_MODEL_DEFAULT = 6
} ReussirRelocationModel;

// Query if TPDE support is compiled in
int reussir_bridge_has_tpde(void);

// Get default target triple (caller must free with free())
char *reussir_bridge_get_default_target_triple(void);

// Get default target CPU (caller must free with free())
char *reussir_bridge_get_default_target_cpu(void);

// Get the features string
char *reussir_bridge_get_default_target_features(void);

// Compile for a specific target
void reussir_bridge_compile_for_target(
    const char *mlir_module, const char *source_name, const char *output_file,
    ReussirOutputTarget target, ReussirOptOption opt, ReussirLogLevel log_level,
    const char *target_triple, const char *target_cpu,
    const char *target_features, ReussirCodeModel code_model,
    ReussirRelocationModel reloc_model);

// An opaque stable pointer for AST.
typedef void *ASTStablePtr;
// A callback function that returns the MLIR IR of the AST.
// This callback took away the ownership of the ASTStablePtr.
typedef const char *(*ASTCallbackFn)(ASTStablePtr);
// A callback function that frees the ASTStablePtr. This is used when the AST
// unit is never materialized.
typedef void (*ASTFreeFn)(ASTStablePtr);
// An opaque handle for the JIT engine.
typedef void *ReussirJIT;

ReussirJIT reussir_bridge_jit_create(ASTCallbackFn callback,
                                     ReussirOptOption opt);
void reussir_bridge_jit_destroy(ReussirJIT jit);
bool reussir_bridge_jit_add_symbols(ReussirJIT jit, ASTStablePtr ast,
                                    const char *symbol_names[],
                                    uint8_t symbol_flags[],
                                    size_t symbol_count);
void *reussir_bridge_jit_lookup_symbol(ReussirJIT jit, const char *symbol_name);

#ifdef __cplusplus
}
#endif

#endif // REUSSIR_BRIDGE_H
