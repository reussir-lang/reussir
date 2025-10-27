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
#ifndef REUSSIR_BRIDGE_H
#define REUSSIR_BRIDGE_H

#include <stddef.h>

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

// currently, we only support compiling for native machine target
void reussir_bridge_compile_for_native_machine(const char *mlir_module,
                                               const char *source_name,
                                               const char *output_file,
                                               ReussirOutputTarget target,
                                               ReussirOptOption opt,
                                               ReussirLogLevel log_level);

#ifdef __cplusplus
}
#endif

#endif // REUSSIR_BRIDGE_H
