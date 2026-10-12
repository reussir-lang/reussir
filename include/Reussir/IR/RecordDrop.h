//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_IR_RECORDDROP_H
#define REUSSIR_IR_RECORDDROP_H

#include "Reussir/IR/ReussirTypes.h"
#include <optional>

namespace reussir {
enum class RecordDropKind { Direct, TailLoop, Worklist };

std::optional<RecordDropKind> classifyRecordDrop(RecordType record);
} // namespace reussir

#endif
