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

namespace reussir {
enum class RecordDropKind { Invalid, Direct, TailLoop, Worklist };
} // namespace reussir

#endif
