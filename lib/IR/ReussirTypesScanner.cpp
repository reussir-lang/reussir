//===-- ReussirTypesScanner.cpp - Scanner instructions for types -*- c++
//-*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements scanner instruction emission for Reussir types.
//
//===----------------------------------------------------------------------===//

#include <cassert>
#include <cstdint>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/TypeSize.h>
#include <mlir/Dialect/LLVMIR/LLVMTypes.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <optional>

#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {
void RecordType::emitScannerInstructions(llvm::SmallVectorImpl<int32_t> &buffer,
                                         const mlir::DataLayout &dataLayout,
                                         std::optional<size_t> syncSize) const {
  // TODO: Implement scanner instructions emission
  using namespace scanner;
  if (isCompound()) {
    size_t currentFieldPosition = 0;
    size_t cursorPosition = 0;
    for (auto [rawMember, cap] :
         llvm::zip(getMembers(), getMemberCapabilities())) {
      if (!rawMember)
        continue;
      mlir::Type member;
      if (cap == Capability::flex || cap == Capability::rigid ||
          cap == Capability::shared || cap == Capability::field)
        member = mlir::LLVM::LLVMPointerType::get(getContext());
      else
        member = rawMember;
      // advance to the next field
      llvm::TypeSize memberSize = dataLayout.getTypeSize(member);
      if (!memberSize.isFixed())
        llvm_unreachable("RecordType must have a fixed size");
      uint64_t memberSizeInBytes = memberSize.getFixedValue();
      uint64_t memberAlignment = dataLayout.getTypeABIAlignment(member);
      currentFieldPosition =
          llvm::alignTo(currentFieldPosition, memberAlignment);
      if (cap == Capability::field) {
        if (cursorPosition < currentFieldPosition) {
          buffer.push_back(advance(currentFieldPosition - cursorPosition));
          cursorPosition = currentFieldPosition;
        }
        buffer.push_back(field());
      }
      RecordType nestedRecordType = llvm::dyn_cast<RecordType>(rawMember);

      if (cap == Capability::value && nestedRecordType &&
          !nestedRecordType.hasNoRegionalFields()) {
        // nested record can be arbitrarily complicated.
        // To avoid further complexity, we always first align current cursor
        // to a current field position and also require the nested record
        // to ends at a known boundary. The nested routine always ends in a
        // single trailing instruction. All other ending parts jumps to that
        // instruction.
        if (cursorPosition < currentFieldPosition) {
          buffer.push_back(advance(currentFieldPosition - cursorPosition));
          cursorPosition = currentFieldPosition;
        }
        nestedRecordType.emitScannerInstructions(buffer, dataLayout,
                                                 memberSizeInBytes);
        buffer.pop_back();
        cursorPosition += memberSizeInBytes;
      }
      currentFieldPosition += memberSizeInBytes;
    }
    if (syncSize && *syncSize > cursorPosition)
      buffer.push_back(advance(*syncSize - cursorPosition));
    buffer.push_back(end());
    return;
  }

  // Variant record
  auto indexTy = mlir::IndexType::get(getContext());
  buffer.push_back(variant());
  buffer.push_back(advance(dataLayout.getTypeSize(indexTy)));
}

} // namespace reussir