//===-- TypeConverter.cpp - Reussir type converter impl ---------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/TypeConverter.h"
#include "Reussir/IR/ReussirTypes.h"
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>
#include <mlir/IR/BuiltinTypes.h>



namespace reussir {
namespace {

mlir::LowerToLLVMOptions getLowerOptions(mlir::ModuleOp op) {

  llvm::StringRef dataLayoutString;
  auto dataLayoutAttr = op->template getAttrOfType<mlir::StringAttr>(
      mlir::LLVM::LLVMDialect::getDataLayoutAttrName());
  if (dataLayoutAttr)
    dataLayoutString = dataLayoutAttr.getValue();

  auto options = mlir::LowerToLLVMOptions(op.getContext());
  auto llvmDL = llvm::DataLayout(dataLayoutString);
  // FIXME: Should translateDataLayout in the MLIR layer be doing this?
  if (llvmDL.getPointerSizeInBits(0) == 32)
    options.overrideIndexBitwidth(32);

  options.dataLayout = llvmDL;
  return options;
}

class PopGuard {
  llvm::SmallVectorImpl<mlir::Type> *callStack;

public:
  PopGuard() : callStack(nullptr) {}

  void install(llvm::SmallVectorImpl<mlir::Type> &callStack) {
    this->callStack = &callStack;
  }
  ~PopGuard() {
    if (callStack)
      callStack->pop_back();
  }
};
}; // end anonymous namespace

/// Get the target triple attribute name used in MLIR modules.
static llvm::StringRef getTargetTripleAttrName() {
  return "llvm.target_triple";
}

/// Get the target triple from the module, or infer from data layout.
static llvm::Triple getTargetTriple(mlir::ModuleOp op) {
  // First, try to get explicit target triple attribute
  if (auto tripleAttr = op->getAttrOfType<mlir::StringAttr>(
          getTargetTripleAttrName())) {
    return llvm::Triple(tripleAttr.getValue());
  }

  // Fallback: try to infer from data layout string
  // This is a heuristic approach
  if (auto dataLayoutAttr = op->getAttrOfType<mlir::StringAttr>(
          mlir::LLVM::LLVMDialect::getDataLayoutAttrName())) {
    llvm::StringRef dlStr = dataLayoutAttr.getValue();

    // Windows data layouts typically contain specific patterns
    // e.g., "-m:w-" for Windows COFF or "-m:x-" for MinGW
    if (dlStr.contains("-m:w-") || dlStr.contains("-m:x-")) {
      // Likely Windows target
      if (dlStr.contains("e-m:") && dlStr.contains("i64:64")) {
        // 64-bit Windows
        return llvm::Triple("x86_64-pc-windows-msvc");
      }
    }

    // macOS/iOS patterns
    if (dlStr.contains("-m:o-")) {
      // Mach-O, likely Darwin
      if (dlStr.contains("i64:64") && !dlStr.contains("p:32:32")) {
        return llvm::Triple("arm64-apple-macosx");
      }
    }

    // Default to a reasonable x86_64 Linux triple
    return llvm::Triple("x86_64-unknown-linux-gnu");
  }

  // Ultimate fallback: use host triple
  return llvm::Triple(llvm::sys::getDefaultTargetTriple());
}

LLVMTypeConverter::LLVMTypeConverter(mlir::ModuleOp op)
    : mlir::LLVMTypeConverter(op.getContext(), getLowerOptions(op)),
      dataLayout(op),
      targetTriple(::reussir::getTargetTriple(op)) {


  // Record types
  addConversion(
      [this](RecordType type, llvm::SmallVectorImpl<mlir::Type> &results) {
        return convertRecordType(type, results);
      });

  // Pointer-like types: RefType, RegionType, RcType, TokenType
  addConversion([this](RefType type) {
    return mlir::LLVM::LLVMPointerType::get(&getContext());
  });
  addConversion([this](RegionType type) {
    return mlir::LLVM::LLVMPointerType::get(&getContext());
  });
  addConversion([this](RcType type) {
    return mlir::LLVM::LLVMPointerType::get(&getContext());
  });
  addConversion([this](TokenType type) {
    return mlir::LLVM::LLVMPointerType::get(&getContext());
  });
  addConversion([this](RawPtrType type) {
    return mlir::LLVM::LLVMPointerType::get(&getContext());
  });

  // Closure types
  addConversion([this](ClosureType type) {
    // Convert to LLVM struct: { void* vtable, void* arg_start, void* arg_cursor
    // }
    llvm::SmallVector<mlir::Type> members;
    members.push_back(
        mlir::LLVM::LLVMPointerType::get(&getContext())); // vtable
    members.push_back(
        mlir::LLVM::LLVMPointerType::get(&getContext())); // arg_cursor
    return mlir::LLVM::LLVMStructType::getLiteral(&getContext(), members);
  });

  // Nullable types
  addConversion(
      [this](NullableType type) { return convertType(type.getPtrTy()); });

  // RcBox types
  addConversion([this](RcBoxType type) {
    llvm::SmallVector<mlir::Type> members;
    auto ptrTy = mlir::LLVM::LLVMPointerType::get(&getContext());
    if (type.isRegional()) {
      members.push_back(ptrTy);
      members.push_back(ptrTy);
      members.push_back(ptrTy);
    } else
      members.push_back(getIndexType());
    members.push_back(convertType(type.getElementType()));
    return mlir::LLVM::LLVMStructType::getLiteral(&getContext(), members);
  });

  // ClosureBox types
  addConversion([this](ClosureBoxType type) {
    llvm::SmallVector<mlir::Type> members;
    auto ptrTy = mlir::LLVM::LLVMPointerType::get(&getContext());
    members.push_back(ptrTy);
    members.push_back(ptrTy);
    for (auto payloadType : type.getPayloadTypes())
      members.push_back(convertType(payloadType));
    return mlir::LLVM::LLVMStructType::getLiteral(&getContext(), members);
  });

  // Str types
  addConversion([this](StrType type) {
    auto indexTy = getIndexType();
    auto ptrTy = mlir::LLVM::LLVMPointerType::get(&getContext());
    return mlir::LLVM::LLVMStructType::getLiteral(&getContext(),
                                                  {ptrTy, indexTy});
  });
}

std::optional<llvm::LogicalResult> LLVMTypeConverter::convertRecordType(
    RecordType type, llvm::SmallVectorImpl<mlir::Type> &results) {
  PopGuard popGuard;
  mlir::StringAttr name = type.getName();
  mlir::LLVM::LLVMStructType structType;

  if (name) {
    structType = mlir::LLVM::LLVMStructType::getIdentified(&getContext(), name);
    auto &callStack = getCurrentThreadRecursiveStack();
    if (llvm::is_contained(callStack, structType)) {
      results.push_back(structType);
      return mlir::success();
    }
    callStack.push_back(structType);
    popGuard.install(callStack);
  }

  llvm::SmallVector<mlir::Type> members;
  if (type.getKind() == reussir::RecordKind::variant) {
    // For variant records, we need to include the tag type as the first member
    members.push_back(getIndexType());
    auto [size, _y, representative] =
        type.getElementRegionLayoutInfo(getDataLayout());
    // member can be all empty
    if (representative) {
      members.push_back(convertType(representative));
      auto representativeSize = dataLayout.getTypeSize(representative);
      // Pad the representative type to the size of the record
      if (representativeSize < size)
        members.push_back(mlir::LLVM::LLVMArrayType::get(
            mlir::IntegerType::get(&getContext(), 8),
            size.getFixedValue() - representativeSize.getFixedValue()));
    }
  } else {
    for (auto [member, capability] :
         llvm::zip(type.getMembers(), type.getMemberIsField())) {
      mlir::Type projectedType =
          getProjectedType(member, capability, Capability::unspecified);
      members.push_back(convertType(projectedType));
    }
  }
  if (!name)
    structType = mlir::LLVM::LLVMStructType::getLiteral(&getContext(), members);
  if (name && failed(structType.setBody(members, false)))
    return mlir::failure();

  results.push_back(structType);
  return mlir::success();
}

bool LLVMTypeConverter::shouldUseSret(mlir::Type llvmType) const {
  // Only struct types can potentially need sret
  auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(llvmType);
  if (!structType)
    return false;

  // Get the size of the struct using MLIR DataLayout
  llvm::TypeSize typeSize = dataLayout.getTypeSize(llvmType);

  // Can't use sret for scalable types (shouldn't happen for structs)
  if (typeSize.isScalable())
    return false;

  uint64_t size = typeSize.getFixedValue();


  // Windows x64 ABI (Microsoft x64 calling convention)
  // Structs of size 1, 2, 4, or 8 bytes can be returned in RAX
  // All other sizes must use sret
  if (isWindowsTarget() && isX86_64()) {
    return size != 1 && size != 2 && size != 4 && size != 8;
  }

  // System V AMD64 ABI (used by Linux x86_64)
  // Simplified rule: structs <= 16 bytes can be returned in registers
  // (actual ABI is more complex with eightbyte classification, but this
  // is a good enough approximation for most cases)
  if (isX86_64()) {
    return size > 16;
  }

  // AAPCS64 (used by Linux aarch64 and Darwin arm64)
  // Simplified rule: structs <= 16 bytes can be returned in registers
  // (actual ABI considers HFAs/HVAs, but this is a reasonable approximation)
  if (isAArch64()) {
    return size > 16;
  }

  // For 32-bit targets, typically structs > 8 bytes use sret
  if (is32Bit()) {
    return size > 8;
  }

  // Default: conservative approach, use sret for anything > 8 bytes
  return size > 8;
}
} // namespace reussir
