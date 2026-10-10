//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements Reussir type conversion utilities.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/TypeConverter.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Sync/Conversion/TypeConverter.h"
#include "Sync/IR/SyncTypes.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DataLayout.h>
#include <memory>
#include <mlir/Dialect/DLTI/DLTI.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Target/LLVMIR/Import.h>

namespace reussir {
namespace {
mlir::LowerToLLVMOptions buildLowerOptions(mlir::ModuleOp op) {
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

thread_local llvm::DenseMap<const mlir::LLVMTypeConverter *,
                            llvm::SmallVector<mlir::Type, 8>>
    conversionCallStacks;

std::shared_ptr<mlir::DataLayout>
buildMLIRDataLayout(const mlir::LLVMTypeConverter &converter) {
  mlir::MLIRContext *context = &converter.getContext();
  context->getOrLoadDialect<mlir::DLTIDialect>();
  context->getOrLoadDialect<mlir::LLVM::LLVMDialect>();
  auto layoutModule = mlir::ModuleOp::create(mlir::UnknownLoc::get(context));
  std::string dataLayoutString =
      converter.getDataLayout().getStringRepresentation();
  if (!dataLayoutString.empty()) {
    layoutModule->setAttr(mlir::LLVM::LLVMDialect::getDataLayoutAttrName(),
                          mlir::StringAttr::get(context, dataLayoutString));
    layoutModule->setAttr(
        mlir::DLTIDialect::kDataLayoutAttrName,
        mlir::translateDataLayout(converter.getDataLayout(), context));
  }
  return std::make_shared<mlir::DataLayout>(layoutModule);
}

// What a first-class load and store of an LLVM value keeps of one byte of its
// storage, or what an arm needs kept: all of it (an integer of whole bytes, a
// pointer), only bit 0 (an i1), the byte as part of the floating-point value
// of type `floatType` that starts at `floatStart`, or nothing (padding).
struct PayloadByte {
  enum Kind { None, Bit0, Float, Full } kind = None;
  mlir::Type floatType = {};
  uint64_t floatStart = 0;

  bool operator==(const PayloadByte &other) const {
    return kind == other.kind && floatType == other.floatType &&
           floatStart == other.floatStart;
  }
  // A whole byte serves every need, and any value keeps bit 0 of its byte. A
  // floating-point byte serves only the same value at the same offset.
  bool keeps(const PayloadByte &needed) const {
    return kind == Full || needed.kind == None ||
           (needed.kind == Bit0 && kind != None) ||
           (needed.kind == Float && *this == needed);
  }
};

// Records in `bytes` what each byte of the LLVM type `type`, placed at
// `offset`, holds. Struct members are at their DataLayout offsets, array
// elements at their stride. A leaf of another type (a vector, an integer of
// an odd width) gets `other`. Returns false if the layout is not known.
bool markBytes(mlir::Type type, uint64_t offset, PayloadByte::Kind other,
               const mlir::DataLayout &dataLayout,
               llvm::MutableArrayRef<PayloadByte> bytes) {
  if (!type)
    return false;
  if (auto structTy = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(type)) {
    if (structTy.isOpaque() ||
        (structTy.isIdentified() && !structTy.isInitialized()))
      return false;
    uint64_t memberOffset = 0;
    for (mlir::Type member : structTy.getBody()) {
      if (!structTy.isPacked())
        memberOffset = llvm::alignTo(memberOffset,
                                     dataLayout.getTypeABIAlignment(member));
      if (!markBytes(member, offset + memberOffset, other, dataLayout, bytes))
        return false;
      memberOffset += dataLayout.getTypeSize(member);
    }
    return true;
  }
  if (auto arrayTy = llvm::dyn_cast<mlir::LLVM::LLVMArrayType>(type)) {
    mlir::Type element = arrayTy.getElementType();
    uint64_t stride = llvm::alignTo(dataLayout.getTypeSize(element),
                                    dataLayout.getTypeABIAlignment(element));
    for (uint64_t i = 0; i < arrayTy.getNumElements(); ++i)
      if (!markBytes(element, offset + i * stride, other, dataLayout, bytes))
        return false;
    return true;
  }
  PayloadByte byte{other};
  auto intTy = llvm::dyn_cast<mlir::IntegerType>(type);
  if (intTy && intTy.getWidth() == 1)
    byte = {PayloadByte::Bit0};
  else if ((intTy && intTy.getWidth() % 8 == 0) ||
           llvm::isa<mlir::LLVM::LLVMPointerType>(type))
    byte = {PayloadByte::Full};
  else if (llvm::isa<mlir::FloatType>(type))
    byte = {PayloadByte::Float, type, offset};
  uint64_t end = offset + dataLayout.getTypeSize(type);
  for (uint64_t i = offset; i < end && i < bytes.size(); ++i)
    bytes[i] = byte;
  return true;
}

// A [value] variant's payload is converted to its representative arm (the
// last arm with the largest alignment) and an [n x i8] tail, and [value]
// variants move as first-class aggregates of that type (record.variant,
// arguments, spills, fields). Such a move keeps all bytes of an integer of
// whole bytes or of a pointer, only bit 0 of an i1, the bytes of a
// floating-point value only as that value, and no padding. Another arm needs
// all bytes of its integers (also of [n x i8] members: the tail of a nested
// variant holds bytes) and pointers, bit 0 of its i1s, and its
// floating-point values. If the move loses a byte that an arm needs, use an
// array of alignment-sized integers of the payload's size instead: it has
// the same size and alignment, and every byte survives. Field accesses
// address the payload member and then use the arm's own type, so they are
// unaffected. Returns null to keep the representative (also when no integer
// type has the payload's alignment).
mlir::Type opaquePayloadType(mlir::LLVMTypeConverter &converter,
                             RecordType type, mlir::Type representative,
                             llvm::ArrayRef<mlir::Type> payload,
                             llvm::TypeSize size, llvm::Align alignment,
                             const mlir::DataLayout &dataLayout) {
  auto word = mlir::IntegerType::get(type.getContext(), alignment.value() * 8);
  if (dataLayout.getTypeABIAlignment(word) != alignment.value() ||
      size.getFixedValue() % alignment.value() != 0)
    return nullptr;
  auto opaque = mlir::LLVM::LLVMArrayType::get(word, size.getFixedValue() /
                                                         alignment.value());
  llvm::SmallVector<PayloadByte> kept(size.getFixedValue());
  if (!markBytes(
          mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), payload),
          0, PayloadByte::None, dataLayout, kept))
    return opaque;
  for (auto [member, isField] :
       llvm::zip(type.getMembers(), type.getMemberIsField())) {
    if (!member)
      continue;
    mlir::Type arm = getProjectedType(member, isField, Capability::value);
    if (arm == representative)
      continue;
    llvm::SmallVector<PayloadByte> needed(size.getFixedValue());
    if (!markBytes(converter.convertType(arm), 0, PayloadByte::Full,
                   dataLayout, needed))
      return opaque;
    for (auto [keptByte, neededByte] : llvm::zip(kept, needed))
      if (!keptByte.keeps(neededByte))
        return opaque;
  }
  return nullptr;
}

std::optional<llvm::LogicalResult>
convertRecordType(mlir::LLVMTypeConverter &converter,
                  const mlir::DataLayout &dataLayout, RecordType type,
                  llvm::SmallVectorImpl<mlir::Type> &results) {
  PopGuard popGuard;
  mlir::StringAttr name = type.getName();
  mlir::LLVM::LLVMStructType structType;
  auto &callStack = conversionCallStacks[&converter];

  if (name) {
    structType =
        mlir::LLVM::LLVMStructType::getIdentified(type.getContext(), name);
    if (llvm::is_contained(callStack, structType)) {
      results.push_back(structType);
      return mlir::success();
    }
    callStack.push_back(structType);
    popGuard.install(callStack);
  }

  llvm::SmallVector<mlir::Type> members;
  if (type.getKind() == reussir::RecordKind::variant) {
    // A fused-header variant is `{i32 count slot, i32 tag, payload}` — the
    // rc box overlays its refcount on field 0. Value variants carry just
    // their minimal-width tag; either way the payload lands at its natural
    // alignment boundary after the header.
    if (type.hasFusedHeader())
      members.push_back(mlir::IntegerType::get(type.getContext(), 32));
    members.push_back(type.getTagType());
    auto [size, alignment, representative] =
        type.getElementRegionLayoutInfo(dataLayout);
    if (representative) {
      llvm::SmallVector<mlir::Type, 2> payload{
          converter.convertType(representative)};
      auto representativeSize = dataLayout.getTypeSize(representative);
      if (representativeSize < size)
        payload.push_back(mlir::LLVM::LLVMArrayType::get(
            mlir::IntegerType::get(type.getContext(), 8),
            size.getFixedValue() - representativeSize.getFixedValue()));
      // Fused-header (shared) variants are accessed through their boxes: a
      // value of one only exists between its construction and the rc.create
      // that boxes it, which RcCreateFusion turns into stores into the box
      // (rc.create_variant). [value] variants are moved as values.
      mlir::Type opaque =
          type.hasFusedHeader()
              ? mlir::Type{}
              : opaquePayloadType(converter, type, representative, payload,
                                  size, alignment, dataLayout);
      if (opaque)
        members.push_back(opaque);
      else
        members.append(payload);
    }
  } else {
    size_t expectedTotalSize = dataLayout.getTypeSize(type);
    size_t currentSize = 0;
    // Struct fields are emitted in packed physical order (descending storage
    // alignment, stable on declaration order); lowerings translate logical
    // member indices through getPhysicalMemberIndex.
    for (uint32_t logical : type.getPackedOrder(dataLayout)) {
      mlir::Type member = type.getMembers()[logical];
      bool capability = type.getMemberIsField()[logical];
      mlir::Type projectedType =
          getProjectedType(member, capability, Capability::unspecified);
      auto align = dataLayout.getTypeABIAlignment(projectedType);
      if (currentSize % align != 0) {
        size_t lastMemberNeedToPad = align - (currentSize % align);
        mlir::Type lastMemberType = members.back();
        size_t lastMemberSize = dataLayout.getTypeSize(lastMemberType);
        llvm::SmallVector<mlir::Type> liftCandidates = {
            mlir::IntegerType::get(type.getContext(), 64),
            mlir::IntegerType::get(type.getContext(), 32),
            mlir::IntegerType::get(type.getContext(), 16),
            mlir::IntegerType::get(type.getContext(), 8),
        };
        // Widen the previous member over the padding only to an integer no
        // more aligned than the next member: that integer then starts where
        // the previous member does and does not raise the record's
        // alignment. A more aligned one (a 3-byte record followed by a u16,
        // lifted to i32) would move members and make the LLVM struct larger
        // than the layout Reussir allocates; use explicit padding instead.
        bool lift = false;
        for (auto liftCandidate : liftCandidates) {
          auto liftCandidateSize = dataLayout.getTypeSize(liftCandidate);
          if (lastMemberSize < liftCandidateSize &&
              lastMemberSize + lastMemberNeedToPad == liftCandidateSize &&
              dataLayout.getTypeABIAlignment(liftCandidate) <= align) {
            members.back() = liftCandidate;
            lift = true;
            break;
          }
        }
        if (!lift) {
          members.back() = mlir::LLVM::LLVMStructType::getLiteral(
              type.getContext(),
              {lastMemberType, mlir::LLVM::LLVMArrayType::get(
                                   mlir::IntegerType::get(type.getContext(), 8),
                                   lastMemberNeedToPad)});
        }
        currentSize += lastMemberNeedToPad;
      }
      members.push_back(converter.convertType(projectedType));
      currentSize += dataLayout.getTypeSize(projectedType);
    }
    if (currentSize < expectedTotalSize)
      members.push_back(mlir::LLVM::LLVMArrayType::get(
          mlir::IntegerType::get(type.getContext(), 8),
          expectedTotalSize - currentSize));
  }

  if (!name)
    structType =
        mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), members);
  if (name && failed(structType.setBody(members, false)))
    return mlir::failure();

  results.push_back(structType);
  return mlir::success();
}
} // namespace

mlir::LowerToLLVMOptions getReussirToLLVMOptions(mlir::ModuleOp op) {
  return buildLowerOptions(op);
}

void populateReussirToLLVMTypeConversions(mlir::LLVMTypeConverter &converter) {
  auto dataLayout = buildMLIRDataLayout(converter);
  // Lock-guarded cells lower onto the `sync` dialect: register its type
  // conversions so that `convertType` can resolve `!sync.*` wrappers (and, via
  // the CellType conversion below, the reussir cell types built on them).
  mlir::sync::populateSyncToLLVMTypeConversions(converter);
  converter.addConversion(
      [&converter, dataLayout](RecordType type,
                               llvm::SmallVectorImpl<mlir::Type> &results) {
        return convertRecordType(converter, *dataLayout, type, results);
      });

  converter.addConversion([](RefType type) {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  converter.addConversion([](HoleType type) {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  // A constructor context is a {root, hole} pointer pair; a null hole is the
  // empty context.
  converter.addConversion([](CctxType type) {
    auto ptrTy = mlir::LLVM::LLVMPointerType::get(type.getContext());
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(),
                                                  {ptrTy, ptrTy});
  });
  converter.addConversion([](RegionType type) {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  converter.addConversion([](RcType type) {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  converter.addConversion([](TokenType type) -> mlir::Type {
    // Every token is a bare pointer. A dynamic token (`size: ?`, from an
    // unpinned variant decrement under per-constructor sizing) carries no
    // size: its free/realloc go through the unsized runtime ABI, which
    // recovers the block from the pointer (a size-recovering allocator).
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  converter.addConversion([](RawPtrType type) {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });

  converter.addConversion([](ClosureType type) {
    llvm::SmallVector<mlir::Type> members;
    members.push_back(mlir::LLVM::LLVMPointerType::get(type.getContext()));
    members.push_back(mlir::LLVM::LLVMPointerType::get(type.getContext()));
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), members);
  });

  converter.addConversion([&converter](NullableType type) {
    return converter.convertType(type.getPtrTy());
  });

  // Plain and atomic cells are semantic one-field wrappers. Keeping the
  // wrapper in the LLVM type makes `ref.project [0]` the uniform way to
  // address their inline mutable slot; atomicity changes the accesses, not
  // the RC payload shape. An exclusive cell additionally carries a trailing
  // i1 in-use flag, addressed as `ref.project [1]`.
  converter.addConversion([&converter](CellType type) -> mlir::Type {
    // A lock-guarded cell is physically the corresponding `sync` primitive
    // wrapping the payload (`!sync.rwlock<T>` etc.). Defer to the sync type
    // conversions so the RC box gets the lock header plus payload layout.
    mlir::MLIRContext *ctx = type.getContext();
    if (type.getRwlock())
      return converter.convertType(
          mlir::sync::RwLockType::get(ctx, type.getElementType()));
    if (type.getMutex())
      return converter.convertType(
          mlir::sync::MutexType::get(ctx, type.getElementType()));
    if (type.getFlatlock())
      return converter.convertType(
          mlir::sync::CombiningLockType::get(ctx, type.getElementType()));
    llvm::SmallVector<mlir::Type> members{
        converter.convertType(type.getElementType())};
    if (type.getExclusive())
      members.push_back(mlir::IntegerType::get(ctx, 1));
    return mlir::LLVM::LLVMStructType::getLiteral(ctx, members);
  });

  converter.addConversion([&converter](RcBoxType type) -> mlir::Type {
    // A box of a fused-header variant IS the variant record: its refcount
    // lives in the record's leading count slot.
    if (type.isHeaderFused())
      return converter.convertType(type.getElementType());
    llvm::SmallVector<mlir::Type> members;
    for (mlir::Type headerType : type.getHeaderTypes())
      members.push_back(converter.convertType(headerType));
    mlir::Type payloadType;
    if (type.hasDynamicArrayPayload()) {
      // Only the box layout uses a zero-length tail for dynamic storage;
      // a dynamic array itself has no concrete LLVM value type.
      auto arrayType = llvm::cast<ArrayType>(type.getElementType());
      mlir::Type elementType = converter.convertType(arrayType.getElementType());
      if (!elementType)
        return {};
      payloadType = mlir::LLVM::LLVMArrayType::get(elementType, 0);
    } else {
      payloadType = converter.convertType(type.getElementType());
      if (!payloadType)
        return {};
    }
    members.push_back(payloadType);
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), members);
  });

  converter.addConversion([&converter, dataLayout](ClosureBoxType type) {
    llvm::SmallVector<mlir::Type> payloadMembers;
    for (auto payloadType : type.getPayloadTypes())
      payloadMembers.push_back(converter.convertType(payloadType));

    auto payloadStruct = mlir::LLVM::LLVMStructType::getLiteral(
        type.getContext(), payloadMembers);

    // Keep the closure box pointer-aligned regardless of the payload. Packing
    // only the payload tail prevents the normally laid-out payload aggregate
    // from raising the box alignment; explicit padding places that aggregate
    // at its required address.
    llvm::SmallVector<mlir::Type> tailMembers;
    uint64_t payloadPadding = type.getPayloadPadding(*dataLayout);
    if (payloadPadding != 0) {
      auto i8Ty = mlir::IntegerType::get(type.getContext(), 8);
      tailMembers.push_back(
          mlir::LLVM::LLVMArrayType::get(i8Ty, payloadPadding));
    }
    tailMembers.push_back(payloadStruct);
    auto payloadTail = mlir::LLVM::LLVMStructType::getLiteral(
        type.getContext(), tailMembers, /*isPacked=*/true);
    llvm::SmallVector<mlir::Type> members;
    for (mlir::Type headerType : type.getHeaderTypes())
      members.push_back(converter.convertType(headerType));
    members.push_back(payloadTail);
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), members);
  });

  converter.addConversion([&converter](StrType type) {
    auto indexTy = converter.getIndexType();
    auto ptrTy = mlir::LLVM::LLVMPointerType::get(type.getContext());
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(),
                                                  {ptrTy, indexTy});
  });

  converter.addConversion([&converter](ArrayType type) -> mlir::Type {
    if (type.hasTargetAttr()) {
      llvm::SmallVector<mlir::Type> members;
      if (mlir::failed(
              converter.convertTypes(type.getDescriptorTypes(), members)))
        return {};
      return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), members);
    }
    // Dynamic arrays must not reach LLVM lowering as concrete values.
    if (type.hasDynamicShape())
      return {};
    mlir::Type lowered = converter.convertType(type.getElementType());
    if (!lowered)
      return {};
    for (int64_t extent : llvm::reverse(type.getShape()))
      lowered = mlir::LLVM::LLVMArrayType::get(lowered, extent);
    return lowered;
  });
}
} // namespace reussir
