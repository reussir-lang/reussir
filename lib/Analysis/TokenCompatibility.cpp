//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/Analysis/TokenCompatibility.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/Support/AllocatorBinModel.h"

#include <mlir/Interfaces/LoopLikeInterface.h>
#include <mlir/Interfaces/ValueBoundsOpInterface.h>

namespace reussir {

namespace {
mlir::Value getDynamicAllocationSize(TokenAcceptor acceptor) {
  if (!acceptor || !acceptor.getToken())
    return {};
  mlir::Operation *op = acceptor.getToken().getDefiningOp();
  if (auto alloc = mlir::dyn_cast_if_present<ReussirTokenAllocOp>(op))
    return alloc.getDynamicSize();
  if (auto ensure = mlir::dyn_cast_if_present<ReussirTokenEnsureOp>(op))
    return ensure.getDynamicSize();
  if (auto realloc = mlir::dyn_cast_if_present<ReussirTokenReallocOp>(op))
    return realloc.getDynamicSize();
  return {};
}

bool areEqual(mlir::Value lhs, mlir::Value rhs) {
  return lhs == rhs || mlir::ValueBoundsConstraintSet::compare(
                           lhs, mlir::ValueBoundsConstraintSet::EQ, rhs);
}

mlir::ValueRange getArrayExtents(mlir::Operation *op) {
  if (auto create = mlir::dyn_cast_if_present<ReussirArrayCreateOp>(op))
    return create.getExtents();
  if (auto instantiate =
          mlir::dyn_cast_if_present<ReussirArrayInstantiateOp>(op))
    return instantiate.getExtents();
  return {};
}

bool haveEqualArrayExtents(mlir::TypedValue<RcType> donorRc,
                           TokenAcceptor donor, TokenAcceptor recipient) {
  mlir::ValueRange newExtents = getArrayExtents(recipient);
  if (!donorRc || newExtents.empty())
    return false;
  auto arrayType =
      mlir::dyn_cast<ArrayType>(donorRc.getType().getElementType());
  auto resultType = mlir::cast<RcType>(recipient->getResult(0).getType());
  // Equal extents imply equal bytes only for the same header and element
  // layout. Compare dimensions individually: their product may be nonlinear.
  if (!arrayType || arrayType != resultType.getElementType())
    return false;
  mlir::ValueRange oldExtents = getArrayExtents(donor);
  if (!oldExtents.empty() &&
      llvm::all_of(llvm::zip(oldExtents, newExtents), [](auto pair) {
        return areEqual(std::get<0>(pair), std::get<1>(pair));
      }))
    return true;

  // An existing descriptor also covers incoming arrays. The memref models
  // relate its dimensions to memref.dim and arithmetic on those results;
  // no header reads are inserted after the donor's lifetime has ended.
  for (mlir::Operation *user : donorRc.getUsers()) {
    auto borrow = mlir::dyn_cast<ReussirRcBorrowOp>(user);
    if (!borrow)
      continue;
    for (mlir::Operation *refUser : borrow.getBorrowed().getUsers()) {
      auto view = mlir::dyn_cast<ReussirArrayViewOp>(refUser);
      if (!view || !mlir::isa<mlir::MemRefType>(view.getView().getType()))
        continue;
      unsigned dynamicDim = 0;
      bool equal = true;
      for (auto [dim, extent] : llvm::enumerate(arrayType.getShape())) {
        if (!mlir::ShapedType::isDynamic(extent))
          continue;
        if (!mlir::ValueBoundsConstraintSet::compare(
                {view.getView(), static_cast<int64_t>(dim)},
                mlir::ValueBoundsConstraintSet::EQ, newExtents[dynamicDim++])) {
          equal = false;
          break;
        }
      }
      if (equal)
        return true;
    }
  }
  return false;
}
} // namespace

bool TokenSizeAnalysis::haveEqualSizes(TokenType donorType,
                                       mlir::TypedValue<RcType> donorRc,
                                       TokenAcceptor recipient) {
  TokenType recipientType = recipient.getTokenType();
  if (donorType.getAlign() != recipientType.getAlign())
    return false;
  if (!donorType.isDynamicSize() && !recipientType.isDynamicSize())
    return donorType == recipientType;
  auto donor =
      donorRc
          ? mlir::dyn_cast_if_present<TokenAcceptor>(donorRc.getDefiningOp())
          : TokenAcceptor{};
  auto oldSize = getConstantSize(donorType, donor);
  auto newSize = getConstantSize(recipientType, recipient);
  if (oldSize && newSize)
    return *oldSize == *newSize;
  mlir::Value oldBytes = getDynamicAllocationSize(donor);
  mlir::Value newBytes = getDynamicAllocationSize(recipient);
  if (oldBytes && newBytes && areEqual(oldBytes, newBytes))
    return true;
  return haveEqualArrayExtents(donorRc, donor, recipient);
}

std::optional<uint64_t>
TokenSizeAnalysis::getConstantSize(TokenType type, TokenAcceptor acceptor) {
  if (!type.isDynamicSize())
    return type.getSize();
  mlir::Value size = getDynamicAllocationSize(acceptor);
  if (!size)
    return std::nullopt;
  auto [it, inserted] = constantSizes.try_emplace(size, std::nullopt);
  if (inserted) {
    auto bound = mlir::ValueBoundsConstraintSet::computeConstantBound(
        mlir::presburger::BoundType::EQ, size);
    if (mlir::succeeded(bound) && *bound >= 0)
      it->second = static_cast<uint64_t>(*bound);
  }
  return it->second;
}

int scoreTokenForAcceptor(TokenType producedType,
                          mlir::TypedValue<RcType> producerRc,
                          TokenAcceptor consumer,
                          EquivalenceAnalysis &equivalence,
                          TokenSizeAnalysis &sizes) {
  // Under a proven size and alignment match, measure the locality score.
  if (sizes.haveEqualSizes(producedType, producerRc, consumer)) {
    ReussirRcCreateOp create =
        dyn_cast<ReussirRcCreateOp>(consumer.getOperation());
    int localityScore = kReallocEnsureCutoff;
    // First, we do a very coarse grained copy avoidance analysis.
    if (producerRc && create &&
        mlir::isa<RecordType>(create.getRcPtr().getType().getElementType())) {
      // Check if any record field is assembled from a projection whose source
      // is aliased with the producer.
      mlir::ValueRange fields{};
      mlir::Operation *op = create.getRcPtr().getDefiningOp();
      while (auto variant =
                 llvm::dyn_cast_if_present<ReussirRecordVariantOp>(op))
        op = variant.getValue().getDefiningOp();
      if (auto compound =
              llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(op))
        fields = compound.getFields();
      for (mlir::Value field : fields) {
        if (auto loaded = llvm::dyn_cast_if_present<ReussirRefLoadOp>(
                field.getDefiningOp())) {
          mlir::Value root = loaded.getRef();
          while (true) {
            if (auto projection =
                    llvm::dyn_cast<ReussirRefProjectOp>(root.getDefiningOp())) {
              root = projection.getRef();
              continue;
            }
            if (auto blkArg = llvm::dyn_cast<mlir::BlockArgument>(root)) {
              if (auto dispatch =
                      llvm::dyn_cast_if_present<ReussirRecordDispatchOp>(
                          blkArg.getOwner()->getParentOp())) {
                root = dispatch.getValue();
                continue;
              }
            }
            break;
          }
          if (auto borrow = llvm::dyn_cast_if_present<ReussirRcBorrowOp>(
                  root.getDefiningOp()))
            localityScore +=
                equivalence.areEquivalent(borrow.getRcPtr(), producerRc);
        }
      }
    }
    return localityScore;
  }
  // A dynamic token (`token<align, ?>`, an unpinned variant decrement) can be
  // resized to any acceptor at runtime via realloc — a universal *fallback*
  // donor. It scores below both the ensure tier and the fixed-size same-bin
  // realloc tier, so it is chosen only when no statically-sized donor fits.
  if (producedType.isDynamicSize() || consumer.getTokenType().isDynamicSize())
    return 0;
  TokenType consumerType = consumer.getTokenType();
  size_t oldSize = producedType.getSize();
  size_t newSize = consumerType.getSize();
  size_t oldAlign = producedType.getAlign();
  size_t newAlign = consumerType.getAlign();
  // A fixed-size same-bin donor also reallocs, but is preferred over a
  // dynamic one — score it one tier above.
  return sameAllocatorBin(oldAlign, oldSize, newAlign, newSize) ? 1 : -1;
}

// A call sits in tail position when nothing observable runs after it on any
// path to the function return: it immediately precedes its block terminator,
// its results feed nothing but that terminator, and every enclosing region op
// sits in tail position itself (loops never qualify — a call ending a loop
// body resumes iteration, not the return). Tokens carried across such a call
// can never be consumed afterwards — the only thing that could follow is
// their `token.free`, which both keeps the frame alive (deep nbe-shaped
// recursion overflows the default stack) and defers the release of every
// carried block until after the whole callee subtree (peak live memory ~
// doubles on allocation-dense evaluators). So the matcher must treat a
// tail-positioned call as a flush point even with reuse-across-call enabled.
bool isTailPositioned(mlir::Operation *op) {
  while (true) {
    mlir::Block *block = op->getBlock();
    if (!block)
      return false;
    mlir::Operation *terminator = block->getTerminator();
    if (op->getNextNode() != terminator)
      return false;
    for (mlir::OpResult res : op->getResults())
      for (mlir::Operation *user : res.getUsers())
        if (user != terminator)
          return false;
    mlir::Operation *parent = block->getParentOp();
    if (!parent)
      return false;
    if (isa<mlir::FunctionOpInterface>(parent))
      return terminator->hasTrait<mlir::OpTrait::ReturnLike>();
    if (isa<mlir::LoopLikeOpInterface>(parent) ||
        !isa<mlir::RegionBranchOpInterface>(parent))
      return false;
    op = parent;
  }
}

} // namespace reussir
