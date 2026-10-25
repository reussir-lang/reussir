//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// Arbitrary-precision integers stored in Reussir values. An `!rcgmp.mpz`
// member occupies one word, like a shared link, but its count lives behind
// rcgmp's tagged handle, so retains and releases go through the rcgmp dialect
// instead of `reussir.rc.inc`/`reussir.rc.dec`.
//
//===----------------------------------------------------------------------===//
#include <llvm/Support/ErrorHandling.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/BuiltinTypeInterfaces.h>
#include <mlir/IR/DialectRegistry.h>
#include <mlir/IR/Matchers.h>

#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#ifdef REUSSIR_ENABLE_RCGMP
#include "RCGMP/IR/RCGMPDialect.h"
#include "RCGMP/IR/RCGMPOps.h"
#include "RCGMP/IR/RCGMPTypes.h"
#endif

namespace reussir {

bool isRCGMPIntegerType(mlir::Type type) {
#ifdef REUSSIR_ENABLE_RCGMP
  return llvm::isa_and_present<mlir::rcgmp::MpzType>(type);
#else
  (void)type;
  return false;
#endif
}

#ifdef REUSSIR_ENABLE_RCGMP
namespace {
// An array of integers is viewed as a memref of handles. To the memref the
// handle is one plain word; the array operations manage its references.
struct MpzMemRefElementModel
    : public mlir::MemRefElementTypeInterface::ExternalModel<
          MpzMemRefElementModel, mlir::rcgmp::MpzType> {};
} // namespace

void registerRCGMPExternalModels(mlir::DialectRegistry &registry) {
  registry.addExtension(
      +[](mlir::MLIRContext *context, mlir::rcgmp::RCGMPDialect *) {
        mlir::rcgmp::MpzType::attachInterface<MpzMemRefElementModel>(*context);
      });
}

void emitRCGMPRetain(mlir::OpBuilder &builder, mlir::Location loc,
                     mlir::Value value, mlir::Value delta) {
  llvm::APInt constant;
  if (!delta || mlir::matchPattern(delta, mlir::m_ConstantInt(&constant))) {
    uint64_t count = delta ? constant.getZExtValue() : 1;
    for (uint64_t i = 0; i < count; ++i)
      mlir::rcgmp::RetainOp::create(builder, loc, value);
    return;
  }
  // `rcgmp.retain` adds one reference; a count known only at run time loops.
  auto zero = mlir::arith::ConstantIndexOp::create(builder, loc, 0);
  auto one = mlir::arith::ConstantIndexOp::create(builder, loc, 1);
  mlir::scf::ForOp::create(builder, loc, zero, delta, one, mlir::ValueRange{},
                           [&](mlir::OpBuilder &body, mlir::Location bodyLoc,
                               mlir::Value, mlir::ValueRange) {
                             mlir::rcgmp::RetainOp::create(body, bodyLoc,
                                                           value);
                             mlir::scf::YieldOp::create(body, bodyLoc);
                           });
}

void emitRCGMPRelease(mlir::OpBuilder &builder, mlir::Location loc,
                      mlir::Value value) {
  mlir::rcgmp::ReleaseOp::create(builder, loc, value);
}

mlir::Value getRCGMPRetainedValue(mlir::Operation *op) {
  auto retain = llvm::dyn_cast<mlir::rcgmp::RetainOp>(op);
  return retain ? retain.getValue() : mlir::Value{};
}

mlir::Value getRCGMPReleasedValue(mlir::Operation *op) {
  auto release = llvm::dyn_cast<mlir::rcgmp::ReleaseOp>(op);
  return release ? release.getValue() : mlir::Value{};
}
#else
void registerRCGMPExternalModels(mlir::DialectRegistry &) {}

void emitRCGMPRetain(mlir::OpBuilder &, mlir::Location, mlir::Value,
                     mlir::Value) {
  llvm_unreachable("Reussir is built without rcgmp");
}

void emitRCGMPRelease(mlir::OpBuilder &, mlir::Location, mlir::Value) {
  llvm_unreachable("Reussir is built without rcgmp");
}

mlir::Value getRCGMPRetainedValue(mlir::Operation *) { return {}; }

mlir::Value getRCGMPReleasedValue(mlir::Operation *) { return {}; }
#endif

} // namespace reussir
