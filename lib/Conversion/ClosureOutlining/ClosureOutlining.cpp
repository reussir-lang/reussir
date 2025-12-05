//===-- ClosureOutlining.cpp ------------------------------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <mlir/Pass/Pass.h>

#include "Reussir/Conversion/ClosureOutlining.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "llvm/ADT/Twine.h"

#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/xxhash.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/IR/PatternMatch.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRCLOSUREOUTLININGPASS
#include "Reussir/Conversion/Passes.h.inc"

//===----------------------------------------------------------------------===//
// ClosureOutliningPass
//===----------------------------------------------------------------------===//

namespace {
class ClosureNameUniquifier {
  llvm::StringMap<size_t> nameOccurrences;

public:
  ClosureNameUniquifier() = default;
  std::string uniquify(ReussirClosureCreateOp op) {
    llvm::SmallString<128> buffer;
    auto function = op->getParentOfType<mlir::func::FuncOp>();
    auto moduleOp = function->getParentOfType<mlir::ModuleOp>();
    buffer.append(moduleOp.getName() ? *moduleOp.getName() : "<anonymous>");
    buffer.append(function.getName());
    std::stringstream ss;
    auto [high, lower] = llvm::xxh3_128bits(llvm::ArrayRef(
        reinterpret_cast<const uint8_t *>(buffer.data()), buffer.size()));
    ss << "reussir_closure_" << std::hex << high << '_' << std::hex << lower;
    std::string name = ss.str();
    if (nameOccurrences[name]++) {
      name.append("_");
      name.append(std::to_string(nameOccurrences[name]));
    }
    return name;
  }
};
struct ClosureOutliningPass
    : public impl::ReussirClosureOutliningPassBase<ClosureOutliningPass> {
  using Base::Base;

  /// Creates a function from the closure's inlined region and returns it.
  /// The function will have:
  /// - Arguments matching the closure's input types
  /// - Return type matching the closure's output type (if any)
  /// - Body cloned from the closure's region with yield replaced by return
  mlir::func::FuncOp createFunctionAndInlineRegion(ReussirClosureCreateOp op,
                                                   llvm::Twine name,
                                                   mlir::IRRewriter &rewriter);

  /// Create the closure drop function.
  mlir::func::FuncOp createClosureDropFunction(ReussirClosureCreateOp op,
                                               llvm::Twine name,
                                               mlir::IRRewriter &rewriter);

  /// Create the closure clone function
  mlir::func::FuncOp createClosureCloneFunction(ReussirClosureCreateOp op,
                                                llvm::Twine name,
                                                mlir::IRRewriter &rewriter);

  void runOnOperation() override;
};

void ClosureOutliningPass::runOnOperation() {
  mlir::ModuleOp moduleOp = getOperation();
  llvm::SmallVector<ReussirClosureCreateOp> closureCreateOps;
  moduleOp.walk([&](ReussirClosureCreateOp op) {
    if (op.isInlined())
      closureCreateOps.push_back(op);
  });
  ClosureNameUniquifier nameUniquifier;
  mlir::IRRewriter rewriter(moduleOp.getContext());
  for (auto op : closureCreateOps) {
    auto name = nameUniquifier.uniquify(op);
    // First, create a function, inline the region into the function and
    // change the yield op to return op during the translation.
    mlir::func::FuncOp evaluateFunction =
        createFunctionAndInlineRegion(op, name, rewriter);

    // Second, create closure's clone and drop functions
    mlir::func::FuncOp cloneFunction =
        createClosureCloneFunction(op, name, rewriter);
    mlir::func::FuncOp dropFunction =
        createClosureDropFunction(op, name, rewriter);

    // Third, create vtable operation
    rewriter.setInsertionPointToEnd(moduleOp.getBody());
    std::string vtableName = name + "_vtable";

    // Create the vtable operation
    mlir::FlatSymbolRefAttr dropAttr = mlir::FlatSymbolRefAttr::get(
        rewriter.getContext(), dropFunction.getName());
    mlir::FlatSymbolRefAttr cloneAttr = mlir::FlatSymbolRefAttr::get(
        rewriter.getContext(), cloneFunction.getName());
    mlir::FlatSymbolRefAttr evaluateAttr = mlir::FlatSymbolRefAttr::get(
        rewriter.getContext(), evaluateFunction.getName());

    rewriter.create<ReussirClosureVtableOp>(op.getLoc(),
                                            rewriter.getStringAttr(vtableName),
                                            evaluateAttr, dropAttr, cloneAttr);

    // Fourth, update the closure to use
    // outlined version (e.g. remove the
    // region and set vtable attribute)
    mlir::FlatSymbolRefAttr vtableAttr =
        mlir::FlatSymbolRefAttr::get(rewriter.getContext(), vtableName);
    op.setVtableAttr(vtableAttr);
  }
}

mlir::func::FuncOp ClosureOutliningPass::createFunctionAndInlineRegion(
    ReussirClosureCreateOp op, llvm::Twine name, mlir::IRRewriter &rewriter) {
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  std::string invokeName = (name + "_evaluate").str();
  mlir::ModuleOp moduleOp = getOperation();
  rewriter.setInsertionPointToEnd(moduleOp.getBody());

  // Get the closure type to determine function signature
  ClosureType closureType =
      llvm::cast<ClosureType>(op.getClosure().getType().getElementType());
  mlir::Type outputType = closureType.getOutputType();
  ClosureBoxType closureBoxType = op.getClosureBoxType();
  RcType specializedRcType = RcType::get(rewriter.getContext(), closureBoxType);

  llvm::SmallVector<mlir::Type> resultTypes;
  if (outputType)
    resultTypes.push_back(outputType);
  mlir::FunctionType funcType =
      rewriter.getFunctionType(specializedRcType, resultTypes);

  // Create the function
  auto funcOp =
      rewriter.create<mlir::func::FuncOp>(op.getLoc(), invokeName, funcType);
  funcOp.setPrivate();
  funcOp->setAttr("llvm.linkage",
                  mlir::LLVM::LinkageAttr::get(rewriter.getContext(),
                                               mlir::LLVM::Linkage::Internal));

  // Create entry block for the function
  mlir::Block *entryBlock = funcOp.addEntryBlock();
  rewriter.setInsertionPointToStart(entryBlock);
  mlir::Location loc = op.getLoc();

  // === PROLOGUE: Borrow the Rc and load all fields ===
  mlir::Value rcPtr = entryBlock->getArgument(0);

  // Borrow the rc to get a reference to the closure box
  RefType closureBoxRefType =
      rewriter.getType<RefType>(closureBoxType, Capability::unspecified);
  mlir::Value closureBoxRef =
      rewriter.create<ReussirRcBorrowOp>(loc, closureBoxRefType, rcPtr);

  // Load all payload fields from the closure box
  auto payloadTypes = closureBoxType.getPayloadTypes();
  llvm::SmallVector<mlir::Value> blockArgs;
  for (size_t i = 0; i < payloadTypes.size(); ++i) {
    mlir::Type payloadType = payloadTypes[i];
    RefType payloadRefType =
        rewriter.getType<RefType>(payloadType, Capability::unspecified);

    // Get reference to this payload field
    mlir::Value payloadRef = rewriter.create<ReussirClosureInspectPayloadOp>(
        loc, payloadRefType, rewriter.getIndexAttr(i), closureBoxRef);

    // Load the payload value
    mlir::Value payloadValue =
        rewriter.create<ReussirRefLoadOp>(loc, payloadType, payloadRef);
    blockArgs.push_back(payloadValue);
  }

  // Add the input arguments (from function args after rcPtr)
  for (size_t i = 1; i < entryBlock->getNumArguments(); ++i)
    blockArgs.push_back(entryBlock->getArgument(i));

  // Inline the closure block into the function entry block
  mlir::Block &closureBlock = op.getBody().front();
  rewriter.inlineBlockBefore(&closureBlock, entryBlock, entryBlock->end(),
                             blockArgs);

  // === EPILOGUE: Replace yield with fetch_dec + conditional free + return ===
  funcOp.walk([&](ReussirClosureYieldOp yieldOp) {
    rewriter.setInsertionPoint(yieldOp);
    mlir::Location yieldLoc = yieldOp.getLoc();

    // Fetch dec on the rc pointer
    auto fetchDec = rewriter.create<ReussirRcFetchDecOp>(yieldLoc, rcPtr);

    // If fetch_dec result is 1, free the token
    auto one = rewriter.create<mlir::arith::ConstantIndexOp>(yieldLoc, 1);
    auto isOne = rewriter.create<mlir::arith::CmpIOp>(
        yieldLoc, mlir::arith::CmpIPredicate::eq, fetchDec.getRefCount(), one);

    auto ifOp =
        rewriter.create<mlir::scf::IfOp>(yieldLoc, mlir::TypeRange{}, isOne,
                                         /*addThenRegion=*/true,
                                         /*addElseRegion=*/false);

    // Inside the then block: free the token
    rewriter.setInsertionPointToStart(ifOp.thenBlock());

    // Compute TokenType from RcBoxType using DataLayout
    RcBoxType rcBoxType = specializedRcType.getInnerBoxType();
    mlir::DataLayout dataLayout = mlir::DataLayout::closest(funcOp);
    size_t size = dataLayout.getTypeSize(rcBoxType).getFixedValue();
    size_t align = dataLayout.getTypeABIAlignment(rcBoxType);
    TokenType tokenType = TokenType::get(rewriter.getContext(), align, size);

    mlir::Value token =
        rewriter.create<ReussirRcReinterpretOp>(yieldLoc, tokenType, rcPtr);
    rewriter.create<ReussirTokenFreeOp>(yieldLoc, token);
    rewriter.create<mlir::scf::YieldOp>(yieldLoc);

    // After the if, create the return
    rewriter.setInsertionPointAfter(ifOp);
    if (yieldOp.getValue())
      rewriter.create<mlir::func::ReturnOp>(yieldLoc, yieldOp.getValue());
    else
      rewriter.create<mlir::func::ReturnOp>(yieldLoc);

    rewriter.eraseOp(yieldOp);
  });

  return funcOp;
}

mlir::func::FuncOp ClosureOutliningPass::createClosureDropFunction(
    ReussirClosureCreateOp op, llvm::Twine name, mlir::IRRewriter &rewriter) {
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  std::string dropName = (name + "_drop").str();
  mlir::ModuleOp moduleOp = getOperation();
  rewriter.setInsertionPointToEnd(moduleOp.getBody());
  ClosureBoxType closureBoxType = op.getClosureBoxType();

  RcType specializedRcType =
      RcType::get(rewriter.getContext(), closureBoxType, Capability::shared);
  // transparently use RcType as refType of the Rc-wrapped closure box
  mlir::FunctionType funcType = rewriter.getFunctionType(
      llvm::ArrayRef<mlir::Type>{specializedRcType}, {});
  auto funcOp =
      rewriter.create<mlir::func::FuncOp>(op.getLoc(), dropName, funcType);
  funcOp.setPrivate();
  funcOp->setAttr("llvm.linkage",
                  mlir::LLVM::LinkageAttr::get(rewriter.getContext(),
                                               mlir::LLVM::Linkage::Internal));

  // Create entry block for the function
  mlir::Block *entryBlock = funcOp.addEntryBlock();
  rewriter.setInsertionPointToStart(entryBlock);
  mlir::Value rcPtr = entryBlock->getArgument(0);
  mlir::Location loc = op.getLoc();

  // First, fetch dec on the rc pointer
  auto fetchDec = rewriter.create<ReussirRcFetchDecOp>(loc, rcPtr);

  // If fetch_dec result is 1, we then drop the closure box
  auto one = rewriter.create<mlir::arith::ConstantIndexOp>(loc, 1);
  auto isOne = rewriter.create<mlir::arith::CmpIOp>(
      loc, mlir::arith::CmpIPredicate::eq, fetchDec.getRefCount(), one);

  auto ifOp = rewriter.create<mlir::scf::IfOp>(loc, mlir::TypeRange{}, isOne,
                                               /*addThenRegion=*/true,
                                               /*addElseRegion=*/false);

  // Inside the then block: drop the closure box contents
  rewriter.setInsertionPointToStart(ifOp.thenBlock());

  // Use rc.borrow to get a reference to the closure box
  RefType closureBoxRefType =
      rewriter.getType<RefType>(closureBoxType, Capability::unspecified);
  mlir::Value closureBoxRef =
      rewriter.create<ReussirRcBorrowOp>(loc, closureBoxRefType, rcPtr);

  // Extract the cursor pointer from the closure box
  auto payloadTypes = closureBoxType.getPayloadTypes();
  RefType cursorRefType =
      rewriter.getType<RefType>(rewriter.getI8Type(), Capability::unspecified);
  mlir::Value cursorRef = rewriter.create<ReussirClosureCursorOp>(
      loc, cursorRefType, rewriter.getIndexAttr(0), closureBoxRef);

  // For each payload field, check if cursor is strictly greater than the field
  // pointer and if so, add a drop operation for the field
  for (size_t i = 0; i < payloadTypes.size(); ++i) {
    mlir::Type payloadType = payloadTypes[i];
    if (isTriviallyCopyable(payloadType))
      continue;
    RefType payloadRefType =
        rewriter.getType<RefType>(payloadType, Capability::unspecified);

    // Get reference to this payload field
    mlir::Value payloadRef = rewriter.create<ReussirClosureInspectPayloadOp>(
        loc, payloadRefType, rewriter.getIndexAttr(i), closureBoxRef);

    // Compare cursor with payload reference: if cursor > payload, the field
    // was initialized and needs to be dropped
    auto cursorGtPayload = rewriter.create<ReussirRefCmpOp>(
        loc, rewriter.getI1Type(), mlir::arith::CmpIPredicate::ugt, cursorRef,
        payloadRef);

    // Conditionally drop the field if it was initialized
    auto dropIfOp = rewriter.create<mlir::scf::IfOp>(
        loc, mlir::TypeRange{}, cursorGtPayload, /*addThenRegion=*/true,
        /*addElseRegion=*/false);
    rewriter.setInsertionPointToStart(dropIfOp.thenBlock());
    rewriter.create<ReussirRefDropOp>(loc, payloadRef);
    rewriter.create<mlir::scf::YieldOp>(loc);
    rewriter.setInsertionPointAfter(dropIfOp);
  }

  // Free the token after dropping all fields
  // Compute TokenType from RcBoxType using DataLayout
  RcBoxType rcBoxType = specializedRcType.getInnerBoxType();
  mlir::DataLayout dataLayout = mlir::DataLayout::closest(funcOp);
  size_t size = dataLayout.getTypeSize(rcBoxType).getFixedValue();
  size_t align = dataLayout.getTypeABIAlignment(rcBoxType);
  TokenType tokenType = TokenType::get(rewriter.getContext(), align, size);

  mlir::Value token =
      rewriter.create<ReussirRcReinterpretOp>(loc, tokenType, rcPtr);
  rewriter.create<ReussirTokenFreeOp>(loc, token);

  // Yield from the outer if
  rewriter.create<mlir::scf::YieldOp>(loc);

  // Return from the function (after the if)
  rewriter.setInsertionPointAfter(ifOp);
  rewriter.create<mlir::func::ReturnOp>(loc);

  return funcOp;
}

mlir::func::FuncOp ClosureOutliningPass::createClosureCloneFunction(
    ReussirClosureCreateOp op, llvm::Twine name, mlir::IRRewriter &rewriter) {
  mlir::OpBuilder::InsertionGuard guard(rewriter);
  std::string cloneName = (name + "_clone").str();
  mlir::ModuleOp moduleOp = getOperation();
  rewriter.setInsertionPointToEnd(moduleOp.getBody());
  ClosureBoxType closureBoxType = op.getClosureBoxType();

  RcType specializedRcType =
      RcType::get(rewriter.getContext(), closureBoxType, Capability::shared);
  // Clone function takes Rc<ClosureBox> and returns Rc<ClosureBox>
  mlir::FunctionType funcType =
      rewriter.getFunctionType(llvm::ArrayRef<mlir::Type>{specializedRcType},
                               llvm::ArrayRef<mlir::Type>{specializedRcType});
  auto funcOp =
      rewriter.create<mlir::func::FuncOp>(op.getLoc(), cloneName, funcType);
  funcOp.setPrivate();
  funcOp->setAttr("llvm.linkage",
                  mlir::LLVM::LinkageAttr::get(rewriter.getContext(),
                                               mlir::LLVM::Linkage::Internal));

  // Create entry block for the function
  mlir::Block *entryBlock = funcOp.addEntryBlock();
  rewriter.setInsertionPointToStart(entryBlock);
  mlir::Value srcRcPtr = entryBlock->getArgument(0);
  mlir::Location loc = op.getLoc();

  // Use ClosureAllocaOp to create a place for assembling new closure
  RefType closureBoxRefType =
      rewriter.getType<RefType>(closureBoxType, Capability::unspecified);

  // Allocate token for RcBox<ClosureBox>
  RcBoxType rcBoxType = specializedRcType.getInnerBoxType();
  mlir::DataLayout dataLayout = mlir::DataLayout::closest(funcOp);
  size_t size = dataLayout.getTypeSize(rcBoxType).getFixedValue();
  size_t align = dataLayout.getTypeABIAlignment(rcBoxType);
  TokenType tokenType = TokenType::get(rewriter.getContext(), align, size);
  mlir::Value token = rewriter.create<ReussirTokenAllocOp>(loc, tokenType);

  // Allocate assemble space on stack first
  mlir::Value dstClosureBoxRef =
      rewriter.create<ReussirClosureAllocaOp>(loc, closureBoxRefType);

  // Borrow the source closure box
  mlir::Value srcClosureBoxRef =
      rewriter.create<ReussirRcBorrowOp>(loc, closureBoxRefType, srcRcPtr);

  // Extract the cursor pointer from the source closure box
  auto payloadTypes = closureBoxType.getPayloadTypes();
  RefType cursorRefType =
      rewriter.getType<RefType>(rewriter.getI8Type(), Capability::unspecified);
  mlir::Value srcCursorRef = rewriter.create<ReussirClosureCursorOp>(
      loc, cursorRefType, rewriter.getIndexAttr(0), srcClosureBoxRef);

  // Transfer the closure box contents, including header and applied arguments
  rewriter.create<ReussirClosureTransferOp>(loc, srcClosureBoxRef,
                                            dstClosureBoxRef);

  // For each payload field, check if cursor is greater than the field
  // pointer and if so, copy the data and emit ownership acquisition
  for (size_t i = 0; i < payloadTypes.size(); ++i) {
    mlir::Type payloadType = payloadTypes[i];
    if (isTriviallyCopyable(payloadType))
      continue;
    RefType payloadRefType =
        rewriter.getType<RefType>(payloadType, Capability::unspecified);

    // Get reference to source payload field
    mlir::Value srcPayloadRef = rewriter.create<ReussirClosureInspectPayloadOp>(
        loc, payloadRefType, rewriter.getIndexAttr(i), srcClosureBoxRef);

    // Get reference to destination payload field
    mlir::Value dstPayloadRef = rewriter.create<ReussirClosureInspectPayloadOp>(
        loc, payloadRefType, rewriter.getIndexAttr(i), dstClosureBoxRef);

    // Compare cursor with payload reference: if cursor > payload, the field
    // was initialized and needs to be copied
    auto cursorGtPayload = rewriter.create<ReussirRefCmpOp>(
        loc, rewriter.getI1Type(), mlir::arith::CmpIPredicate::ugt,
        srcCursorRef, srcPayloadRef);

    // Conditionally copy the field if it was initialized
    auto copyIfOp = rewriter.create<mlir::scf::IfOp>(
        loc, mlir::TypeRange{}, cursorGtPayload, /*addThenRegion=*/true,
        /*addElseRegion=*/false);
    rewriter.setInsertionPointToStart(copyIfOp.thenBlock());

    // Emit ownership acquisition for non-trivially copyable types
    if (emitOwnershipAcquisition(dstPayloadRef, rewriter, loc).failed())
      funcOp.emitError("failed to emit ownership acquisition for payload");

    rewriter.create<mlir::scf::YieldOp>(loc);
    rewriter.setInsertionPointAfter(copyIfOp);
  }

  // Load the closure box value from the alloca
  mlir::Value closureBoxValue =
      rewriter.create<ReussirRefLoadOp>(loc, closureBoxType, dstClosureBoxRef);

  // Create the new Rc pointer using RcCreateOp
  mlir::Value newRcPtr = rewriter.create<ReussirRcCreateOp>(
      loc, specializedRcType, closureBoxValue, token,
      /*region=*/mlir::Value{}, /*vtable=*/mlir::FlatSymbolRefAttr{});

  // Return the new Rc pointer
  rewriter.create<mlir::func::ReturnOp>(loc, newRcPtr);

  return funcOp;
}

} // namespace

} // namespace reussir
