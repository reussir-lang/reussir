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
/// This file implements IFRT bytecode symbols and PJRT JIT call verification.
///
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/OpenXLATarget.h"
#include "Reussir/IR/ReussirOps.h"
#ifdef REUSSIR_ENABLE_OPENXLA
#include "stablehlo/dialect/StablehloOps.h"
#endif

#include <llvm/ADT/SmallBitVector.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/BLAKE3.h>
#include <llvm/Support/MemoryBufferRef.h>
#include <mlir/Bytecode/BytecodeReader.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Parser/Parser.h>

namespace reussir {

mlir::LogicalResult verifyIfrtKernelModule(mlir::ModuleOp module) {
  auto main = module.lookupSymbol<mlir::func::FuncOp>("main");
  if (!main || main.isDeclaration() || !main.isPublic())
    return module.emitError("kernel requires a defined public @main function");
  auto result = module.walk([&](mlir::Operation *op) -> mlir::WalkResult {
    auto dialect = op->getName().getDialectNamespace();
    if (dialect != "builtin" && dialect != "func" && dialect != "stablehlo" &&
        dialect != "sdy") {
      op->emitOpError("is not supported in a standalone StableHLO kernel");
      return mlir::WalkResult::interrupt();
    }
    if (auto func = llvm::dyn_cast<mlir::func::FuncOp>(op)) {
      if (func.isDeclaration()) {
        func.emitOpError("kernel cannot reference an external function");
        return mlir::WalkResult::interrupt();
      }
      for (auto type : llvm::concat<const mlir::Type>(func.getArgumentTypes(),
                                                      func.getResultTypes())) {
        bool valid = llvm::isa<mlir::RankedTensorType>(type);
#ifdef REUSSIR_ENABLE_OPENXLA
        // Future/Token are kernel SSA values, not PJRT input/output buffers.
        // Private helpers may pass them without exposing a host ABI.
        if (func.isPrivate()) {
          if (auto future = llvm::dyn_cast<mlir::stablehlo::FutureType>(type))
            valid = !future.getTypes().empty() &&
                    llvm::all_of(future.getTypes(), [](mlir::Type payload) {
                      return llvm::isa<mlir::RankedTensorType>(payload);
                    });
          else
            valid |= llvm::isa<mlir::stablehlo::TokenType>(type);
        }
#endif
        if (!valid) {
          func.emitOpError(
              func.isPrivate()
                  ? "kernel helper signatures require ranked tensors or "
                    "StableHLO Future/Token values"
                  : "public kernel functions require ranked tensor signatures");
          return mlir::WalkResult::interrupt();
        }
      }
    }
    return mlir::WalkResult::advance();
  });
  return mlir::failure(result.wasInterrupted());
}

mlir::LogicalResult ReussirIFRTBytecodeOp::verify() {
  auto bytes = getBytecode();
  if (!mlir::isBytecode(llvm::MemoryBufferRef(bytes, getSymName())))
    return emitOpError("requires MLIR bytecode");
  auto checksum = getChecksum();
  if (checksum.size() != 2 * LLVM_BLAKE3_OUT_LEN ||
      !llvm::all_of(checksum, llvm::isHexDigit))
    return emitOpError("requires a 64-character hexadecimal BLAKE3 checksum");
  llvm::BLAKE3 hasher;
  hasher.update(bytes);
  auto digest = hasher.final();
  if (!checksum.equals_insensitive(llvm::toHex(digest, /*LowerCase=*/true)))
    return emitOpError("BLAKE3 checksum does not match bytecode");
  if (auto cached = (*this)->getAttrOfType<mlir::TypeAttr>("kernel_type")) {
    auto module = mlir::parseSourceString<mlir::ModuleOp>(bytes, getContext());
    if (!module || mlir::failed(verifyIfrtKernelModule(*module)))
      return emitOpError("requires a standalone StableHLO kernel");
    auto main = module->lookupSymbol<mlir::func::FuncOp>("main");
    if (cached.getValue() != main.getFunctionType())
      return emitOpError("kernel_type does not match bytecode");
  }
  return mlir::success();
}

mlir::LogicalResult ReussirPJRTJitCallOp::verify() {
  if ((*this)->hasAttr("reussir.disable_donation") &&
      (!(*this)->hasAttrOfType<mlir::UnitAttr>("reussir.disable_donation") ||
       !getIoAliases().empty() || !getDonatedInputIndices().empty() ||
       !getDonationVariants().empty()))
    return emitOpError("donation opt-out requires a unit attribute and no donation contracts or variants");
#ifdef REUSSIR_ENABLE_OPENXLA
  if (mlir::failed(verifyIfrtCallSignature(
          getOperation(), getInputs().getTypes(), getOutputs().getTypes(),
          getControlInputs().getTypes(), getControlOutput().getType(),
          getDevices(), getIoAliases(), getDonatedInputIndices())))
    return mlir::failure();
  if (getArgAttrs() && getArgAttrs()->size() != getInputs().size())
    return emitOpError("arg_attrs must match the input count");
  if (getResAttrs() && getResAttrs()->size() != getOutputs().size())
    return emitOpError("res_attrs must match the output count");
  if (getDonationGuards().empty() != getDonationVariants().empty())
    return emitOpError(
        "donation guards and variants must be provided together");
  if (!getDonationGuards().empty() &&
      getDonationGuards().size() != getInputs().size())
    return emitOpError("donation guards must match input count");
  if (!getDonationVariants().empty() &&
      (!getIoAliases().empty() || !getDonatedInputIndices().empty()))
    return emitOpError(
        "guarded variants cannot override an explicit donation contract");
  for (auto attr : getDonationVariants()) {
    auto variant = llvm::dyn_cast<mlir::DictionaryAttr>(attr);
    auto arguments =
        variant ? variant.getAs<mlir::DenseI32ArrayAttr>("arguments") : nullptr;
    auto donors =
        variant ? variant.getAs<mlir::DenseI32ArrayAttr>("donors") : nullptr;
    auto callee =
        variant ? variant.getAs<mlir::FlatSymbolRefAttr>("callee") : nullptr;
    if (!arguments || !donors || !callee || donors.empty())
      return emitOpError(
          "variant requires callee, arguments, and nonempty donors");
    llvm::SmallBitVector present(getInputs().size());
    for (int32_t i : arguments.asArrayRef()) {
      if (i < 0 || unsigned(i) >= getInputs().size() || present.test(i))
        return emitOpError(
            "variant arguments must be distinct in-range indices");
      present.set(i);
    }
    llvm::SmallBitVector seen(getInputs().size());
    for (int32_t i : donors.asArrayRef()) {
      if (i < 0 || unsigned(i) >= getInputs().size() || !present.test(i) ||
          seen.test(i))
        return emitOpError("variant donors must be distinct argument indices");
      seen.set(i);
      auto view = getInputs()[i].getDefiningOp<ReussirArrayToIfrtOp>();
      if (view && !view.getMayDonate())
        return emitOpError("variant donor requires a donation-capable view");
    }
  }
  if (auto options = getCompileOptions())
    return verifyPjrtCompileOptions(getOperation(), *options);
  return mlir::success();
#else
  return emitOpError("requires an OpenXLA-enabled build");
#endif
}

mlir::LogicalResult ReussirPJRTAfterOp::verify() {
#ifdef REUSSIR_ENABLE_OPENXLA
  if (!isIfrtControlType(getControl().getType()) ||
      !llvm::all_of(getInputs().getTypes(), isIfrtArrayType))
    return emitOpError("requires IFRT arrays and a control result");
  return mlir::success();
#else
  return emitOpError("requires an OpenXLA-enabled build");
#endif
}

mlir::LogicalResult
ReussirPJRTJitCallOp::verifySymbolUses(mlir::SymbolTableCollection &symbols) {
  auto *symbol =
      symbols.lookupNearestSymbolFrom(getOperation(), getCalleeAttr());
  if (auto global = llvm::dyn_cast_or_null<mlir::LLVM::GlobalOp>(symbol)) {
    auto type =
        llvm::dyn_cast<mlir::LLVM::LLVMStructType>(global.getGlobalType());
    if (global.getConstant() && type && !type.isOpaque() &&
        type.getBody().size() == 2) {
      auto bytes = llvm::dyn_cast<mlir::LLVM::LLVMArrayType>(type.getBody()[1]);
      auto checksum =
          llvm::dyn_cast<mlir::LLVM::LLVMArrayType>(type.getBody()[0]);
      if (bytes && bytes.getElementType().isInteger(8) && checksum &&
          checksum.getElementType().isInteger(8) &&
          checksum.getNumElements() == 64)
        return mlir::success();
    }
    return emitOpError(
        "lowered callee must be a constant {checksum, bytecode} global");
  }
  auto code = llvm::dyn_cast_or_null<ReussirIFRTBytecodeOp>(symbol);
  if (!code)
    return emitOpError("callee must reference a reussir.ifrt.bytecode symbol");
#ifdef REUSSIR_ENABLE_OPENXLA
  auto signature = verifyIfrtCallSignature(
      getOperation(), getInputs().getTypes(), getOutputs().getTypes(),
      getControlInputs().getTypes(), getControlOutput().getType(), getDevices(),
      getIoAliases(), getDonatedInputIndices());
  if (mlir::failed(signature))
    return mlir::failure();
  auto kernelType = [&](ReussirIFRTBytecodeOp code) -> mlir::FunctionType {
    if (auto cached = code->getAttrOfType<mlir::TypeAttr>("kernel_type"))
      return llvm::dyn_cast<mlir::FunctionType>(cached.getValue());
    auto module = mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(),
                                                          getContext());
    if (!module || mlir::failed(verifyIfrtKernelModule(*module)))
      return {};
    return module->lookupSymbol<mlir::func::FuncOp>("main").getFunctionType();
  };
  if (kernelType(code) != *signature)
    return emitOpError(
        "IFRT array shapes and dtypes must match the kernel signature");
  for (auto attribute : getDonationVariants()) {
    auto variant = llvm::cast<mlir::DictionaryAttr>(attribute);
    auto reference = variant.getAs<mlir::FlatSymbolRefAttr>("callee");
    auto target = symbols.lookupNearestSymbolFrom<ReussirIFRTBytecodeOp>(
        getOperation(), reference);
    if (!target) {
      if (symbols.lookupNearestSymbolFrom<mlir::LLVM::GlobalOp>(getOperation(),
                                                                reference))
        continue;
      return emitOpError("donation variant must reference kernel bytecode");
    }
    llvm::SmallVector<mlir::Type> inputs;
    for (int32_t i :
         variant.getAs<mlir::DenseI32ArrayAttr>("arguments").asArrayRef())
      inputs.push_back(signature->getInput(i));
    if (kernelType(target) !=
        mlir::FunctionType::get(getContext(), inputs, signature->getResults()))
      return emitOpError(
          "donation variant signature does not match argument mapping");
  }
#endif
  return mlir::success();
}

} // namespace reussir
