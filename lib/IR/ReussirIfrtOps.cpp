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

#include <llvm/ADT/StringExtras.h>
#include <llvm/Support/BLAKE3.h>
#include <llvm/Support/MemoryBufferRef.h>
#include <mlir/Bytecode/BytecodeReader.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
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
        if (!llvm::isa<mlir::RankedTensorType>(type)) {
          func.emitOpError("kernel functions require ranked tensor signatures");
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
  return mlir::success();
}

mlir::LogicalResult ReussirPJRTJitCallOp::verify() {
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
  return mlir::success();
#else
  return emitOpError("requires an OpenXLA-enabled build");
#endif
}

mlir::LogicalResult
ReussirPJRTJitCallOp::verifySymbolUses(mlir::SymbolTableCollection &symbols) {
  auto code = symbols.lookupNearestSymbolFrom<ReussirIFRTBytecodeOp>(
      getOperation(), getCalleeAttr());
  if (!code)
    return emitOpError("callee must reference a reussir.ifrt.bytecode symbol");
#ifdef REUSSIR_ENABLE_OPENXLA
  auto signature = verifyIfrtCallSignature(
      getOperation(), getInputs().getTypes(), getOutputs().getTypes(),
      getControlInputs().getTypes(), getControlOutput().getType(), getDevices(),
      getIoAliases(), getDonatedInputIndices());
  if (mlir::failed(signature))
    return mlir::failure();
  auto module =
      mlir::parseSourceString<mlir::ModuleOp>(code.getBytecode(), getContext());
  if (!module || mlir::failed(verifyIfrtKernelModule(*module)))
    return emitOpError("callee must contain a standalone StableHLO kernel");
  auto main = module->lookupSymbol<mlir::func::FuncOp>("main");
  if (main.getFunctionType() != *signature)
    return emitOpError(
        "IFRT array shapes and dtypes must match the kernel signature");
#endif
  return mlir::success();
}

} // namespace reussir
