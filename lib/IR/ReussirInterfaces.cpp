//===-- ReussirInterfaces.cpp - Reussir Interfaces -------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the interfaces for the Reussir dialect.
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirInterfaces.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Linker/Linker.h>
#include <llvm/Support/MemoryBuffer.h>
#include <mlir/Target/LLVMIR/LLVMTranslationInterface.h>
#include <mlir/Target/LLVMIR/ModuleTranslation.h>
#include <mlir/Transforms/InliningUtils.h>
/// Include the generated interface definitions.
#include "Reussir/IR/ReussirInterfaces.cpp.inc"

namespace reussir {
// namespace {
// using namespace mlir;
// struct ReussirLLVMTranslation : public mlir::LLVMTranslationDialectInterface
// {
//   using LLVMTranslationDialectInterface::LLVMTranslationDialectInterface;

//   LogicalResult
//   convertOperation(Operation *op, llvm::IRBuilderBase &builder,
//                    LLVM::ModuleTranslation &state) const override {
//     if (auto polyffi = dyn_cast<ReussirPolyFFIOp>(op)) {
//       if (!polyffi.getCompiledModule())
//         return op->emitError("PolyFFI operation has no compiled module");
//       auto denseI8array =
//           dyn_cast<DenseElementsAttr>(*polyffi.getCompiledModule());
//       if (!denseI8array)
//         return op->emitError("compiledModule is not a DenseElementsAttr");
//       llvm::ArrayRef<char> bitcodeData = denseI8array.getRawData();
//       std::unique_ptr<llvm::MemoryBuffer> memBuffer =
//           llvm::MemoryBuffer::getMemBuffer(
//               llvm::StringRef(bitcodeData.data(), bitcodeData.size()));
//       llvm::Expected<std::unique_ptr<llvm::Module>> moduleOrErr =
//           llvm::parseBitcodeFile(memBuffer->getMemBufferRef(),
//                                  state.getLLVMContext());
//       if (!moduleOrErr)
//         return op->emitError("PolyFFI operation has invalid bitcode");
//       std::unique_ptr<llvm::Module> innerModule = std::move(*moduleOrErr);
//       auto layout = state.getLLVMModule()->getDataLayout();
//       innerModule->setDataLayout(layout);
//       bool flag = llvm::Linker::linkModules(*state.getLLVMModule(),
//                                             std::move(innerModule));
//       if (flag)
//         return op->emitError("PolyFFI operation cannot be linked");
//       return success();
//     }
//     return failure();
//   }
// };
// } // namespace

namespace {
struct ReussirInlinerInterface : public mlir::DialectInlinerInterface {
  using DialectInlinerInterface::DialectInlinerInterface;
  bool isLegalToInline(mlir::Operation *, mlir::Region *, bool,
                       mlir::IRMapping &) const final {
    return true;
  }
};
} // namespace

void ReussirDialect::registerInterfaces() {
  // addInterfaces<ReussirLLVMTranslation>();
  addInterfaces<ReussirInlinerInterface>();
}
// void registerReussirDialectTranslation(DialectRegistry &registry) {
//   registry.insert<ReussirDialect>();
//   registry.addExtension(+[](MLIRContext *ctx, ReussirDialect *dialect) {
//     dialect->addInterfaces<ReussirLLVMTranslation>();
//   });
// }
} // namespace reussir
