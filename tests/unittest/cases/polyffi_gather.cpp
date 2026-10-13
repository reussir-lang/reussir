//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirOps.h"
#include <gtest/gtest.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

import reussir.test;

namespace reussir {
namespace {
constexpr llvm::StringLiteral layout = "e-p:64:64-i64:64-n8:16:32:64";
constexpr llvm::StringLiteral triple = "x86_64-unknown-linux-gnu";

void appendBitcode(mlir::ModuleOp destination, llvm::StringRef bytes) {
  mlir::OpBuilder builder(destination.getContext());
  builder.setInsertionPointToEnd(destination.getBody());
  auto type = mlir::RankedTensorType::get({static_cast<int64_t>(bytes.size())},
                                          builder.getI8Type());
  mlir::OperationState state(builder.getUnknownLoc(),
                             ReussirPolyFFIOp::getOperationName());
  state.addAttribute("compiledModule",
                     mlir::DenseElementsAttr::getFromRawBuffer(
                         type, llvm::ArrayRef(bytes.data(), bytes.size())));
  builder.create(state);
}

void appendModule(mlir::ModuleOp destination, const llvm::Module &source) {
  llvm::SmallVector<char> bytes;
  llvm::raw_svector_ostream stream(bytes);
  llvm::WriteBitcodeToFile(source, stream);
  appendBitcode(destination, llvm::StringRef(bytes.data(), bytes.size()));
}
} // namespace

TEST_F(ReussirTest, GatherPolyFFIEmptyAndSingleModule) {
  auto module = parse("module {}");
  ASSERT_TRUE(module);
  llvm::LLVMContext llvmContext;
  auto empty = gatherCompiledModules(*module, llvmContext, layout, triple);
  ASSERT_NE(empty, nullptr);
  EXPECT_TRUE(empty->empty());
  EXPECT_EQ(empty->getDataLayoutStr(), layout);
  EXPECT_EQ(empty->getTargetTriple().str(), triple);

  llvm::Module source("single", llvmContext);
  source.setTargetTriple(llvm::Triple(triple));
  new llvm::GlobalVariable(
      source, llvm::Type::getInt32Ty(llvmContext), true,
      llvm::GlobalValue::ExternalLinkage,
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvmContext), 42),
      "answer");
  appendModule(*module, source);
  // Without an explicit target override, preserve the bitcode's target.
  auto single = gatherCompiledModules(*module, llvmContext, layout);
  ASSERT_NE(single, nullptr);
  EXPECT_NE(single->getNamedGlobal("answer"), nullptr);
  EXPECT_EQ(single->getDataLayoutStr(), layout);
  EXPECT_EQ(single->getTargetTriple().str(), triple);
  EXPECT_TRUE(module->getOps<ReussirPolyFFIOp>().empty());
}

TEST_F(ReussirTest, GatherPolyFFIResolvesSymbolsAndPreservesMetadata) {
  auto module = parse("module {}");
  ASSERT_TRUE(module);
  llvm::LLVMContext llvmContext;
  auto *functionType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(llvmContext), false);
  // Three textures share a linkonce definition and metadata; a declaration in
  // the first is resolved by a definition in the last, across repeated links.
  for (unsigned i = 0; i < 3; ++i) {
    llvm::Module source("texture", llvmContext);
    source.setTargetTriple(llvm::Triple("aarch64-unknown-linux-gnu"));
    auto *callee = llvm::Function::Create(
        functionType, llvm::GlobalValue::ExternalLinkage, "callee", source);
    llvm::IRBuilder<> builder(llvmContext);
    if (i == 0) {
      auto *caller = llvm::Function::Create(
          functionType, llvm::GlobalValue::ExternalLinkage, "caller", source);
      builder.SetInsertPoint(llvm::BasicBlock::Create(llvmContext, "", caller));
      builder.CreateRet(builder.CreateCall(callee));
    } else if (i == 2) {
      builder.SetInsertPoint(llvm::BasicBlock::Create(llvmContext, "", callee));
      builder.CreateRet(builder.getInt32(42));
    }
    new llvm::GlobalVariable(source, builder.getInt32Ty(), true,
                             llvm::GlobalValue::LinkOnceODRLinkage,
                             builder.getInt32(7), "shared");
    source.getOrInsertNamedMetadata("textures")
        ->addOperand(llvm::MDNode::get(
            llvmContext, llvm::MDString::get(llvmContext, std::to_string(i))));
    appendModule(*module, source);
  }
  auto gathered = gatherCompiledModules(*module, llvmContext, layout, triple);
  ASSERT_NE(gathered, nullptr);
  EXPECT_FALSE(llvm::verifyModule(*gathered, &llvm::errs()));
  ASSERT_NE(gathered->getFunction("callee"), nullptr);
  EXPECT_FALSE(gathered->getFunction("callee")->isDeclaration());
  EXPECT_NE(gathered->getFunction("caller"), nullptr);
  EXPECT_NE(gathered->getNamedGlobal("shared"), nullptr);
  EXPECT_EQ(gathered->global_size(), 1u);
  ASSERT_NE(gathered->getNamedMetadata("textures"), nullptr);
  EXPECT_EQ(gathered->getNamedMetadata("textures")->getNumOperands(), 3u);
  EXPECT_EQ(gathered->getDataLayoutStr(), layout);
  EXPECT_EQ(gathered->getTargetTriple().str(), triple);
  EXPECT_TRUE(module->getOps<ReussirPolyFFIOp>().empty());
}

TEST_F(ReussirTest, GatherPolyFFIBadBitcodePreservesOperations) {
  auto module = parse("module {}");
  ASSERT_TRUE(module);
  llvm::LLVMContext llvmContext;
  llvm::Module source("valid", llvmContext);
  appendModule(*module, source);
  appendBitcode(*module, "not bitcode");
  EXPECT_EQ(gatherCompiledModules(*module, llvmContext, layout), nullptr);
  EXPECT_EQ(std::distance(module->getOps<ReussirPolyFFIOp>().begin(),
                          module->getOps<ReussirPolyFFIOp>().end()),
            2);
}
} // namespace reussir
