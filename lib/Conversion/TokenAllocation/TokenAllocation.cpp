//===-- TokenAllocation.cpp - Token allocation pass -------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the TokenAllocation pass which inserts token allocation
// operations for TokenAcceptor operations that do not have a token assigned.
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirInterfaces.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/Operation.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRTOKENALLOCATIONPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {

class ReussirTokenAllocationPass
    : public impl::ReussirTokenAllocationPassBase<ReussirTokenAllocationPass> {
public:
  void runOnOperation() override {
    mlir::func::FuncOp funcOp = getOperation();
    
    // Walk through all operations in the function
    funcOp.walk([&](mlir::Operation *op) {
      // Check if this operation implements TokenAcceptor interface
      if (auto tokenAcceptor = dyn_cast<TokenAcceptor>(op)) {
        // Skip if already has a token
        if (tokenAcceptor.hasToken()) {
          return;
        }
        
        // Get the expected token type
        TokenType tokenType = tokenAcceptor.getTokenType();
        
        // Create a builder and insert allocation before the current operation
        mlir::OpBuilder builder(op);
        mlir::Location loc = op->getLoc();
        
        // Create the token allocation operation
        auto allocOp = builder.create<ReussirTokenAllocOp>(loc, tokenType);
        
        // Assign the token to the operation
        tokenAcceptor.assignToken(allocOp.getToken());
      }
    });
  }
};

} // namespace

} // namespace reussir
