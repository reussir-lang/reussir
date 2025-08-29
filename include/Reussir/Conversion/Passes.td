//===-- Passes.td - Conversion pass definition file --------*- tablegen -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file defines the conversion passes for the Reussir dialect.
//
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_CONVERSION_PASSES_TD
#define REUSSIR_CONVERSION_PASSES_TD

include "mlir/Pass/PassBase.td"

class ReussirLoweringPass<string mnemonic, string target = "">
    : Pass<"reussir-lowering-"#mnemonic, target>;

//===----------------------------------------------------------------------===//
// BasicOpsLowering
//===----------------------------------------------------------------------===//
def ReussirBasicOpsLoweringPass
    : ReussirLoweringPass<"basic-ops", "::mlir::ModuleOp"> {
  let summary = "lower straight-forward operations to MLIR basic operations";
  let description = [{
    `reussir-lowering-basic-ops` lowers straight-forward operations to MLIR basic
    operations.
  }];
  let dependentDialects = ["::mlir::arith::ArithDialect",
                           "::mlir::memref::MemRefDialect",
                           "::mlir::scf::SCFDialect",
                           "::mlir::math::MathDialect",
                           "::mlir::LLVM::LLVMDialect",
                           "::mlir::func::FuncDialect",
                           "::mlir::ub::UBDialect",
                           "::reussir::ReussirDialect"];
}

//===----------------------------------------------------------------------===//
// SCFOpsLowering
//===----------------------------------------------------------------------===//
def ReussirSCFOpsLoweringPass
    : ReussirLoweringPass<"scf-ops"> {
  let summary = "lower Reussir SCF operations to MLIR SCF operations";
  let description = [{
    `reussir-lowering-scf-ops` lowers Reussir SCF operations to MLIR SCF
    operations.
  }];
  let dependentDialects = ["::mlir::arith::ArithDialect",
                           "::reussir::ReussirDialect",
                           "::mlir::math::MathDialect",
                           "::mlir::scf::SCFDialect",
                           "::mlir::func::FuncDialect",
                           "::mlir::ub::UBDialect"];
}

//===----------------------------------------------------------------------===//
// DropExpansion
//===----------------------------------------------------------------------===//
def ReussirDropExpansionPass
    : Pass<"reussir-drop-expansion"> {
  let summary = "expand Reussir drop operations";
  let description = [{
    `reussir-drop-expansion` expands Reussir drop operations. This is a greedy
    pattern conversion pass.

    1. it elminates no-op drop operations.
    2. for normal drop, it inserts destruction operations for inner elements.

    Notice that this pass terminates normally for recursive types, as such types
    are backed by RC pointers. The decrement operation will not be inlined at
    this pass. After the expansion, the optimizer has chances to fuse increment
    and decrement operations. Eventually, remaining drop operations expanded from
    decrement operations will be outlined to avoid recursive inlining.

    To also expand rc decrement operations, use `expand-decrement` option. When
    enabled, conversion patterns from `rc-decrement-expansion` will also be applied
    in the greedy matching process.

    If you have recursive types, if `expand-decrement` is enabled, the pass may
    not terminate as dec-drop will be expanded infinitely. To avoid this, you
    should enable `outline-record` option, which converts record destructions to
    outline function calls.
  }];
  let options = [
    Option<"outlineRecord", "outline-record", "bool", /*default=*/"false",
        "whether to outline record destruction">,
    Option<"expandDecrement", "expand-decrement", "bool", /*default=*/"false",
        "whether to expand rc decrement operations (using the `rc-decrement-expansion` pass)">,   
  ];
  let dependentDialects = ["::reussir::ReussirDialect"];
}

//===----------------------------------------------------------------------===//
// RcDecrementExpansion
//===----------------------------------------------------------------------===//
def ReussirRcDecrementExpansionPass
    : Pass<"reussir-rc-decrement-expansion"> {
  let summary = "expand Reussir rc decrement operations";
  let description = [{
    `reussir-rc-decrement-expansion` expands Reussir rc decrement operations.
    This is separated from scf expansion pass as it may be used in early stages
    of the pipeline.
  }];

  let dependentDialects = ["::reussir::ReussirDialect", 
                           "::mlir::arith::ArithDialect", 
                           "::mlir::scf::SCFDialect"];
}

//===----------------------------------------------------------------------===//
// InferVariantTag
//===----------------------------------------------------------------------===//
def ReussirInferVariantTagPass
    : Pass<"reussir-infer-variant-tag", "::mlir::func::FuncOp"> {
  let summary = "infer variant tags for drop operations";
  let description = [{
    `reussir-infer-variant-tag` infers variant tags for drop operations.
  }];

  let dependentDialects = ["::reussir::ReussirDialect"];
}

//===----------------------------------------------------------------------===//
// IncDecCancelation
//===----------------------------------------------------------------------===//
def ReussirIncDecCancelationPass
    : Pass<"reussir-inc-dec-cancellation", "::mlir::ModuleOp"> {
  let summary = "cancel out adjacent increment and decrement operations";
  let description = [{
    `reussir-inc-dec-cancellation` cancels out adjacent increment and decrement
    operations.
  }];

  let dependentDialects = ["::reussir::ReussirDialect"];
}
#endif // REUSSIR_CONVERSION_PASSES_TD
