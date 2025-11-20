//===-- CompilePolymorphicFFI.cpp - Compile polymorphic FFI pass --*- C++
//-*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the CompilePolymorphicFFI pass which compiles
// polymorphic FFI template into concrete LLVM bitcode and embeds them into the
// module.
//
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirTypes.h"
#include "mlir/IR/Attributes.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/Interfaces/DataLayoutInterfaces.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Pass/Pass.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRCOMPILEPOLYMORPHICFFIPASS
#include "Reussir/Conversion/Passes.h.inc"

namespace {

//===----------------------------------------------------------------------===//
// CompilePolymorphicFFI Pass
//===----------------------------------------------------------------------===//

class ReussirCompilePolymorphicFFIPass
    : public impl::ReussirCompilePolymorphicFFIPassBase<
          ReussirCompilePolymorphicFFIPass> {

  void formatInto(llvm::raw_ostream &os, mlir::StringAttr attr) {
    os << attr.getValue();
  }
  void formatInto(llvm::raw_ostream &os, mlir::TypeAttr type, llvm::Twine name,
                  bool isTopLevel = true) {
    llvm::TypeSwitch<mlir::Type>(type.getValue())
        .Case<mlir::IntegerType, mlir::FloatType, mlir::IndexType>(
            [&](mlir::Type ty) { os << ty; })
        .Case<RcType>([&](RcType rcType) {
          if (rcType.getCapability() != Capability::shared &&
              rcType.getCapability() != Capability::rigid)
            llvm::report_fatal_error("rc with other capabilities is not "
                                     "supported in FFI generation");
          llvm::StringRef typePrefix =
              (rcType.getCapability() == Capability::shared)
                  ? "::reussir_rt::rc::Rc"
                  : "::reussir_rt::region::rusty::RigidRc";
          llvm::Twine innerName = name + "Pointee";
          formatInto(os, mlir::TypeAttr::get(rcType.getElementType()),
                     innerName, false);
          os << "type " << name << " = " << typePrefix << "<" << innerName
             << ">\n";
        })
        .Case<RecordType>([&](RecordType ty) {
          mlir::DataLayout dataLayout(getOperation());
          mlir::SymbolTable symbolTable(getOperation());
          bool withoutCleanup = !isTopLevel || isTriviallyCopyable(ty);
          if (withoutCleanup) {
            os << "#[derive(Copy, Clone), repr(C, align("
               << dataLayout.getTypeABIAlignment(ty) << "))]\n";
            os << "struct " << name << "([u8; " << dataLayout.getTypeSize(ty)
               << "]);\n";
          } else {
            os << "// Non-trivial type may need custom drop implementations.\n";
            os << "#[repr(C, align(" << dataLayout.getTypeABIAlignment(ty)
               << "))]\n";
            os << "struct " << name << "([u8; " << dataLayout.getTypeSize(ty)
               << "]);\n";
          }
        })
        .Default([&](mlir::Type ty) {
          llvm::report_fatal_error("unsupported type in FFI generation");
        });
  }

public:
  using impl::ReussirCompilePolymorphicFFIPassBase<
      ReussirCompilePolymorphicFFIPass>::ReussirCompilePolymorphicFFIPassBase;

  void runOnOperation() override {
    llvm::SmallVector<ReussirPolyFFIOp> uncompiledOps;
    getOperation().walk([&](ReussirPolyFFIOp op) {
      if (!op.getCompiledModule().has_value())
        uncompiledOps.push_back(op);
    });
  }
};

} // namespace

} // namespace reussir
