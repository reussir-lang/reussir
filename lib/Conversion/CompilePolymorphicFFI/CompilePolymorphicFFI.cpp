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

#include "Reussir/Conversion/Passes.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"
#include "Reussir/RustCompiler.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributeInterfaces.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/IR/OwningOpRef.h>
#include <mlir/IR/SymbolTable.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Target/LLVMIR/Import.h>

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
  void formatInto(llvm::raw_ostream &os, mlir::OpBuilder &builder,
                  mlir::TypeAttr type, llvm::Twine name,
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
          formatInto(os, builder, mlir::TypeAttr::get(rcType.getElementType()),
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
            os << "#[repr(C, align(" << dataLayout.getTypeABIAlignment(ty)
               << "))]\n";
            os << "struct " << name << "([u8; " << dataLayout.getTypeSize(ty)
               << "]);\n";
            os << "extern \"C\" {\n";
            {
              os << "#[link_name = \"" << ty.getDtorName().getValue() << "\"]\n"
                 << "unsafe fn " << "drop_in_place" << "(ptr: *mut " << name
                 << ");\n";
              os << "#[link_name = \"" << ty.getAcquireName().getValue()
                 << "\"]\n"
                 << "unsafe fn " << "acquire_in_place" << "(ptr: *mut " << name
                 << ");\n";
            }
            os << "}\n";
            os << "impl Drop for " << name << " {\n";
            os << "    fn drop(&mut self) {\n";
            os << "        unsafe { drop_in_place(self) };\n";
            os << "    }\n";
            os << "}\n";
            // clang-format off
            os << "impl Clone for " << name << " {\n";
            os << "    fn clone(&self) -> Self {\n";
            os << "        let mut raw_data = ::std::mem::MaybeUninit::uninit()\n";
            os << "        unsafe { ::std::ptr::copy_nonoverlapping(self, raw_data.as_mut_ptr(), " << dataLayout.getTypeSize(ty) << ") };\n";
            os << "        unsafe { acquire_in_place(raw_data.as_mut_ptr()) };\n";
            os << "        unsafe { raw_data.assume_init() }\n";
            os << "    }\n";
            os << "}\n";
            // clang-format on
            createDtorIfNotExists(getOperation(), ty, builder);
            emitOwnershipAcquisitionFuncIfNotExists(getOperation(), ty,
                                                    builder);
          }
        })
        .Default([](mlir::Type) {
          llvm::report_fatal_error("unsupported type in FFI generation");
        });
  }
  std::string monomorphize(ReussirPolyFFIOp op) {
    mlir::OpBuilder builder(getOperation());
    std::string buffer;
    llvm::raw_string_ostream os(buffer);
    size_t cursor = 0;
    size_t index = 0;
    constexpr llvm::StringRef SUBSTART = "[:";
    constexpr llvm::StringRef SUBEND = ":]";
    const llvm::StringRef text = op.getModuleTexture();
    bool inSubstitution = false;
    while (index < text.size()) {
      if (!inSubstitution) {
        if (text.substr(index, 2) != SUBSTART) {
          index++;
          continue;
        } else {
          inSubstitution = true;
          os << text.substr(cursor, index);
          index += 2;
          cursor = index;
        }
      } else {
        if (text.substr(index, 2) != SUBEND) {
          index++;
          continue;
        } else {
          inSubstitution = false;
          llvm::StringRef key = text.substr(cursor, index);
          mlir::Attribute attr = op.getSubstitutions().get(key);
          if (!attr) {
            os << SUBSTART << key << SUBEND;
          } else {
            llvm::TypeSwitch<mlir::Attribute>(attr)
                .Case<mlir::StringAttr>(
                    [&](mlir::StringAttr str) { os << str.getValue(); })
                .Case<mlir::IntegerAttr>(
                    [&](mlir::IntegerAttr value) { os << value.getValue(); })
                .Case([&](mlir::TypeAttr attr) {
                  formatInto(os, builder, attr, key, false);
                })
                .Default([](mlir::Attribute) {
                  llvm::report_fatal_error(
                      "unsupported attribute in substitution");
                });
          }
          index += 2;
          cursor = index;
        }
      }
    }
    os << text.substr(cursor);
    return buffer;
  }

public:
  using impl::ReussirCompilePolymorphicFFIPassBase<
      ReussirCompilePolymorphicFFIPass>::ReussirCompilePolymorphicFFIPassBase;

  void runOnOperation() override {
    llvm::LLVMContext context;
    llvm::SmallVector<ReussirPolyFFIOp> uncompiledOps;
    getOperation().walk(
        [&](ReussirPolyFFIOp op) { uncompiledOps.push_back(op); });
    for (ReussirPolyFFIOp op : uncompiledOps) {
      std::string monomorphized = monomorphize(op);
      std::unique_ptr<llvm::Module> module =
          compileRustSource(context, monomorphized);
      if (!module)
        return signalPassFailure();
      mlir::OwningOpRef<mlir::ModuleOp> translated =
          mlir::translateLLVMIRToModule(std::move(module),
                                        getOperation()->getContext());
      if (!translated)
        return signalPassFailure();

      // Get the current module and build symbol tables
      mlir::ModuleOp currentModule = getOperation();
      mlir::SymbolTable currentSymbolTable(currentModule);
      mlir::SymbolTable translatedSymbolTable(*translated);

      // First pass: delete symbol declarations in current module if the
      // imported module contains the definitions
      llvm::SmallVector<mlir::Operation *> toErase;
      for (auto &op : currentModule.getBody()->getOperations()) {
        if (auto symbolOp = mlir::dyn_cast<mlir::SymbolOpInterface>(op)) {
          mlir::StringAttr symbolName = symbolOp.getNameAttr();

          // Check if this is a declaration (not a definition)
          if (symbolOp.isDeclaration()) {
            // Check if the translated module has a definition for this symbol
            mlir::Operation *translatedOp =
                translatedSymbolTable.lookup(symbolName.getValue());
            if (translatedOp) {
              // Check if the translated operation is a definition
              if (auto translatedSymbol =
                      mlir::dyn_cast<mlir::SymbolOpInterface>(translatedOp)) {
                if (!translatedSymbol.isDeclaration()) {
                  toErase.push_back(&op);
                }
              }
            }
          }
        }
      }

      // Erase the declarations
      for (mlir::Operation *op : toErase)
        op->erase();

      // Second pass: inline all operations from the imported module into the
      // current module
      mlir::OpBuilder builder(currentModule.getBodyRegion());
      builder.setInsertionPointToEnd(currentModule.getBody());
      for (auto &op :
           llvm::make_early_inc_range(translated->getBody()->getOperations())) {
        // Skip the module terminator
        if (mlir::isa<mlir::ModuleOp>(op))
          continue;
        // Skip existing definitions in the current module
        if (auto symInterface = mlir::dyn_cast<mlir::SymbolOpInterface>(op))
          if (currentSymbolTable.lookup(symInterface.getName()))
            continue;
        // Clone the operation into the current module
        mlir::Operation *clonedOp = builder.clone(op);

        // Internalize Rust symbols to avoid conflicts
        if (auto linkageAttr = clonedOp->getAttrOfType<mlir::LLVM::LinkageAttr>(
                "llvm.linkage")) {
          if (linkageAttr.getLinkage() == mlir::LLVM::Linkage::External) {
            clonedOp->setAttr(
                "llvm.linkage",
                mlir::LLVM::LinkageAttr::get(builder.getContext(),
                                             mlir::LLVM::Linkage::WeakODR));
          }
        }
      }

      // Erase the polymorphic FFI operation now that it's been compiled
      op.erase();
    }
  }
};

} // namespace

} // namespace reussir
