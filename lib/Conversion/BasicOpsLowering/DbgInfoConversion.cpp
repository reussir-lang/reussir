
//===-- DbgInfoConversion.cpp - Reussir debug info conversion ---*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements lowering of Reussir debug info attributes to LLVM DI.
//
//===----------------------------------------------------------------------===//

#include "Reussir/Conversion/BasicOpsLowering.h"
#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirOps.h"

#include <llvm/ADT/TypeSwitch.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/TypeSize.h>
#include <mlir/Dialect/LLVMIR/LLVMAttrs.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/BuiltinOps.h>

namespace reussir {

namespace {

// Helper function to extract line and column from MLIR locations.
// Walks through FusedLoc to find the first FileLineColLoc with valid line info.
std::pair<unsigned, unsigned> extractLineCol(mlir::Location loc) {
  if (auto fileLoc = llvm::dyn_cast<mlir::FileLineColLoc>(loc))
    return {fileLoc.getLine(), fileLoc.getColumn()};
  if (auto fusedLoc = llvm::dyn_cast<mlir::FusedLoc>(loc)) {
    for (auto inner : fusedLoc.getLocations()) {
      auto [line, col] = extractLineCol(inner);
      if (line != 0)
        return {line, col};
    }
  }
  if (auto nameLoc = llvm::dyn_cast<mlir::NameLoc>(loc))
    return extractLineCol(nameLoc.getChildLoc());
  if (auto callSiteLoc = llvm::dyn_cast<mlir::CallSiteLoc>(loc))
    return extractLineCol(callSiteLoc.getCallee());
  return {0, 0};
}

mlir::Type getUnderlyingTypeFromDbgAttr(mlir::Attribute dbgAttr) {
  return llvm::TypeSwitch<mlir::Attribute, mlir::Type>(dbgAttr)
      .template Case<DBGFPTypeAttr>(
          [](DBGFPTypeAttr fptAttr) { return fptAttr.getInnerType(); })
      .template Case<DBGIntTypeAttr>(
          [](DBGIntTypeAttr intAttr) { return intAttr.getInnerType(); })
      .template Case<DBGRecordTypeAttr>(
          [](DBGRecordTypeAttr recAttr) { return recAttr.getUnderlyingType(); })
      .Default([](auto attr) { return mlir::Type{}; });
}

template <typename RetType = mlir::Attribute>
RetType translateDBGAttrToLLVM(mlir::ModuleOp moduleOp, mlir::Attribute dbgAttr,
                               mlir::LLVM::DIFileAttr diFile,
                               mlir::LLVM::DICompileUnitAttr diCU,
                               mlir::LLVM::LLVMFuncOp funcOp,
                               mlir::LLVM::DIScopeAttr funcScope,
                               mlir::Location loc) {
  mlir::DataLayout dataLayout{moduleOp};
  return llvm::dyn_cast_if_present<RetType>(
      llvm::TypeSwitch<mlir::Attribute, mlir::Attribute>(dbgAttr)
          .template Case<DBGFPTypeAttr>([&](DBGFPTypeAttr fptAttr) {
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(fptAttr.getInnerType());
            return mlir::LLVM::DIBasicTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_base_type,
                fptAttr.getDbgName(), sizeInBits, llvm::dwarf::DW_ATE_float);
          })
          .template Case<DBGIntTypeAttr>([&](DBGIntTypeAttr intAttr) {
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(intAttr.getInnerType());
            return mlir::LLVM::DIBasicTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_base_type,
                intAttr.getDbgName(), sizeInBits,
                intAttr.getIsSigned() ? llvm::dwarf::DW_ATE_signed
                                      : llvm::dwarf::DW_ATE_unsigned);
          })
          // TODO: we ignore boxed and variant for now
          .template Case<DBGRecordTypeAttr>([&](DBGRecordTypeAttr recAttr)
                                                -> mlir::Attribute {
            if (recAttr.getIsVariant())
              return nullptr;
            llvm::SmallVector<mlir::LLVM::DINodeAttr> members;
            size_t currentOffset = 0;
            for (auto element : recAttr.getMembers()) {
              auto memberAttr = llvm::dyn_cast<DBGRecordMemberAttr>(element);
              if (!memberAttr)
                return nullptr;
              auto memberTy = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                  moduleOp, memberAttr.getTypeAttr(), diFile, diCU, funcOp,
                  funcScope, loc);
              if (!memberTy)
                return nullptr;
              auto memberUnderlyingTy =
                  getUnderlyingTypeFromDbgAttr(memberAttr.getTypeAttr());
              if (!memberUnderlyingTy)
                return nullptr;
              auto sizeInBits =
                  dataLayout.getTypeSizeInBits(memberUnderlyingTy);
              auto alignInBytes =
                  dataLayout.getTypeABIAlignment(memberUnderlyingTy);
              currentOffset = llvm::alignTo(currentOffset, alignInBytes);
              // align current offset
              members.push_back(mlir::LLVM::DIDerivedTypeAttr::get(
                  moduleOp->getContext(), llvm::dwarf::DW_TAG_member,
                  memberAttr.getName(), memberTy, sizeInBits, alignInBytes * 8,
                  currentOffset * 8,
                  /*address space=*/std::nullopt, /*extraData=*/nullptr));
              currentOffset += sizeInBits / 8;
            }
            auto sizeInBits =
                dataLayout.getTypeSizeInBits(recAttr.getUnderlyingType());
            auto alignInBits =
                dataLayout.getTypeABIAlignment(recAttr.getUnderlyingType()) * 8;
#if LLVM_VERSION_MAJOR >= 22
            return mlir::LLVM::DICompositeTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_class_type,
                recAttr.getDbgName(), /*file=*/diFile, /*line=*/0, diCU,
                /*baseType=*/nullptr, /*flags=*/mlir::LLVM::DIFlags::Zero,
                sizeInBits, alignInBits,
                /*dataLocation=*/nullptr, /*rank=*/nullptr,
                /*allocated=*/nullptr, /*associated=*/nullptr, members);
#else
            return mlir::LLVM::DICompositeTypeAttr::get(
                moduleOp.getContext(), llvm::dwarf::DW_TAG_class_type,
                recAttr.getDbgName(), /*file=*/diFile, /*line=*/0, diCU,
                /*baseType=*/nullptr, /*flags=*/mlir::LLVM::DIFlags::Zero,
                sizeInBits, alignInBits, members, nullptr, nullptr, nullptr,
                nullptr);
#endif
          })
          .template Case<DBGSubprogramAttr>(
              [&](DBGSubprogramAttr spAttr) -> mlir::Attribute {
                auto linkageName = funcOp.getSymNameAttr();
                llvm::SmallVector<mlir::LLVM::DINodeAttr> argTypes;
                for (auto paramAttr : spAttr.getTypeParams()) {
                  auto paramTy = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                      moduleOp, paramAttr, diFile, diCU, funcOp, funcScope,
                      loc);
                  if (!paramTy)
                    return nullptr;
                  argTypes.push_back(paramTy);
                }
                // TODO: function type is not emitted now
                auto emptyRoutine = mlir::LLVM::DISubroutineTypeAttr::get(
                    moduleOp.getContext(), {});
                // Extract line/column from the function's location
                auto [line, col] = extractLineCol(loc);
                auto res = mlir::LLVM::DISubprogramAttr::get(
                    moduleOp.getContext(),
                    mlir::DistinctAttr::create(
                        mlir::UnitAttr::get(moduleOp.getContext())),
                    false,
                    mlir::DistinctAttr::create(
                        mlir::UnitAttr::get(moduleOp.getContext())),
                    diCU, diFile, spAttr.getRawName(), linkageName, diFile,
                    line, col, mlir::LLVM::DISubprogramFlags::Definition,
                    emptyRoutine, {}, {});

                return res;
              })
          .template Case<DBGLocalVarAttr>(
              [&](DBGLocalVarAttr localVarAttr) -> mlir::Attribute {
                auto underlyingTy =
                    getUnderlyingTypeFromDbgAttr(localVarAttr.getDbgType());
                auto varType = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                    moduleOp, localVarAttr.getDbgType(), diFile, diCU, funcOp,
                    funcScope, loc);
                if (!varType)
                  return nullptr;
                auto scope = funcScope ? funcScope : diFile;
                auto alignInBits =
                    dataLayout.getTypeABIAlignment(underlyingTy) * 8;
                // Extract line/column from the operation's location
                // Note: 5th parameter is 'arg' (argument number), not column.
                // For local variables (not function parameters), arg should be
                // 0.
                auto [line, col] = extractLineCol(loc);
                (void)col; // Column is not used in DILocalVariable
                return mlir::LLVM::DILocalVariableAttr::get(
                    moduleOp.getContext(), scope, localVarAttr.getVarName(),
                    diFile, line, /*arg=*/0, alignInBits, varType,
                    mlir::LLVM::DIFlags::Zero);
              })
          .template Case<DBGFuncArgAttr>(
              [&](DBGFuncArgAttr funcArgAttr) -> mlir::Attribute {
                auto underlyingTy =
                    getUnderlyingTypeFromDbgAttr(funcArgAttr.getDbgType());
                auto varType = translateDBGAttrToLLVM<mlir::LLVM::DITypeAttr>(
                    moduleOp, funcArgAttr.getDbgType(), diFile, diCU, funcOp,
                    funcScope, loc);
                if (!varType)
                  return nullptr;
                auto scope = funcScope ? funcScope : diFile;
                auto alignInBits =
                    dataLayout.getTypeABIAlignment(underlyingTy) * 8;
                // For function arguments, use the 1-based arg index
                auto [line, col] = extractLineCol(loc);
                (void)col;
                return mlir::LLVM::DILocalVariableAttr::get(
                    moduleOp.getContext(), scope, funcArgAttr.getArgName(),
                    diFile, line, funcArgAttr.getArgIndex(), alignInBits,
                    varType, mlir::LLVM::DIFlags::Zero);
              })
          .Default([](auto attr) { return attr; }));
}
} // namespace

void lowerFusedDBGAttributeInLocations(mlir::ModuleOp moduleOp) {
  auto context = moduleOp.getContext();
  auto fileBasename = mlir::dyn_cast_if_present<mlir::StringAttr>(
      moduleOp->getAttr("reussir.dbg.file_basename"));
  auto fileDirectory = mlir::dyn_cast_if_present<mlir::StringAttr>(
      moduleOp->getAttr("reussir.dbg.file_directory"));
  if (!fileBasename || !fileDirectory)
    return;
  auto llvmDIFileAttr =
      mlir::LLVM::DIFileAttr::get(context, fileBasename, fileDirectory);
  auto dbgCompileUnitAttr = mlir::LLVM::DICompileUnitAttr::get(
      mlir::DistinctAttr::create(mlir::UnitAttr::get(context)),
      llvm::dwarf::DW_LANG_C_plus_plus_20, llvmDIFileAttr,
      mlir::StringAttr::get(context, "reussir"), true,
      mlir::LLVM::DIEmissionKind::Full);
  mlir::OpBuilder builder(moduleOp);
  moduleOp->walk([&](mlir::LLVM::LLVMFuncOp funcOp) {
    mlir::LocationAttr funcLoc = funcOp->getLoc();
    if (auto fused =
            llvm::dyn_cast_if_present<mlir::FusedLocWith<DBGSubprogramAttr>>(
                funcLoc)) {

      auto subprogram = translateDBGAttrToLLVM<mlir::LLVM::DISubprogramAttr>(
          moduleOp, fused.getMetadata(), llvmDIFileAttr, dbgCompileUnitAttr,
          funcOp, nullptr, funcLoc);
      if (subprogram) {
        auto updated =
            mlir::FusedLoc::get(context, fused.getLocations(), subprogram);
        funcOp->setLoc(updated);
      }

      // Process function argument debug info attribute
      // Note: Use funcOp->getLoc() which has the updated subprogram
      auto updatedFuncLoc = funcOp->getLoc();
      if (auto dbgFuncArgsAttr =
              funcOp->getAttrOfType<mlir::ArrayAttr>("reussir.dbg_func_args")) {
        for (auto [idx, argAttr] :
             llvm::enumerate(dbgFuncArgsAttr.getValue())) {
          if (auto funcArgAttr = mlir::dyn_cast<DBGFuncArgAttr>(argAttr)) {
            auto translated =
                translateDBGAttrToLLVM<mlir::LLVM::DILocalVariableAttr>(
                    moduleOp, funcArgAttr, llvmDIFileAttr, dbgCompileUnitAttr,
                    funcOp, subprogram, updatedFuncLoc);
            if (translated && idx < funcOp.getNumArguments()) {
              auto argValue = funcOp.getArgument(idx);
              auto &entryBlock = funcOp.getBody().front();
              builder.setInsertionPointToStart(&entryBlock);
              builder.create<mlir::LLVM::DbgValueOp>(updatedFuncLoc, argValue,
                                                     translated);
            }
          }
        }
        // Remove the attribute after processing
        funcOp->removeAttr("reussir.dbg_func_args");
      }
      funcOp->walk([&](mlir::Operation *op) {
        mlir::LocationAttr opLoc = op->getLoc();
        if (auto innerFused =
                llvm::dyn_cast_if_present<mlir::FusedLoc>(opLoc)) {
          auto translated = translateDBGAttrToLLVM(
              moduleOp, innerFused.getMetadata(), llvmDIFileAttr,
              dbgCompileUnitAttr, funcOp, subprogram, opLoc);
          if (translated) {
            if (auto localVar = mlir::dyn_cast<mlir::LLVM::DILocalVariableAttr>(
                    translated)) {
              auto value = op->getResult(0);
              builder.setInsertionPointAfterValue(value);
              builder.create<mlir::LLVM::DbgValueOp>(op->getLoc(), value,
                                                     localVar);
            }
            auto updatedInnerLoc =
                mlir::FusedLoc::get(context, fused.getLocations(), translated);
            op->setLoc(updatedInnerLoc);
          }
        }
      });
    }
  });
}
} // namespace reussir
