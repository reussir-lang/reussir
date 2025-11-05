//===-- ReussirInterfaces.td - Reussir Interfaces ----------*- tablegen -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces for the Reussir dialect.
//
//===----------------------------------------------------------------------===//

#ifndef REUSSIR_IR_REUSSIRINTERFACES_TD
#define REUSSIR_IR_REUSSIRINTERFACES_TD

include "mlir/IR/OpBase.td"
include "Reussir/IR/ReussirTypes.td"

//===----------------------------------------------------------------------===//
// TokenAcceptor Interface
//===----------------------------------------------------------------------===//

def TokenAcceptorInterface : OpInterface<"TokenAcceptor"> {
  let description = [{
    Interface for operations that can accept and manage tokens.
    Operations implementing this interface support token assignment, retrieval,
    and removal operations.
  }];

  let cppNamespace = "::reussir";

  let methods = [
    InterfaceMethod<
      /*desc=*/"Get the expected token type for this operation.",
      /*retTy=*/"::reussir::TokenType",
      /*methodName=*/"getTokenType",
      /*args=*/(ins),
      /*methodBody=*/"",
      /*defaultImplementation=*/""
    >,
    InterfaceMethod<
      /*desc=*/"Check whether the operation has been assigned a token.",
      /*retTy=*/"bool",
      /*methodName=*/"hasToken",
      /*args=*/(ins),
      /*methodBody=*/"",
      /*defaultImplementation=*/[{
        return $_op.getToken() != nullptr;
      }]
    >,
    InterfaceMethod<
      /*desc=*/"Assign a token value to this operation.",
      /*retTy=*/"void",
      /*methodName=*/"assignToken",
      /*args=*/(ins "::mlir::Value":$token),
      /*methodBody=*/"",
      /*defaultImplementation=*/[{
        $_op.getTokenMutable().assign(token);
      }]
    >,
    InterfaceMethod<
      /*desc=*/"Remove the token from this operation and return it.",
      /*retTy=*/"::mlir::Value",
      /*methodName=*/"removeToken",
      /*args=*/(ins),
      /*methodBody=*/"",
      /*defaultImplementation=*/[{
        ::mlir::Value token = $_op.getToken();
        $_op.getTokenMutable().clear();
        return token;
      }]
    >,
    InterfaceMethod<
      /*desc=*/"Get the current token value (may be null).",
      /*retTy=*/"::mlir::Value",
      /*methodName=*/"getToken",
      /*args=*/(ins),
      /*methodBody=*/"",
      /*defaultImplementation=*/""
    >,
    InterfaceMethod<
      /*desc=*/"Get mutable access to the token operand.",
      /*retTy=*/"::mlir::MutableOperandRange",
      /*methodName=*/"getTokenMutable",
      /*args=*/(ins),
      /*methodBody=*/"",
      /*defaultImplementation=*/""
    >
  ];
}

#endif // REUSSIR_IR_REUSSIRINTERFACES_TD
