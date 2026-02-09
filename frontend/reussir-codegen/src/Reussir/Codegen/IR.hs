module Reussir.Codegen.IR (
    Instr (..),
    FuncCall (..),
    Block (..),
    VariantDispData (..),
    Function (..),
    Linkage (..),
    LLVMVisibility (..),
    MLIRVisibility (..),
    YieldKind (..),
    instrCodegen,
    functionCodegen,
) where

import Reussir.Codegen.IR.Data
import Reussir.Codegen.IR.Emission (functionCodegen, instrCodegen)
