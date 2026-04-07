{-# LANGUAGE OverloadedStrings #-}
module Reussir.Codegen.Trampoline (
    Trampoline(..),
    ImportTrampoline(..),
    trampolineCodegen,
    importTrampolineCodegen,
) where
import qualified Data.Text as T
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Context.Codegen (Codegen)
import Reussir.Codegen.Context.Emission (emitBuilderLineM)
import qualified Data.Text.Builder.Linear as TB
import Reussir.Codegen.Context (Emission(emit))

-- | Export trampoline: wraps a Reussir function for C ABI export.
data Trampoline = Trampoline {
    trampolineName :: Symbol,
    trampolineTarget :: Symbol,
    trampolineABI :: T.Text
}

-- | Import trampoline: declares an external C function callable from Reussir.
-- At MLIR level, this generates:
--   reussir.import_trampoline "C" @reussir_name = @c_name
data ImportTrampoline = ImportTrampoline {
    importTrampolineName :: Symbol,
    importTrampolineTarget :: Symbol,
    importTrampolineABI :: T.Text
}

trampolineCodegen :: Trampoline -> Codegen ()
trampolineCodegen (Trampoline name target abi) = emitBuilderLineM $ do
    let opName = "reussir.trampoline"
    let abi' = TB.fromText $ T.show abi
    name' <- emit name
    target' <- emit target
    return $ opName <> " " <> abi' <> " @" <> name' <> " = @" <> target'

-- | Emit an import trampoline operation.
-- Format: reussir.import_trampoline "C" @reussir_name = @c_name
importTrampolineCodegen :: ImportTrampoline -> Codegen ()
importTrampolineCodegen (ImportTrampoline name target abi) = emitBuilderLineM $ do
    let opName = "reussir.import_trampoline"
    let abi' = TB.fromText $ T.show abi
    name' <- emit name
    target' <- emit target
    return $ opName <> " " <> abi' <> " @" <> name' <> " = @" <> target'
