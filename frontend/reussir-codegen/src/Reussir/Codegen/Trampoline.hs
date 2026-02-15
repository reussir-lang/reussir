{-# LANGUAGE OverloadedStrings #-}
module Reussir.Codegen.Trampoline where
import qualified Data.Text as T
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Context.Codegen (Codegen)
import Reussir.Codegen.Context.Emission (emitBuilderLineM)
import qualified Data.Text.Builder.Linear as TB
import Reussir.Codegen.Context (Emission(emit))

data Trampoline = Trampoline {
    trampolineName :: Symbol,
    trampolineTarget :: Symbol,
    trampolineABI :: T.Text
}

trampolineCodegen :: Trampoline -> Codegen ()
trampolineCodegen (Trampoline name target abi) = emitBuilderLineM $ do
    let opName = "reussir.trampoline"
    let abi' = TB.fromText $ T.show abi
    name' <- emit name
    target' <- emit target
    return $ opName <> " " <> abi' <> " @" <> name' <> " = @" <> target' 
