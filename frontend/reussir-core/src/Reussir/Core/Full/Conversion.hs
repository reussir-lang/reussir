module Reussir.Core.Full.Conversion where

import Control.Monad (when)
import Data.Foldable (forM_)
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)

import Data.HashTable.IO qualified as H
import Effectful.State.Static.Local qualified as State

import Reussir.Core.Data.Full.Context (FullContext (ctxTrampolines))
import Reussir.Core.Data.Generic (GenericSolution)
import Reussir.Core.Full.Context (addError, emptyFullContext)
import Reussir.Core.Full.Function (convertAllSemiFunctions)
import Reussir.Core.Full.Record (convertSemiRecordTable)

import Reussir.Core.Data.Semi.Context qualified as Semi
import Reussir.Core.Data.Semi.Function qualified as Semi
import qualified Data.HashMap.Strict as HashMap
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Parser.Types.Lexer (Identifier(Identifier))
import Reussir.Core.Semi.Mangle (mangleABIName)
import Reussir.Core.Data.Semi.Type (Type(TypeRecord))
import qualified Reussir.Core.Data.Semi.Type as Semi

convertCtx ::
    (IOE :> es, Prim :> es, Log :> es) =>
    Semi.SemiContext -> GenericSolution -> Eff es FullContext
convertCtx semiCtx sol = do
    emptyCtx <- emptyFullContext (Semi.currentFile semiCtx)
    State.execState emptyCtx $ do
        errs <- inject $ convertSemiRecordTable (Semi.knownRecords semiCtx) sol
        forM_ errs (inject . addError)
        when (null errs) $ do
            funcProtos <- liftIO $ H.toList (Semi.functionProtos $ Semi.functions semiCtx)
            inject $ convertAllSemiFunctions (map snd funcProtos) sol
            forM_ (HashMap.toList (Semi.trampolines semiCtx)) $ \(Identifier name, (path, abi, tyArgs)) -> do
                let symName = verifiedSymbol name
                let mangledTarget = mangleABIName $ TypeRecord path tyArgs Semi.Irrelevant
                let mangledSymbol = verifiedSymbol mangledTarget
                State.modify $ \ctx -> ctx { 
                    ctxTrampolines = HashMap.insert symName (abi, mangledSymbol) (ctxTrampolines ctx) }
