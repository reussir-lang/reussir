module Reussir.Core2.Full.Conversion where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.HashTable.IO qualified as H
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local qualified as State
import Reussir.Core2.Data.Full.Context (FullContext)
import Reussir.Core2.Data.Generic (GenericSolution)
import Reussir.Core2.Data.Semi.Context qualified as Semi
import Reussir.Core2.Data.Semi.Function qualified as Semi
import Reussir.Core2.Full.Context (addError, emptyFullContext)
import Reussir.Core2.Full.Function (convertAllSemiFunctions)
import Reussir.Core2.Full.Record (convertSemiRecordTable)

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
