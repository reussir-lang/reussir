module Reussir.Core.Full.Conversion where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.HashTable.IO qualified as H
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Log (Log)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local qualified as State
import Reussir.Core.Data.Full.Context (FullContext)
import Reussir.Core.Data.Generic (GenericSolution)
import Reussir.Core.Data.Semi.Context qualified as Semi
import Reussir.Core.Data.Semi.Function qualified as Semi
import Reussir.Core.Full.Context (addError, emptyFullContext)
import Reussir.Core.Full.Function (convertAllSemiFunctions)
import Reussir.Core.Full.Record (convertSemiRecordTable)

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
