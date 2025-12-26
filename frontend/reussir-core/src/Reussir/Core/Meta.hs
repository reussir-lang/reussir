module Reussir.Core.Meta where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (modifyIORef', newIORef', readIORef')
import Reussir.Core.Types.Meta
import Reussir.Core.Types.MetaID
import Reussir.Core.Types.Type (Type)
import Reussir.Parser.Types.Lexer (Path)

emptyMetaState :: (Prim :> es) => Eff es MetaState
emptyMetaState = MetaState <$> newIORef' Seq.empty

newMetaVar ::
    (IOE :> es, Prim :> es) =>
    T.Text ->
    Maybe (Int64, Int64) ->
    [Path] ->
    MetaState ->
    Eff es MetaID
newMetaVar metaName metaSpan metaBounds state = do
    links <- liftIO H.new
    let var = MetaVar{metaName, metaSpan, metaLinks = links, metaBounds}
        ref = getStateRef state
    varID <- fromIntegral . Seq.length <$> readIORef' ref
    modifyIORef' ref (Seq.|> var)
    pure $ MetaID varID

addLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    Maybe Type ->
    MetaState ->
    Eff es ()
addLink (MetaID varID) mType state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    liftIO $ H.insert (metaLinks var) (MetaID varID) mType

addDirectLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaState ->
    Eff es ()
addDirectLink (MetaID varID) state = addLink (MetaID varID) Nothing state

addCtorLink ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    Type ->
    MetaState ->
    Eff es ()
addCtorLink (MetaID varID) mType state = addLink (MetaID varID) (Just mType) state

getName ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaState ->
    Eff es T.Text
getName (MetaID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ metaName var

getSpan ::
    (IOE :> es, Prim :> es) =>
    MetaID ->
    MetaState ->
    Eff es (Maybe (Int64, Int64))
getSpan (MetaID varID) state = do
    let ref = getStateRef state
    vars <- readIORef' ref
    let var = Seq.index vars (fromIntegral varID)
    pure $ metaSpan var
