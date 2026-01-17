module Reussir.Core2.Semi.Variable where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim.IORef.Strict (Prim, newIORef', readIORef', writeIORef')
import Reussir.Core2.Data.Semi.Type (Type)
import Reussir.Core2.Data.Semi.Variable (
    ChangeLog (..),
    VarDef (..),
    VarTable (..),
 )
import Reussir.Core2.Data.UniqueID (VarID (..))
import Reussir.Parser.Types.Lexer (Identifier)

newVariableTable :: (IOE :> es, Prim :> es) => Eff es VarTable
newVariableTable = do
    localBindings <- liftIO $ H.new
    uniqueBindings <- newIORef' mempty
    return $ VarTable{localBindings, uniqueBindings}

{- |
    Create a new variable and return its ID
-}
newVariable ::
    (IOE :> es, Prim :> es) =>
    Identifier ->
    Maybe (Int64, Int64) ->
    Type ->
    VarTable ->
    Eff es (VarID, ChangeLog)
newVariable varName varSpan varType varTable = do
    uniqueBinds <- readIORef' $ uniqueBindings varTable
    let varID = VarID $ fromIntegral $ Seq.length uniqueBinds
        varDef = VarDef{varName, varSpan, varType}
        localBinds = localBindings varTable
    writeIORef' (uniqueBindings varTable) $ uniqueBinds Seq.|> varDef
    prevLocal <- liftIO $ (,) varName <$> H.lookup localBinds varName
    liftIO $ H.insert localBinds varName varID
    let changeLog = ChangeLog{prevLocal, prevUnique = uniqueBinds}
    return (varID, changeLog)

-- | Get the type of a variable, assuming it exists
getVarType :: (Prim :> es) => VarID -> VarTable -> Eff es Type
getVarType (VarID idx) varTable = do
    uniqueBinds <- readIORef' $ uniqueBindings varTable
    let varDef = Seq.index uniqueBinds (fromIntegral idx)
    return $ varType varDef

-- | Lookup the locally effective variable with the given name
lookupVar :: (IOE :> es, Prim :> es) => Identifier -> VarTable -> Eff es (Maybe (VarID, Type))
lookupVar varName varTable = do
    let nameMap = localBindings varTable
    liftIO (H.lookup nameMap varName) >>= \case
        Just varID -> getVarType varID varTable >>= \ty -> pure (Just (varID, ty))
        Nothing -> do
            return Nothing

-- | Rollback the variable table to the given change log
rollbackVar :: (IOE :> es, Prim :> es) => ChangeLog -> VarTable -> Eff es ()
rollbackVar changeLog varTable = do
    let localBinds = localBindings varTable
    case prevLocal changeLog of
        (name, Just varID) -> liftIO $ H.insert localBinds name varID
        (name, Nothing) -> liftIO $ H.delete localBinds name
    writeIORef' (uniqueBindings varTable) $ prevUnique changeLog
