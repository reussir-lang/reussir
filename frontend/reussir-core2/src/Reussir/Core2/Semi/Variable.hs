module Reussir.Core2.Semi.Variable where

import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim.IORef.Strict (Prim, readIORef', writeIORef')
import Reussir.Core2.Types.Semi.Type (Type)
import Reussir.Core2.Types.Semi.Variable (VarDef (..), VarTable (..))
import Reussir.Core2.Types.UniqueID (VarID (..))
import Reussir.Parser.Types.Lexer (Identifier)

{- |
    Create a new variable and return its ID
-}
newVariable ::
    (IOE :> es, Prim :> es) =>
    Identifier ->
    Maybe (Int64, Int64) ->
    Type ->
    VarTable ->
    Eff es VarID
newVariable varName varSpan varType varTable = do
    uniqueBinds <- readIORef' $ uniqueBindings varTable
    let varID = VarID $ fromIntegral $ Seq.length uniqueBinds
        varDef = VarDef{varName, varSpan, varType}
        localBinds = localBindings varTable
    writeIORef' (uniqueBindings varTable) $ uniqueBinds Seq.|> varDef
    liftIO $ H.insert localBinds varName varID
    return varID

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
