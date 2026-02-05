module Reussir.Core.Semi.Function where

import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (writeIORef')
import Reussir.Parser.Types.Lexer (Path)

import Data.HashTable.IO qualified as H

import Reussir.Core.Data.Semi.Expr (Expr)
import Reussir.Core.Data.Semi.Function (FunctionProto (..), FunctionTable (..))

addFunctionProto ::
    (IOE :> es) => Path -> FunctionProto -> FunctionTable -> Eff es ()
addFunctionProto path proto (FunctionTable table) = do
    liftIO $ H.insert table path proto

getFunctionProto ::
    (IOE :> es) => Path -> FunctionTable -> Eff es (Maybe FunctionProto)
getFunctionProto path (FunctionTable table) = do
    liftIO $ H.lookup table path

addFunctionBody ::
    (IOE :> es, Prim :> es) => Path -> Maybe Expr -> FunctionTable -> Eff es ()
addFunctionBody path body (FunctionTable table) = do
    mProto <- liftIO $ H.lookup table path
    case mProto of
        Just proto -> writeIORef' (funcBody proto) body
        Nothing -> return ()

newFunctionTable :: (IOE :> es) => Eff es FunctionTable
newFunctionTable = FunctionTable <$> liftIO H.new
