module Reussir.Core.Function where

import Data.HashTable.IO qualified as H
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Prim (Prim)
import Reussir.Core.Types.Function (FunctionProto, FunctionTable (..))
import Reussir.Parser.Types.Lexer (Path)

addFunctionProto :: (IOE :> es) => Path -> FunctionProto -> FunctionTable -> Eff es ()
addFunctionProto path proto (FunctionTable table) = do
    liftIO $ H.insert table path proto

getFunctionProto :: (IOE :> es) => Path -> FunctionTable -> Eff es (Maybe FunctionProto)
getFunctionProto path (FunctionTable table) = do
    liftIO $ H.lookup table path

addFunctionBody :: (IOE :> es, Prim :> es) => Path -> FunctionProto -> FunctionTable -> Eff es ()
addFunctionBody path proto (FunctionTable table) = do
    liftIO $ H.insert table path proto

newFunctionTable :: (IOE :> es) => Eff es FunctionTable
newFunctionTable = FunctionTable <$> liftIO H.new
