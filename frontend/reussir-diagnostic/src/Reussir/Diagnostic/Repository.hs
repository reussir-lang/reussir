module Reussir.Diagnostic.Repository where

import Data.Function ((&))
import Data.HashMap.Lazy as LazyHM
import Data.Int (Int64)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.IO qualified as LazyTextIO
import Data.Traversable (forM)
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Diagnostic.LineCache (LineCache, SelectedLine, fromFile, selectLines)

data File = File
    { filePath :: FilePath
    , fileContent :: LazyText.Text
    , fileLineCache :: LineCache
    }
    deriving (Show, Eq)

newtype Repository = Repository (LazyHM.HashMap FilePath File)
    deriving (Show)

createRepository :: (IOE :> es) => [FilePath] -> Eff es Repository
createRepository paths = do
    files <- forM paths $ \p -> do
        text <- liftIO $ LazyTextIO.readFile p
        pure (p, File p text (fromFile text))
    pure . Repository $ LazyHM.fromList files

lookup ::
    Repository ->
    (FilePath, Int64, Int64) ->
    [SelectedLine]
lookup (Repository table) (path, s, e) =
    LazyHM.lookup path table
        & maybe [] (\f -> selectLines (fileLineCache f) s e)
