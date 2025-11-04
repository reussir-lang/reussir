{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Path (
    Path (..),
    pathSingleton,
    pathList,
)
where

import Data.Hashable (Hashable)
import Data.Interned (intern)
import Data.Interned.Text (InternedText)
import Data.Text qualified as T

newtype Path = Path [InternedText]
    deriving (Eq, Show, Hashable)

{- | Create a single-element path from a showable value.
  Example: pathSingleton 42 -> Path ["42"]
-}
pathSingleton :: T.Text -> Path
pathSingleton x = Path [intern x]

{- | Create a path from a list of showable values.
  Example: pathList [1, 2, 3] -> Path ["1", "2", "3"]
-}
pathList :: [T.Text] -> Path
pathList xs = Path (map intern xs)
