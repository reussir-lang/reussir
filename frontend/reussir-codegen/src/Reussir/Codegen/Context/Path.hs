{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Context.Path (
    Path (..),
    pathSingleton,
    pathList,
    manglePath,
    manglePathWithPrefix,
)
where

import Data.Hashable (Hashable)
import Data.Interned (Uninternable (unintern), intern)
import Data.Interned.Text (InternedText)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB

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

-- ================================================================
-- Path Mangling
-- ================================================================
--
-- Formal Syntax:
-- <source-name> ::= <positive length number> <identifier>
-- <nested-name> ::= N [<source-name>] <type>* E
--
-- For a path "A::B::C", we mangle it as: N <source-name-A> <source-name-B> <source-name-C> E
-- Where each <source-name> is: <length> <identifier>

{- | Mangle a single path segment (identifier).
  Format: <length><identifier>
  Example: "My" -> "2My", "Path" -> "4Path"
-}
manglePathSegment :: InternedText -> TB.Builder
manglePathSegment text = TB.fromDec (T.length text') <> TB.fromText text'
  where
    text' = unintern text

{- | Mangle all segments of a path.
  Format: <segment1><segment2>...
  Each segment follows <source-name> format: <length><identifier>
-}
manglePath :: Path -> TB.Builder
manglePath (Path segments) = foldMap manglePathSegment segments

{- | Mangle a path with a prefix.
  Format: _Z<path>
  Example: "_ZMyPath"
-}
manglePathWithPrefix :: Path -> TB.Builder
manglePathWithPrefix path = "_Z" <> manglePath path
