module Reussir.Diagnostic.LineCache (
    LineCache (..),
    SelectedLine (..),
    fromFile,
    selectLines,
) where

import Data.Int (Int64)
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.Text.Lazy qualified as LazyText

-- | A cache of lines in a file, indexed by their start offset.
newtype LineCache = LineCache (IntMap LazyText.Text)
    deriving (Show, Eq)

-- | A line selected from a file, with additional information about the selection.
data SelectedLine = SelectedLine
    { lineNumber :: Int64
    , lineText :: LazyText.Text
    , lineStartOffset :: Int64
    , lineEndOffset :: Int64
    , lineColStart :: Int64 -- inclusive
    , lineColEnd :: Int64 -- inclusive
    }
    deriving (Show, Eq)

{- | Populate line cache from file content.
line cache maps start positions (char index) to line
-}
fromFile :: LazyText.Text -> LineCache
fromFile file = LineCache $ IntMap.fromList $ go 0 (LazyText.lines file)
  where
    go _ [] = []
    go offset (l : ls) = (fromIntegral offset, l) : go (offset + LazyText.length l + 1) ls

-- | Select lines from the cache that overlap with the given range.
selectLines :: LineCache -> Int64 -> Int64 -> [SelectedLine]
selectLines (LineCache cache) charStart charEnd
    | charEnd <= charStart = []
    | otherwise =
        [ SelectedLine
            { lineNumber = lineNo
            , lineText = txt
            , lineStartOffset = start
            , lineEndOffset = lineEnd
            , lineColStart = scopeStart
            , lineColEnd = scopeEnd
            }
        | (lineNo, (start_, txt)) <- zip [1 ..] (IntMap.toAscList cache)
        , let start = fromIntegral start_
        , let lineEnd = start + LazyText.length txt
        , lineEnd > charStart
        , start < charEnd
        , let scopeStart = 1 + max 0 (charStart - start)
        , let scopeEnd = min (LazyText.length txt) (charEnd - start)
        ]
