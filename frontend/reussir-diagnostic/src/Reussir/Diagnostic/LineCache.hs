module Reussir.Diagnostic.LineCache where

import Data.Int (Int64)
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.Text.Lazy qualified as LazyText

newtype LineCache = LineCache (IntMap LazyText.Text)
    deriving (Show, Eq)

data SelectedLine = SelectedLine
    { lineNumber :: Int64
    , lineText :: LazyText.Text
    , lineStartOffset :: Int64
    , lineEndOffset :: Int64
    , lineScopeStart :: Int64
    , lineScopeEnd :: Int64
    }
    deriving (Show, Eq)

-- populate line cache from list of lines
-- line cache maps start positions (char index) to line
fromFile :: LazyText.Text -> LineCache
fromFile file = LineCache $ IntMap.fromList $ go 0 (LazyText.lines file)
  where
    go _ [] = []
    go offset (l : ls) = (fromIntegral offset, l) : go (offset + LazyText.length l + 1) ls

selectLines :: LineCache -> Int64 -> Int64 -> [SelectedLine]
selectLines (LineCache cache) charStart charEnd
    | charEnd <= charStart = []
    | otherwise =
        [ SelectedLine
            { lineNumber = lineNo
            , lineText = txt
            , lineStartOffset = start
            , lineEndOffset = lineEnd
            , lineScopeStart = scopeStart
            , lineScopeEnd = scopeEnd
            }
        | (lineNo, (start_, txt)) <- zip [1 ..] (IntMap.toAscList cache)
        , let start = fromIntegral start_
        , let lineEnd = start + LazyText.length txt
        , lineEnd > charStart
        , start < charEnd
        , let scopeStart = max 0 (charStart - start)
        , let scopeEnd = min (LazyText.length txt) (charEnd - start)
        ]
