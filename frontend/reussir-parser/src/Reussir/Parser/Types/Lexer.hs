-- | Module for lexer-level types used in the Reussir parser.
module Reussir.Parser.Types.Lexer where

import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text qualified as T

-- | Represents an identifier in the source code.
-- Wraps a Text value containing the identifier's name.
newtype Identifier = Identifier {unIdentifier :: T.Text}
    deriving (Eq)

-- | Represents a namespace-qualified path (e.g., std::io::File).
-- The path is stored in a way that separates the basename from the segments.
data Path = Path
    { pathBasename :: Identifier  -- ^ The final component of the path (e.g., "File" in "std::io::File")
    , pathSegments :: [Identifier] -- ^ The namespace segments in order (e.g., ["std", "io"] in "std::io::File")
    }
    deriving (Eq)

instance Show Path where
    show (Path base segs) =
        intercalate "::" (map show (segs ++ [base]))

instance Show Identifier where
    show (Identifier name) = '$' : T.unpack name

-- | Wraps a value with its source span information.
-- Useful for tracking the location of parsed elements in the source code.
data WithSpan a = WithSpan
    { spanValue :: a                                  -- ^ The wrapped value
    , spanStartOffset :: {-# UNPACK #-} !Int64        -- ^ The byte offset where the span starts
    , spanEndOffset :: {-# UNPACK #-} !Int64          -- ^ The byte offset where the span ends
    }
    deriving (Show)
