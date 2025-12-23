module Reussir.Parser.Types.Lexer where

import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text qualified as T

newtype Identifier = Identifier {unIdentifier :: T.Text}

data Path = Path
    { pathBasename :: Identifier
    , pathSegments :: [Identifier]
    }

instance Show Path where
    show (Path base segs) =
        intercalate "::" (map show (segs ++ [base]))

instance Show Identifier where
    show (Identifier name) = '$' : T.unpack name

data WithSpan a = WithSpan
    { wsValue :: a
    , spanStartOffset :: {-# UNPACK #-}  !Int64
    , spanEndOffset :: {-# UNPACK #-} !Int64
    }
    deriving (Show)
