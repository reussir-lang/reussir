module Reussir.Parser.Types (
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Parser,
) where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String
