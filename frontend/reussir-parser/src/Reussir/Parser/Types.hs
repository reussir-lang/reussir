module Reussir.Parser.Types (
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Parser,
) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text qualified as T

type Parser = Parsec Void T.Text
