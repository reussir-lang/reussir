module Reussir.Parser.Types (
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Parser,
) where

import Data.Void

import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text
