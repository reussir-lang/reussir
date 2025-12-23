module Main (main) where

import Reussir.Parser.LexerSpec qualified as LexerSpec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spec <- testSpec "Reussir.Parser" LexerSpec.spec
    defaultMain spec
