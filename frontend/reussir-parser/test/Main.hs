module Main (main) where

import Reussir.Parser.LexerSpec qualified as LexerSpec
import Reussir.Parser.TypeSpec qualified as TypeSpec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    lexerSpec <- testSpec "Reussir.Parser.Lexer" LexerSpec.spec
    typeSpec <- testSpec "Reussir.Parser.Type" TypeSpec.spec
    defaultMain (testGroup "Reussir Parser Tests" [lexerSpec, typeSpec])
