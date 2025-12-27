module Main (main) where

import Reussir.Parser.ExprSpec qualified as ExprSpec
import Reussir.Parser.LexerSpec qualified as LexerSpec
import Reussir.Parser.StmtSpec qualified as StmtSpec
import Reussir.Parser.TypeSpec qualified as TypeSpec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    lexerSpec <- testSpec "Reussir.Parser.Lexer" LexerSpec.spec
    typeSpec <- testSpec "Reussir.Parser.Type" TypeSpec.spec
    stmtSpec <- testSpec "Reussir.Parser.Stmt" StmtSpec.spec
    exprSpec <- testSpec "Reussir.Parser.Expr" ExprSpec.spec
    defaultMain (testGroup "Reussir Parser Tests" [lexerSpec, typeSpec, stmtSpec, exprSpec])
