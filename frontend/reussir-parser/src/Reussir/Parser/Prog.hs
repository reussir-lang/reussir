module Reussir.Parser.Prog where

import Reussir.Parser.Expr (parseExpr)
import Reussir.Parser.Lexer (space)
import Reussir.Parser.Stmt
import Reussir.Parser.Types hiding (space)
import Reussir.Parser.Types.Expr (Expr)
import Reussir.Parser.Types.Stmt

type Prog = [Stmt]

parseProg :: Parser Prog
parseProg = space *> many parseStmt <* eof

-- | REPL input can be either a statement or an expression
data ReplInput = ReplStmt Stmt | ReplExpr Expr
    deriving (Show, Eq)

{- | Parse REPL input: try statement first, then expression.
Uses 'try' on parseStmt to allow backtracking if it fails.
-}
parseReplInput :: Parser ReplInput
parseReplInput = space *> (try stmtInput <|> exprInput) <* eof
  where
    stmtInput = ReplStmt <$> parseStmt
    exprInput = ReplExpr <$> parseExpr
