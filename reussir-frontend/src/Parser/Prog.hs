module Parser.Prog where

import Parser.Stmt
import Parser.Types
import Parser.Types.Stmt

type Prog = [AnyGlobalStmt]

parseProg :: Parser Prog 
parseProg = many parseGlobalStmt <* eof

