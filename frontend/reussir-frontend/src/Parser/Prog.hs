module Parser.Prog where

import Parser.Stmt
import Parser.Types
import Parser.Types.Stmt

type Prog = [Stmt]

parseProg :: Parser Prog 
parseProg = space *> many parseStmt <* eof

