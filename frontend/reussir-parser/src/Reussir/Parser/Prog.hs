module Reussir.Parser.Prog where

import Reussir.Parser.Stmt
import Reussir.Parser.Types
import Reussir.Parser.Types.Stmt

type Prog = [Stmt]

parseProg :: Parser Prog
parseProg = space *> many parseStmt <* eof
