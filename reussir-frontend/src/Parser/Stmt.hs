module Parser.Stmt where

import Data.Maybe

import Parser.Expr

import Parser.Types
import Parser.Types.Expr
import Parser.Types.Stmt

parseTypedParam :: Parser (Identifier, Typename)
parseTypedParam = do 
    name <- parseIdentifier <* char ':' <* space 
    ty   <- parseTypename

    return (name, ty)

parseFuncDef :: Parser (GlobalStmt FuncStmt)
parseFuncDef = do 
    vism <- optional (string "pub" *> space)
    name <- string "fn" *> space *> parseIdentifier <* openParen
    args <- optional $ parseTypedParam `sepBy` comma
    ret  <- closeParen *> optional (string "->" *> space *> parseTypename)
    body <- parseBody

    let vis = case vism of { Nothing -> Private; Just () -> Public }

    return (Function vis name (fromMaybe [] args) ret body)

parseGlobalStmt :: Parser AnyGlobalStmt
parseGlobalStmt = AnyGlobalStmt <$> parseFuncDef

parseStmt :: Parser Stmt
parseStmt = error "todo"

