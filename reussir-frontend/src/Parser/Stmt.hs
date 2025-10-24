module Parser.Stmt where

import Data.List.NonEmpty

import Parser.Expr

import Parser.Types
import Parser.Types.Stmt

openBody :: Parser ()
openBody = char '{' *> space

closeBody :: Parser ()
closeBody = char '}' *> space

openParen :: Parser ()
openParen = char '(' *> space

closeParen :: Parser ()
closeParen = char ')' *> space

semicolon :: Parser ()
semicolon = char ';' *> space

comma :: Parser ()
comma = char ',' *> space

parseBody :: Parser (NonEmpty Stmt)
parseBody = do 
    first <- openBody *> parseStmt
    rest  <- many (semicolon *> parseStmt) <* closeBody

    return (first :| rest)

parseTypename :: Parser Typename
parseTypename = fmap (\(Identifier name) -> Typename name) parseIdentifier

parseTypedParam :: Parser (Identifier, Typename)
parseTypedParam = do 
    name <- parseIdentifier <* char ':' <* space 
    ty   <- parseTypename

    return (name, ty)

parseFuncDef :: Parser (GlobalStmt FuncStmt)
parseFuncDef = do 
    vis  <- optional (string "pub" *> space)
    name <- string "fn" *> space *> parseIdentifier <* openParen
    args <- optional $ parseTypedParam `sepBy` comma
    body <- closeParen *> parseBody

    return (Function _ _ _ _)

parseIfStmt :: Parser Stmt 
parseIfStmt = do 
    cond    <- string "if" *> space *> parseExpr 
    iftrue  <- parseBody
    iffalse <- string "else" *> space *> parseBody

    return (IfStmt cond iftrue iffalse)

parseFuncCall :: Parser Stmt
parseFuncCall = do
    name <- parseIdentifier <* openParen
    args <- optional ((:) <$> parseExpr <*> many (comma *> parseExpr)) <* closeParen

    return $ FuncCall name $ case args of
        Nothing -> []
        Just as -> as

parseGlobalStmt :: Parser AnyGlobalStmt
parseGlobalStmt = AnyGlobalStmt <$> parseFuncDef

parseStmt :: Parser Stmt
parseStmt = error "todo"

