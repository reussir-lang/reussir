module Parser.Types.Stmt where

import Parser.Types.Expr

data FunctionVis = Public | Private deriving Show

data GlobalStmtTy = FuncStmt

data GlobalStmt (t :: GlobalStmtTy) where
    Function :: FunctionVis -> Identifier -> [(Identifier, Typename)] -> Maybe Typename -> Expr -> GlobalStmt FuncStmt

deriving instance Show (GlobalStmt t)

data AnyGlobalStmt = forall t . AnyGlobalStmt (GlobalStmt t)

instance Show AnyGlobalStmt where
    show (AnyGlobalStmt e) = show e

data Stmt = FunctionStmt (GlobalStmt FuncStmt)
