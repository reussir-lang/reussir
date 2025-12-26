{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Parser.Pretty (
    PrettyColored (..),
) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer
import Reussir.Parser.Types.Stmt
import Reussir.Parser.Types.Type

-- Helper functions for styles
keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate (color Blue <> bold)

typeName :: Doc AnsiStyle -> Doc AnsiStyle
typeName = annotate (color Green)

literal :: Doc AnsiStyle -> Doc AnsiStyle
literal = annotate (color Yellow)

variable :: Doc AnsiStyle -> Doc AnsiStyle
variable = annotate (color White)

operator :: Doc AnsiStyle -> Doc AnsiStyle
operator = annotate (color Cyan)

class PrettyColored a where
    prettyColored :: a -> Doc AnsiStyle

-- Instances

instance PrettyColored Identifier where
    prettyColored (Identifier t) = variable (pretty t)

instance PrettyColored Path where
    prettyColored (Path base segs) =
        concatWith (surround "::") (map prettyColored segs ++ [prettyColored base])

instance PrettyColored IntegralType where
    prettyColored (Signed n) = typeName $ "i" <> pretty n
    prettyColored (Unsigned n) = typeName $ "u" <> pretty n

instance PrettyColored FloatingPointType where
    prettyColored (IEEEFloat n) = typeName $ "f" <> pretty n
    prettyColored BFloat16 = typeName "bfloat16"
    prettyColored Float8 = typeName "float8"

instance PrettyColored Type where
    prettyColored (TypeExpr path args) =
        prettyColored path <> if null args then mempty else angles (commaSep (map prettyColored args))
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = typeName "bool"
    prettyColored TypeStr = typeName "str"
    prettyColored TypeUnit = typeName "unit"
    prettyColored (TypeArrow t1 t2) = parens (prettyColored t1 <+> "->" <+> prettyColored t2)
    prettyColored (TypeSpanned w) = prettyColored (spanValue w)

instance PrettyColored Constant where
    prettyColored (ConstInt n) = literal (pretty n)
    prettyColored (ConstDouble n) = literal (pretty (show n))
    prettyColored (ConstString t) = literal (dquotes (pretty t))
    prettyColored (ConstBool b) = literal (if b then "true" else "false")

instance PrettyColored BinaryOp where
    prettyColored Add = operator "+"
    prettyColored Sub = operator "-"
    prettyColored Mul = operator "*"
    prettyColored Div = operator "/"
    prettyColored Mod = operator "%"
    prettyColored Lt = operator "<"
    prettyColored Gt = operator ">"
    prettyColored Lte = operator "<="
    prettyColored Gte = operator ">="
    prettyColored Equ = operator "=="
    prettyColored Neq = operator "!="
    prettyColored And = operator "&&"
    prettyColored Or = operator "||"

instance PrettyColored UnaryOp where
    prettyColored Negate = operator "-"
    prettyColored Not = operator "!"

instance PrettyColored Pattern where
    prettyColored (Pattern ns name args) =
        prettyColored ns <> "::" <> prettyColored name <> parens (commaSep (map prettyColored args))

instance PrettyColored Expr where
    prettyColored (ConstExpr c) = prettyColored c
    prettyColored (BinOpExpr op e1 e2) = parens (prettyColored e1 <+> prettyColored op <+> prettyColored e2)
    prettyColored (UnaryOpExpr op e) = parens (prettyColored op <> prettyColored e)
    prettyColored (If e1 e2 e3) =
        group $
            keyword "if"
                <+> prettyColored e1
                <+> keyword "then"
                <> nest 4 (line <> prettyColored e2)
                <> line
                <> keyword "else"
                <> nest 4 (line <> prettyColored e3)
    prettyColored (Cast t e) = keyword "cast" <> angles (prettyColored t) <> parens (prettyColored e)
    prettyColored (LetIn name e1 e2) =
        group $
            keyword "let"
                <+> prettyColored name
                <+> operator "="
                <+> prettyColored e1
                <+> keyword "in"
                <> line
                <> prettyColored e2
    prettyColored (FuncCall path args) = prettyColored path <> parens (commaSep (map prettyColored args))
    prettyColored (Lambda name t e) = operator "\\" <> prettyColored name <> operator ":" <+> prettyColored t <+> operator "->" <+> prettyColored e
    prettyColored (Match e cases) =
        keyword "match" <+> prettyColored e <+> braces (nest 4 (hardline <> vsep (map prettyCase cases)) <> hardline)
      where
        prettyCase (p, expr) = prettyColored p <+> operator "=>" <+> prettyColored expr
    prettyColored (Var path) = prettyColored path
    prettyColored (SpannedExpr w) = prettyColored (spanValue w)

instance PrettyColored Visibility where
    prettyColored Public = keyword "pub" <> space
    prettyColored Private = emptyDoc

instance PrettyColored Stmt where
    prettyColored (Function vis name generics args retType body) =
        prettyColored vis
            <> keyword "fn"
                <+> prettyColored name
            <> prettyGenerics generics
            <> parens (commaSep (map prettyArg args))
                <+> prettyRet retType
                <+> braces (nest 4 (hardline <> prettyColored body) <> hardline)
      where
        prettyGenerics [] = emptyDoc
        prettyGenerics gs = angles (commaSep (map prettyColored gs))
        prettyArg (n, t) = prettyColored n <> operator ":" <+> prettyColored t
        prettyRet Nothing = emptyDoc
        prettyRet (Just t) = operator "->" <+> prettyColored t
    prettyColored (RecordStmt (Record name tyParams fields kind vis)) =
        prettyColored vis
            <> keyword (case kind of StructKind -> "struct"; EnumKind -> "enum")
                <+> prettyColored name
            <> prettyGenerics tyParams
            <> case fields of
                Unamed fs -> parens (commaSep (map (prettyColored . fst) fs))
                Variants vs -> braces (nest 4 (hardline <> vsep (punctuate comma (map prettyVariant vs))) <> hardline)
                Named fs -> braces (nest 4 (hardline <> vsep (punctuate comma (map prettyField fs))) <> hardline)
      where
        prettyGenerics [] = emptyDoc
        prettyGenerics gs = angles (commaSep (map prettyColored gs))
        prettyVariant (n, ts) = prettyColored n <> if null ts then emptyDoc else parens (commaSep (map prettyColored ts))
        prettyField (n, t, _) = prettyColored n <> operator ":" <+> prettyColored t
    prettyColored (SpannedStmt w) = prettyColored (spanValue w)

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = concatWith (surround (comma <> space))
