{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Core.Pretty (
    PrettyColored (..),
) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Reussir.Core.Types.Expr
import Reussir.Core.Types.GenericID (GenericID (..))
import Reussir.Core.Types.Type
import Reussir.Parser.Pretty (PrettyColored (..))

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

comment :: Doc AnsiStyle -> Doc AnsiStyle
comment = annotate (color Black <> bold) -- Using bright black (gray) for types/comments

-- Instances

instance PrettyColored IntegralType where
    prettyColored (Signed n) = typeName $ "i" <> pretty n
    prettyColored (Unsigned n) = typeName $ "u" <> pretty n

instance PrettyColored FloatingPointType where
    prettyColored (IEEEFloat n) = typeName $ "f" <> pretty n
    prettyColored BFloat16 = typeName "bfloat16"
    prettyColored Float8 = typeName "float8"

instance PrettyColored Type where
    prettyColored (TypeRecord path args) =
        prettyColored path <> if null args then mempty else angles (commaSep (map prettyColored args))
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = typeName "bool"
    prettyColored TypeStr = typeName "str"
    prettyColored TypeUnit = typeName "unit"
    prettyColored (TypeClosure args ret) = parens (commaSep (map prettyColored args)) <+> "->" <+> prettyColored ret
    prettyColored (TypeGeneric (GenericID i)) = typeName $ "T" <> pretty i
    prettyColored (TypeHole (HoleID i)) = typeName $ "?" <> pretty i
    prettyColored (TypeRc t cap) = typeName "Rc" <> angles (prettyColored t <> comma <+> prettyColored cap)
    prettyColored (TypeRef t cap) = operator "&" <> prettyColored cap <+> prettyColored t
    prettyColored TypeBottom = typeName "⊥"

instance PrettyColored ArithOp where
    prettyColored Add = operator "+"
    prettyColored Sub = operator "-"
    prettyColored Mul = operator "*"
    prettyColored Div = operator "/"
    prettyColored Mod = operator "%"

instance PrettyColored CmpOp where
    prettyColored Lt = operator "<"
    prettyColored Gt = operator ">"
    prettyColored Lte = operator "<="
    prettyColored Gte = operator ">="
    prettyColored Equ = operator "=="
    prettyColored Neq = operator "!="

instance PrettyColored Expr where
    prettyColored expr =
        let kindDoc = case exprKind expr of
                GlobalStr (idx, len) -> literal $ "str_token(" <> pretty idx <> ", " <> pretty len <> ")"
                Constant n -> literal (pretty (show n))
                Negate e -> operator "-" <> parens (prettyColored e)
                Not e -> operator "!" <> parens (prettyColored e)
                Arith e1 op e2 -> parens (prettyColored e1 <+> prettyColored op <+> prettyColored e2)
                Cmp e1 op e2 -> parens (prettyColored e1 <+> prettyColored op <+> prettyColored e2)
                Cast e t -> keyword "cast" <> angles (prettyColored t) <> parens (prettyColored e)
                ScfIfExpr c t e ->
                    group $
                        keyword "if"
                            <+> prettyColored c
                            <+> keyword "then"
                            <> nest 4 (line <> prettyColored t)
                            <> line
                            <> keyword "else"
                            <> nest 4 (line <> prettyColored e)
                Var (VarID i) -> variable $ "v" <> pretty i
                RcWrap e cap -> keyword "rc_wrap" <> angles (prettyColored cap) <> parens (prettyColored e)
                ProjChain e idxs -> prettyColored e <> mconcat (map (\i -> "." <> pretty i) idxs)
                Let _ (VarID vid) name val body ->
                    group $
                        keyword "let"
                            <+> variable ("v" <> pretty vid)
                            <+> parens (prettyColored name)
                            <+> operator ":"
                            <+> prettyColored (exprType val)
                            <+> operator "="
                            <+> prettyColored val
                            <+> keyword "in"
                            <> line
                            <> prettyColored body
                FuncCall path tyArgs args ->
                    prettyColored path
                        <> (if null tyArgs then mempty else angles (commaSep (map prettyColored tyArgs)))
                        <> parens (commaSep (map prettyColored args))
                CtorCall path tyArgs variant args ->
                    prettyColored path
                        <> (if null tyArgs then mempty else angles (commaSep (map prettyColored tyArgs)))
                        <> (case variant of Nothing -> mempty; Just v -> "::#" <> pretty v)
                        <> parens (commaSep (map prettyColored args))
                Poison -> keyword "poison"
         in kindDoc <+> comment (":" <+> prettyColored (exprType expr))

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = concatWith (surround (comma <> space))
