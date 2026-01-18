{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Core2.Semi.Pretty (
    PrettyColored (..),
) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Reussir.Core2.Data.FP (FloatingPointType (..))
import Reussir.Core2.Data.Integral (IntegralType (..))
import Reussir.Core2.Data.Operator (ArithOp (..), CmpOp (..))
import Reussir.Core2.Data.Semi.Expr (Expr (..), ExprKind (..))
import Reussir.Core2.Data.Semi.Type (Type (..))
import Reussir.Core2.Data.String (StringToken (..))
import Reussir.Core2.Data.UniqueID (GenericID (..), HoleID (..), VarID (..))
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
    prettyColored (TypeRecord path args _flex) =
        -- Flexivity currently ignored in pretty printing
        prettyColored path <> if null args then mempty else angles (commaSep (map prettyColored args))
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = typeName "bool"
    prettyColored TypeStr = typeName "str"
    prettyColored TypeUnit = typeName "unit"
    prettyColored (TypeClosure args ret) = parens (commaSep (map prettyColored args)) <+> "->" <+> prettyColored ret
    prettyColored (TypeGeneric (GenericID i)) = typeName $ "T" <> pretty i
    prettyColored (TypeHole (HoleID i)) = typeName $ "?" <> pretty i
    prettyColored TypeBottom = typeName "⊥"
    prettyColored (TypeNullable t) = typeName "Nullable" <> angles (prettyColored t)

-- TypeRc and TypeRef no longer exist directly or are structured differently?
-- Checked Type.hs: TypeNullable exists. TypeRc/TypeRef seem gone from Semi/Type.hs list or I missed them.
-- Double checking Type.hs content from memory/history:
-- TypeRecord, TypeIntegral, TypeFP, TypeBool, TypeStr, TypeUnit, TypeClosure, TypeHole, TypeGeneric, TypeBottom, TypeNullable.
-- So Rc and Ref are indeed gone or handled via records/capabilities?
-- Proceeding with what matches Type.hs

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
                GlobalStr (StringToken (u1, u2, u3, u4)) -> literal $ "str_token(" <> pretty u1 <> ", " <> pretty u2 <> ", " <> pretty u3 <> ", " <> pretty u4 <> ")"
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
                -- RcWrap removed
                -- ProjChain replaced by Proj
                Proj e idx -> prettyColored e <> "." <> pretty idx
                Assign e1 idx e2 -> prettyColored e1 <> "." <> pretty idx <+> operator "=" <+> prettyColored e2
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
                FuncCall path tyArgs args regional ->
                    prettyColored path
                        <> (if null tyArgs then mempty else angles (commaSep (map prettyColored tyArgs)))
                        <> (if regional then "[regional]" else mempty)
                        <> parens (commaSep (map prettyColored args))
                CompoundCall path tyArgs args ->
                    prettyColored path
                        <> (if null tyArgs then mempty else angles (commaSep (map prettyColored tyArgs)))
                        <> parens (commaSep (map prettyColored args))
                VariantCall path tyArgs variant arg ->
                    prettyColored path
                        <> (if null tyArgs then mempty else angles (commaSep (map prettyColored tyArgs)))
                        <> brackets (pretty variant)
                        <> parens (prettyColored arg)
                Poison -> keyword "poison"
                RegionRun e -> keyword "run_region" <> braces (prettyColored e)
                NullableCall (Just e) -> keyword "nonnull" <> braces (prettyColored e)
                NullableCall Nothing -> keyword "null"
                IntrinsicCall path args ->
                    prettyColored path
                        <> parens (commaSep (map prettyColored args))
         in case exprKind expr of
                Let _ _ _ _ _ -> kindDoc
                ScfIfExpr _ _ _ -> kindDoc
                _ -> kindDoc <+> comment (":" <+> prettyColored (exprType expr))

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = concatWith (surround (comma <> space))
