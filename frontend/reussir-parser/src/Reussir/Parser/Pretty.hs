{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Parser.Pretty (
    PrettyColored (..),
) where

import Prettyprinter
import Prettyprinter.Render.Terminal

import Data.Vector qualified as BV
import Data.Vector.Strict qualified as V

import Reussir.Parser.Types.Capability
import Reussir.Parser.Types.Expr hiding (Named, Unnamed)
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Stmt hiding (Named, Unnamed)
import Reussir.Parser.Types.Type

import Reussir.Parser.Types.Expr qualified as E
import Reussir.Parser.Types.Stmt qualified as S

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
        prettyColored path
            <> if null args then mempty else angles (commaSep (map prettyColored args))
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = typeName "bool"
    prettyColored TypeStr = typeName "str"
    prettyColored TypeUnit = typeName "unit"
    prettyColored (TypeArrow t1 t2) = parens (prettyColored t1 <+> "->" <+> prettyColored t2)
    prettyColored (TypeSpanned w) = prettyColored (spanValue w)
    prettyColored TypeBottom = typeName "⊥"

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
    prettyColored (Pattern kind guard) =
        prettyColored kind <> case guard of
            Just g -> space <> keyword "if" <+> prettyColored g
            Nothing -> emptyDoc

instance PrettyColored PatternKind where
    prettyColored WildcardPat = "_"
    prettyColored (BindPat i) = prettyColored i
    prettyColored (ConstPat c) = prettyColored c
    prettyColored (CtorPat path args ell isNamed) =
        prettyColored path
            <> if BV.null args && not ell
                then emptyDoc
                else
                    let open = if isNamed then lbrace else lparen
                        close = if isNamed then rbrace else rparen
                        argDocs = map prettyColored (BV.toList args)
                        ellDoc = if ell then [operator ".."] else []
                     in open <> commaSep (argDocs ++ ellDoc) <> close

instance PrettyColored PatternCtorArg where
    prettyColored (PatternCtorArg field kind) =
        case field of
            Just f -> prettyColored f <> colon <+> prettyColored kind
            Nothing -> prettyColored kind

instance PrettyColored Access where
    prettyColored (E.Named i) = "." <> prettyColored i
    prettyColored (E.Unnamed i) = "." <> pretty i

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
    prettyColored (Let name tyVal e1) =
        group $
            keyword "let"
                <+> prettyColored (spanValue name)
                <> prettyTy tyVal
                <+> operator "="
                <+> prettyColored e1
      where
        prettyTy Nothing = emptyDoc
        prettyTy (Just (t, True)) = operator ":" <+> brackets (keyword "flex") <+> prettyColored t
        prettyTy (Just (t, False)) = operator ":" <+> prettyColored t
    prettyColored (FuncCallExpr (FuncCall path tys args)) =
        prettyColored path
            <> (if null tys then mempty else angles (commaSep (map prettyTyArg tys)))
            <> parens (commaSep (map prettyColored args))
      where
        prettyTyArg Nothing = "_"
        prettyTyArg (Just t) = prettyColored t
    prettyColored (Lambda name t e) =
        operator "\\"
            <> prettyColored name
            <> operator ":"
            <+> prettyColored t
            <+> operator "->"
            <+> prettyColored e
    prettyColored (Match e cases) =
        keyword "match"
            <+> prettyColored e
            <+> braces (nest 4 (hardline <> vsep (map prettyCase $ V.toList cases)) <> hardline)
      where
        prettyCase (p, expr) = prettyColored p <+> operator "=>" <+> prettyColored expr
    prettyColored (ExprSeq es) =
        braces
            (nest 4 (hardline <> vsep (punctuate semi (map prettyColored es))) <> hardline)
    prettyColored (Var path) = prettyColored path
    prettyColored (SpannedExpr w) = prettyColored (spanValue w)
    prettyColored (RegionalExpr e) = keyword "regional" <+> braces (prettyColored e)
    prettyColored (AccessChain e accesses) = prettyColored e <> V.foldMap prettyColored accesses
    prettyColored (CtorCallExpr (CtorCall path tys args)) =
        prettyColored path
            <> (if null tys then mempty else angles (commaSep (map prettyTyArg tys)))
            <> braces (commaSep (map prettyArg args))
      where
        prettyTyArg Nothing = "_"
        prettyTyArg (Just t) = prettyColored t

        prettyArg (Just n, e) = prettyColored n <> colon <+> prettyColored e
        prettyArg (Nothing, e) = prettyColored e
    prettyColored (Assign e1 e2 e3) =
        prettyColored e1
            <> operator "->"
            <> prettyColored e2
            <> space
            <> operator ":="
            <> space
            <> prettyColored e3

instance PrettyColored Capability where
    prettyColored Unspecified = emptyDoc
    prettyColored Shared = keyword "shared"
    prettyColored Value = keyword "value"
    prettyColored Flex = keyword "flex"
    prettyColored Rigid = keyword "rigid"
    prettyColored Field = keyword "field"
    prettyColored Regional = keyword "regional"

instance PrettyColored Visibility where
    prettyColored Public = keyword "pub" <> space
    prettyColored Private = emptyDoc

instance PrettyColored Stmt where
    prettyColored (FunctionStmt (Function vis name generics args retType isRegional body)) =
        prettyColored vis
            <> (if isRegional then keyword "regional" <> space else emptyDoc)
            <> keyword "fn"
            <+> prettyColored name
            <> prettyGenerics generics
            <> parens (commaSep (map prettyArg args))
            <+> prettyRet retType
            <+> case body of
                Just b -> braces (nest 4 (hardline <> prettyColored b) <> hardline)
                Nothing -> operator ";"
      where
        prettyGenerics [] = emptyDoc
        prettyGenerics gs = angles (commaSep (map prettyGeneric gs))
        prettyGeneric (n, bounds) =
            prettyColored n
                <> if null bounds
                    then emptyDoc
                    else
                        operator ":" <+> concatWith (surround (operator "+")) (map prettyColored bounds)
        prettyArg (n, t, flx) = prettyColored n <> operator ":" <+> prettyFlex flx <> prettyColored t
        prettyRet Nothing = emptyDoc
        prettyRet (Just (t, flx)) = operator "->" <+> prettyFlex flx <> prettyColored t
        prettyFlex False = emptyDoc
        prettyFlex True = brackets (keyword "flex") <> space
    prettyColored (RecordStmt (Record name tyParams fields kind vis cap)) =
        prettyColored vis
            <> keyword (case kind of StructKind -> "struct"; EnumKind -> "enum")
            <> (case cap of Unspecified -> emptyDoc; _ -> space <> brackets (prettyColored cap))
            <+> prettyColored name
            <> prettyGenerics tyParams
            <> case fields of
                S.Unnamed fs ->
                    parens
                        (commaSep (V.map (\(WithSpan (t, f) _ _) -> prettyUnnamedField (t, f)) fs))
                S.Variants vs ->
                    braces
                        ( nest 4 (hardline <> vsep (punctuate comma (map prettyVariant $ V.toList vs)))
                            <> hardline
                        )
                S.Named fs ->
                    braces
                        ( nest 4 (hardline <> vsep (punctuate comma (map prettyField $ V.toList fs)))
                            <> hardline
                        )
      where
        prettyGenerics [] = emptyDoc
        prettyGenerics gs = angles (commaSep (map prettyGeneric gs))
        prettyGeneric (n, bounds) =
            prettyColored n
                <> if null bounds
                    then emptyDoc
                    else
                        operator ":" <+> concatWith (surround (operator "+")) (map prettyColored bounds)
        prettyVariant (WithSpan (n, ts) _ _) =
            prettyColored n
                <> if null ts then emptyDoc else parens (commaSep (V.map prettyColored ts))
        prettyField (WithSpan (n, t, fld) _ _) = prettyColored n <> operator ":" <+> prettyFieldFlag fld <> prettyColored t
        prettyUnnamedField (t, fld) = prettyFieldFlag fld <> prettyColored t
        prettyFieldFlag False = emptyDoc
        prettyFieldFlag True = brackets (keyword "field") <> space
    prettyColored (SpannedStmt w) = prettyColored (spanValue w)
    prettyColored (ExternTrampolineStmt (Identifier sym) abi func tys) =
        keyword "extern"
            <+> literal (dquotes (pretty abi))
            <+> keyword "trampoline"
            <+> literal (dquotes (pretty sym))
            <+> operator "="
            <+> prettyColored func
            <> if null tys then emptyDoc else angles (commaSep (map prettyColored tys))
            <> operator ";"

commaSep :: (Foldable t) => t (Doc AnsiStyle) -> Doc AnsiStyle
commaSep = concatWith (surround (comma <> space))
