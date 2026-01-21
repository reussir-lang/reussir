{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Core2.Full.Pretty (
    PrettyColored (..),
) where

import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Effectful (Eff, IOE, (:>))
import Effectful.Prim (Prim)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Reussir.Codegen.Context.Symbol (Symbol, symbolText)
import Reussir.Codegen.Type.Data qualified as C
import Reussir.Core2.Data.FP (FloatingPointType (..))
import Reussir.Core2.Data.Full.Expr (Expr (..), ExprKind (..))
import Reussir.Core2.Data.Full.Function (Function (..))
import Reussir.Core2.Data.Full.Record (
    FieldFlag,
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core2.Data.Full.Type (Type (..))
import Reussir.Core2.Data.Integral (IntegralType (..))
import Reussir.Core2.Data.Operator (ArithOp (..), CmpOp (..))
import Reussir.Core2.Data.Semi.Type qualified as Semi (Flexivity (..), Type (..))
import Reussir.Core2.Data.String (StringToken (..))
import Reussir.Core2.Data.UniqueID (GenericID (..), HoleID (..), VarID (..))
import Reussir.Parser.Types.Capability qualified as Cap
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Reussir.Parser.Types.Stmt (Visibility (..))

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

class PrettyColored a where
    prettyColored :: (IOE :> es, Prim :> es) => a -> Eff es (Doc AnsiStyle)

-- Instances

instance PrettyColored Identifier where
    prettyColored (Identifier t) = pure $ variable (pretty t)

instance PrettyColored Symbol where
    prettyColored s = pure $ variable (pretty (symbolText s))

instance PrettyColored Path where
    prettyColored (Path base segs) = do
        segsDocs <- mapM prettyColored segs
        baseDoc <- prettyColored base
        pure $ concatWith (surround "::") (segsDocs ++ [baseDoc])

instance PrettyColored Cap.Capability where
    prettyColored Cap.Unspecified = pure mempty
    prettyColored Cap.Shared = pure $ keyword "shared"
    prettyColored Cap.Value = pure $ keyword "value"
    prettyColored Cap.Flex = pure $ keyword "flex"
    prettyColored Cap.Rigid = pure $ keyword "rigid"
    prettyColored Cap.Field = pure $ keyword "field"
    prettyColored Cap.Regional = pure $ keyword "regional"

instance PrettyColored Visibility where
    prettyColored Public = pure $ keyword "pub" <> space
    prettyColored Private = pure mempty

instance PrettyColored IntegralType where
    prettyColored (Signed n) = pure $ typeName $ "i" <> pretty n
    prettyColored (Unsigned n) = pure $ typeName $ "u" <> pretty n

instance PrettyColored FloatingPointType where
    prettyColored (IEEEFloat n) = pure $ typeName $ "f" <> pretty n
    prettyColored BFloat16 = pure $ typeName "bfloat16"
    prettyColored Float8 = pure $ typeName "float8"

-- For Semi Types (needed for generics in Record/Function)
instance PrettyColored Semi.Type where
    prettyColored (Semi.TypeRecord path args flex) = do
        pathDoc <- prettyColored path
        _ <- prettyColored flex -- flex ignored in output usually?
        argsDocs <- mapM prettyColored args
        pure $ pathDoc <> if null args then mempty else angles (commaSep argsDocs)
    prettyColored (Semi.TypeIntegral t) = prettyColored t
    prettyColored (Semi.TypeFP t) = prettyColored t
    prettyColored Semi.TypeBool = pure $ typeName "bool"
    prettyColored Semi.TypeStr = pure $ typeName "str"
    prettyColored Semi.TypeUnit = pure $ typeName "unit"
    prettyColored (Semi.TypeClosure args ret) = do
        argsDocs <- mapM prettyColored args
        retDoc <- prettyColored ret
        pure $ parens (commaSep argsDocs) <+> "->" <+> retDoc
    prettyColored (Semi.TypeGeneric (GenericID i)) = pure $ typeName $ "T" <> pretty i
    prettyColored (Semi.TypeHole (HoleID i)) = pure $ typeName $ "?" <> pretty i
    prettyColored Semi.TypeBottom = pure $ typeName "⊥"
    prettyColored (Semi.TypeNullable t) = do
        tDoc <- prettyColored t
        pure $ typeName "Nullable" <> angles tDoc

instance PrettyColored Semi.Flexivity where
    prettyColored Semi.Irrelevant = pure mempty
    prettyColored Semi.Flex = pure $ brackets $ keyword "flex"
    prettyColored Semi.Rigid = pure $ brackets $ keyword "rigid"
    prettyColored Semi.Regional = pure $ brackets $ keyword "regional"

-- Full Type
instance PrettyColored Type where
    prettyColored (TypeRecord sym) = do
        symDoc <- prettyColored sym
        pure symDoc
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = pure $ typeName "bool"
    prettyColored TypeStr = pure $ typeName "str"
    prettyColored TypeUnit = pure $ typeName "unit"
    prettyColored (TypeClosure args ret) = do
        argsDocs <- mapM prettyColored args
        retDoc <- prettyColored ret
        pure $ parens (commaSep argsDocs) <+> "->" <+> retDoc
    prettyColored TypeBottom = pure $ typeName "⊥"
    prettyColored (TypeNullable t) = do
        tDoc <- prettyColored t
        pure $ typeName "Nullable" <> angles tDoc
    prettyColored (TypeRc t cap) = do
        tDoc <- prettyColored t
        pure $ typeName "Rc" <> angles (tDoc <> comma <+> prettyCap cap)
      where
        prettyCap C.Shared = keyword "Shared"
        prettyCap C.Value = keyword "Value"
        prettyCap C.Flex = keyword "Flex"
        prettyCap C.Rigid = keyword "Rigid"
        prettyCap C.Regional = keyword "Regional"
        prettyCap C.Field = keyword "Field"
        prettyCap C.Unspecified = mempty

instance PrettyColored ArithOp where
    prettyColored Add = pure $ operator "+"
    prettyColored Sub = pure $ operator "-"
    prettyColored Mul = pure $ operator "*"
    prettyColored Div = pure $ operator "/"
    prettyColored Mod = pure $ operator "%"

instance PrettyColored CmpOp where
    prettyColored Lt = pure $ operator "<"
    prettyColored Gt = pure $ operator ">"
    prettyColored Lte = pure $ operator "<="
    prettyColored Gte = pure $ operator ">="
    prettyColored Equ = pure $ operator "=="
    prettyColored Neq = pure $ operator "!="

instance PrettyColored Expr where
    prettyColored expr = do
        kindDoc <- case exprKind expr of
            GlobalStr (StringToken (u1, u2, u3, u4)) -> pure $ literal $ "str_token(" <> pretty u1 <> ", " <> pretty u2 <> ", " <> pretty u3 <> ", " <> pretty u4 <> ")"
            Constant n -> pure $ literal (pretty (show n))
            Negate e -> do
                eDoc <- prettyColored e
                pure $ operator "-" <> parens eDoc
            Not e -> do
                eDoc <- prettyColored e
                pure $ operator "!" <> parens eDoc
            Arith e1 op e2 -> do
                e1Doc <- prettyColored e1
                opDoc <- prettyColored op
                e2Doc <- prettyColored e2
                pure $ parens (e1Doc <+> opDoc <+> e2Doc)
            Cmp e1 op e2 -> do
                e1Doc <- prettyColored e1
                opDoc <- prettyColored op
                e2Doc <- prettyColored e2
                pure $ parens (e1Doc <+> opDoc <+> e2Doc)
            Cast e t -> do
                eDoc <- prettyColored e
                tDoc <- prettyColored t
                pure $ keyword "cast" <> angles tDoc <> parens eDoc
            ScfIfExpr c t e -> do
                cDoc <- prettyColored c
                tDoc <- prettyColored t
                eDoc <- prettyColored e
                pure $
                    group $
                        keyword "if"
                            <+> cDoc
                            <+> keyword "then"
                            <> nest 4 (line <> tDoc)
                            <> line
                            <> keyword "else"
                            <> nest 4 (line <> eDoc)
            Var (VarID i) -> pure $ variable $ "v" <> pretty i
            Proj e indices -> do
                eDoc <- prettyColored e
                pure $ eDoc <> UV.foldMap (("." <>) . pretty) indices
            Assign e1 idx e2 -> do
                e1Doc <- prettyColored e1
                e2Doc <- prettyColored e2
                pure $ e1Doc <> "." <> pretty idx <+> operator "=" <+> e2Doc
            Let _ (VarID vid) name val body -> do
                nameDoc <- prettyColored name
                valTypeDoc <- prettyColored (exprType val)
                valDoc <- prettyColored val
                bodyDoc <- prettyColored body
                pure $
                    group $
                        keyword "let"
                            <+> variable ("v" <> pretty vid)
                            <+> parens nameDoc
                            <+> operator ":"
                            <+> valTypeDoc
                            <+> operator "="
                            <+> valDoc
                            <+> keyword "in"
                            <> line
                            <> bodyDoc
            FuncCall path tyArgs args regional -> do
                pathDoc <- prettyColored path
                tyArgsDocs <- mapM prettyColored tyArgs
                argsDocs <- mapM prettyColored args
                pure $
                    pathDoc
                        <> (if null tyArgs then mempty else angles (commaSep tyArgsDocs))
                        <> (if regional then "[regional]" else mempty)
                        <> parens (commaSep argsDocs)
            CompoundCall args -> do
                argsDocs <- mapM prettyColored args
                pure $
                    (keyword "compound" <> parens (commaSep argsDocs))
            VariantCall variant arg -> do
                argDoc <- prettyColored arg
                pure $
                    (keyword "variant" <> brackets (pretty variant) <> parens argDoc)
            Poison -> pure $ keyword "poison"
            RegionRun e -> do
                eDoc <- prettyColored e
                pure $ keyword "run_region" <> braces eDoc
            NullableCall (Just e) -> do
                eDoc <- prettyColored e
                pure $ keyword "nonnull" <> braces eDoc
            NullableCall Nothing -> pure $ keyword "null"
            IntrinsicCall path args -> do
                pathDoc <- prettyColored path
                argsDocs <- mapM prettyColored args
                pure $
                    keyword "intrinsic"
                        <> angles pathDoc
                        <> parens (commaSep argsDocs)
            RcWrap e -> do
                eDoc <- prettyColored e
                pure $ keyword "rc_wrap" <> parens eDoc

        case exprKind expr of
            Var _ -> pure kindDoc
            Let _ _ _ _ _ -> pure kindDoc
            ScfIfExpr _ _ _ -> pure kindDoc
            _ -> do
                tyDoc <- prettyColored (exprType expr)
                pure $ kindDoc <+> comment (":" <+> tyDoc)

instance PrettyColored Record where
    prettyColored (Record name rawPath tyParams fields kind _defCap _span) = do
        nameDoc <- prettyColored name
        rawPathDoc <- prettyColored rawPath
        genericsDoc <- prettyGenerics tyParams
        fieldsDoc <- prettyFields fields
        pure $
            keyword (case kind of StructKind -> "struct"; EnumKind -> "enum"; EnumVariant _ _ -> "enum_variant")
                <+> nameDoc
                <+> parens ("aka " <> rawPathDoc <> genericsDoc)
                <> fieldsDoc
      where
        prettyGenerics [] = pure mempty
        prettyGenerics gs = do
            gsDocs <- mapM prettyColored gs
            pure $ angles (commaSep gsDocs)

        prettyFields (Named fs) = do
            fsDocs <- mapM prettyField (V.toList fs)
            pure $ braces (nest 4 (hardline <> vsep (punctuate comma fsDocs)) <> hardline)
        prettyFields (Unnamed fs) = do
            fsDocs <- mapM prettyUnnamedField (V.toList fs)
            pure $ parens (commaSep fsDocs)
        prettyFields (Variants vs) = do
            vsDocs <- mapM prettyColored (V.toList vs)
            pure $ braces (nest 4 (hardline <> vsep (punctuate comma vsDocs)) <> hardline)

        prettyField (n, t, fld) = do
            nDoc <- prettyColored n
            fldDoc <- prettyFieldFlag fld
            tDoc <- prettyColored t
            pure $ nDoc <> operator ":" <+> fldDoc <> tDoc

        prettyUnnamedField (t, fld) = do
            fldDoc <- prettyFieldFlag fld
            tDoc <- prettyColored t
            pure $ fldDoc <> tDoc

        prettyFieldFlag :: (IOE :> es, Prim :> es) => FieldFlag -> Eff es (Doc AnsiStyle)
        prettyFieldFlag False = pure mempty
        prettyFieldFlag True = pure $ brackets (keyword "field") <> space

instance PrettyColored Function where
    prettyColored Function{..} = do
        visDoc <- prettyColored funcVisibility
        nameDoc <- prettyColored funcName
        instDoc <- prettyGenerics funcInstantiatedTyArgs
        paramsDoc <- mapM prettyArg funcParams
        retDoc <- prettyColored funcReturnType
        bodyDoc <- case funcBody of
            Just b -> do
                bDoc <- prettyColored b
                pure $ braces (nest 4 (hardline <> bDoc) <> hardline)
            Nothing -> pure $ operator ";"

        pure $
            visDoc
                <> (if funcIsRegional then keyword "regional" <> space else mempty)
                <> keyword "fn"
                    <+> nameDoc
                <> instDoc
                <> parens (commaSep paramsDoc)
                    <+> operator "->"
                    <+> retDoc
                    <+> bodyDoc
      where
        prettyGenerics [] = pure mempty
        prettyGenerics gs = do
            gsDocs <- mapM prettyColored gs
            pure $ angles (commaSep gsDocs)

        prettyArg (n, t) = do
            nDoc <- prettyColored n
            tDoc <- prettyColored t
            pure $ nDoc <> operator ":" <+> tDoc

commaSep :: [Doc AnsiStyle] -> Doc AnsiStyle
commaSep = concatWith (surround (comma <> space))
