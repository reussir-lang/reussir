{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Core.Semi.Pretty (
    PrettyColored (..),
    Style (..),
    styleToAnsi,
    styleToSGR,
    renderAnsi,
    docToFormattedText,
    keyword,
    typeName,
    literal,
    variable,
    operator,
    comment,
) where

import Control.Monad (zipWithM)
import Data.Foldable (toList)
import Effectful (Eff, IOE, (:>))
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (readIORef')
import Prettyprinter
import Prettyprinter.Render.Terminal
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Stmt (Visibility (..))

import Data.HashMap.Strict qualified as Data.HashMap.Strict
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Reussir.Diagnostic.Report (TextWithFormat (..))
import Reussir.Parser.Types.Capability qualified as Cap
import System.Console.ANSI.Codes (SGR (..))
import System.Console.ANSI.Types qualified as ANSI

import Reussir.Core.Data.FP (FloatingPointType (..))
import Reussir.Core.Data.Integral (IntegralType (..))
import Reussir.Core.Data.Operator (ArithOp (..), CmpOp (..))
import Reussir.Core.Data.Semi.Expr (
    DTSwitchCases (..),
    DecisionTree (..),
    Expr (..),
    ExprKind (..),
    PatternVarRef (..),
 )
import Reussir.Core.Data.Semi.Function (FunctionProto (..))
import Reussir.Core.Data.Semi.Record (
    FieldFlag,
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core.Data.Semi.Type (Flexivity (..), Type (..))
import Reussir.Core.Data.String (StringToken (..))
import Reussir.Core.Data.UniqueID (GenericID (..), HoleID (..), VarID (..))

-- Semantic Style Definition
data Style
    = StyleKeyword
    | StyleTypeName
    | StyleLiteral
    | StyleVariable
    | StyleOperator
    | StyleComment
    deriving (Show, Eq)

-- Helper functions for styles
keyword :: Doc Style -> Doc Style
keyword = annotate StyleKeyword

typeName :: Doc Style -> Doc Style
typeName = annotate StyleTypeName

literal :: Doc Style -> Doc Style
literal = annotate StyleLiteral

variable :: Doc Style -> Doc Style
variable = annotate StyleVariable

operator :: Doc Style -> Doc Style
operator = annotate StyleOperator

comment :: Doc Style -> Doc Style
comment = annotate StyleComment

class PrettyColored a where
    prettyColored :: (IOE :> es, Prim :> es) => a -> Eff es (Doc Style)

-- Instances

instance (PrettyColored a) => PrettyColored (Maybe a) where
    prettyColored Nothing = pure mempty
    prettyColored (Just x) = prettyColored x

instance PrettyColored Identifier where
    prettyColored (Identifier t) = pure $ variable (pretty t)

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

instance PrettyColored Flexivity where
    prettyColored Irrelevant = pure mempty
    prettyColored Flex = pure $ brackets $ keyword "flex"
    prettyColored Rigid = pure $ brackets $ keyword "rigid"
    prettyColored Regional = pure $ brackets $ keyword "regional"

instance PrettyColored Type where
    prettyColored (TypeRecord path args flex) = do
        pathDoc <- prettyColored path
        flexDoc <- prettyColored flex
        argsDocs <- mapM prettyColored args
        -- Flexivity currently ignored in pretty printing
        pure $
            pathDoc <> flexDoc <> if null args then mempty else angles (commaSep argsDocs)
    prettyColored (TypeIntegral t) = prettyColored t
    prettyColored (TypeFP t) = prettyColored t
    prettyColored TypeBool = pure $ typeName "bool"
    prettyColored TypeStr = pure $ typeName "str"
    prettyColored TypeUnit = pure $ typeName "unit"
    prettyColored (TypeClosure args ret) = do
        argsDocs <- mapM prettyColored args
        retDoc <- prettyColored ret
        pure $ parens (commaSep argsDocs) <+> "->" <+> retDoc
    prettyColored (TypeGeneric (GenericID i)) = pure $ typeName $ "T" <> pretty i
    prettyColored (TypeHole (HoleID i)) = pure $ typeName $ "?" <> pretty i
    prettyColored TypeBottom = pure $ typeName "⊥"
    prettyColored (TypeNullable t) = do
        tDoc <- prettyColored t
        pure $ typeName "Nullable" <> angles tDoc

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
            GlobalStr (StringToken (u1, u2, u3, u4)) ->
                pure $
                    literal $
                        "str_token("
                            <> pretty u1
                            <> ", "
                            <> pretty u2
                            <> ", "
                            <> pretty u3
                            <> ", "
                            <> pretty u4
                            <> ")"
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
                pure $ e1Doc <> brackets (pretty idx) <+> operator ":=" <+> e2Doc
            Let _ (VarID vid) name val -> do
                nameDoc <- prettyColored name
                valTypeDoc <- prettyColored (exprType val)
                valDoc <- prettyColored val
                pure $
                    group $
                        keyword "let"
                            <+> variable ("v" <> pretty vid)
                            <+> parens nameDoc
                            <+> operator ":"
                            <+> valTypeDoc
                            <+> operator "="
                            <+> valDoc
            FuncCall path tyArgs args regional -> do
                pathDoc <- prettyColored path
                tyArgsDocs <- mapM prettyColored tyArgs
                argsDocs <- mapM prettyColored args
                pure $
                    pathDoc
                        <> (if null tyArgs then mempty else angles (commaSep tyArgsDocs))
                        <> (if regional then "[regional]" else mempty)
                        <> parens (commaSep argsDocs)
            CompoundCall path tyArgs args -> do
                pathDoc <- prettyColored path
                tyArgsDocs <- mapM prettyColored tyArgs
                argsDocs <- mapM prettyColored args
                pure $
                    pathDoc
                        <> (if null tyArgs then mempty else angles (commaSep tyArgsDocs))
                        <> parens (commaSep argsDocs)
            VariantCall path tyArgs variant arg -> do
                pathDoc <- prettyColored path
                tyArgsDocs <- mapM prettyColored tyArgs
                argDoc <- prettyColored arg
                pure $
                    pathDoc
                        <> (if null tyArgs then mempty else angles (commaSep tyArgsDocs))
                        <> brackets (pretty variant)
                        <> parens argDoc
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
                    pathDoc
                        <> parens (commaSep argsDocs)
            Match val dt -> do
                valDoc <- prettyColored val
                dtDoc <- prettyColored dt
                pure $
                    keyword "match"
                        <+> valDoc
                        <+> braces (nest 4 (hardline <> dtDoc) <> hardline)
            Sequence [singleton] -> prettyColored singleton
            Sequence subexprs -> do
                subexprsDocs <- mapM prettyColored subexprs
                pure $
                    braces (nest 4 (hardline <> vsep (punctuate semi subexprsDocs)) <> hardline)

        case exprKind expr of
            Var _ -> pure kindDoc
            Let _ _ _ _ -> pure kindDoc
            Sequence _ -> pure kindDoc
            ScfIfExpr _ _ _ -> pure kindDoc
            Match _ _ -> pure kindDoc
            _ -> do
                tyDoc <- prettyColored (exprType expr)
                pure $ kindDoc <+> comment (":" <+> tyDoc)

instance PrettyColored DecisionTree where
    prettyColored DTUncovered = pure $ keyword "uncovered"
    prettyColored DTUnreachable = pure $ keyword "unreachable"
    prettyColored (DTLeaf body bindings) = do
        binds <- mapM prettyBind (IntMap.toList bindings)
        bodyDoc <- prettyColored body
        pure $ vsep (binds ++ [bodyDoc])
      where
        prettyBind (v, PatternVarRef path) =
            pure $
                keyword "pattern var"
                    <+> variable ("v" <> pretty v)
                    <+> parens (commaSep (map pretty (toList path)))
    prettyColored (DTGuard _ guard trueBr falseBr) = do
        guardDoc <- prettyColored guard
        trueDoc <- prettyColored trueBr
        falseDoc <- prettyColored falseBr
        pure $
            vsep
                [ keyword "if" <+> guardDoc <+> braces (nest 4 (hardline <> trueDoc) <> hardline)
                , keyword "otherwise" <+> braces (nest 4 (hardline <> falseDoc) <> hardline)
                ]
    prettyColored (DTSwitch (PatternVarRef p) cases) = do
        casesDoc <- prettyColored cases
        pure $
            keyword "switch"
                <+> parens (commaSep (map pretty (toList p)))
                <+> braces (nest 4 (hardline <> casesDoc) <> hardline)

instance PrettyColored DTSwitchCases where
    prettyColored (DTSwitchInt m def) = do
        casesDocs <- mapM prettyCase (IntMap.toList m)
        defDoc <- prettyColored def
        pure $
            vsep $
                casesDocs
                    ++ [ keyword "_"
                            <+> operator "=>"
                            <+> braces (nest 4 (hardline <> defDoc) <> hardline)
                       ]
      where
        prettyCase (i, dt) = do
            dtDoc <- prettyColored dt
            pure $
                literal (pretty i)
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> dtDoc) <> hardline)
    prettyColored (DTSwitchBool t f) = do
        tDoc <- prettyColored t
        fDoc <- prettyColored f
        pure $
            vsep
                [ literal "true"
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> tDoc) <> hardline)
                , literal "false"
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> fDoc) <> hardline)
                ]
    prettyColored (DTSwitchCtor cases) = do
        -- This is a simplification as we don't have easy access to ctor names here purely from index
        -- In a real implementation we might want to look up names if possible or print indices
        casesDocs <- zipWithM prettyCase [0 ..] (V.toList cases)
        pure $
            vsep $
                casesDocs
      where
        prettyCase i dt = do
            dtDoc <- prettyColored dt
            pure $
                variable ("ctor@" <> pretty (i :: Int))
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> dtDoc) <> hardline)
    prettyColored (DTSwitchString m def) = do
        defDoc <- prettyColored def
        casesDocs <- mapM prettyCase (Data.HashMap.Strict.toList m)
        pure $
            vsep $
                casesDocs
                    ++ [ keyword "_"
                            <+> operator "=>"
                            <+> braces (nest 4 (hardline <> defDoc) <> hardline)
                       ]
      where
        prettyCase (h, dt) = do
            dtDoc <- prettyColored dt
            pure $
                literal ("hash(" <> pretty (show h) <> ")")
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> dtDoc) <> hardline)
    prettyColored (DTSwitchNullable j n) = do
        jDoc <- prettyColored j
        nDoc <- prettyColored n
        pure $
            vsep
                [ keyword "nonnull"
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> jDoc) <> hardline)
                , keyword "null"
                    <+> operator "=>"
                    <+> braces (nest 4 (hardline <> nDoc) <> hardline)
                ]

instance PrettyColored Record where
    prettyColored (Record name tyParams fieldsRef kind vis cap _) = do
        visDoc <- prettyColored vis
        capDoc <- prettyColored cap
        nameDoc <- prettyColored name
        genericsDoc <- prettyGenerics tyParams
        fieldsMaybe <- readIORef' fieldsRef
        fieldsDoc <- case fieldsMaybe of
            Just f -> prettyFields f
            Nothing -> pure $ comment "<un-elaborated>"
        pure $
            visDoc
                <> keyword
                    ( case kind of
                        StructKind -> "struct"
                        EnumKind -> "enum"
                        EnumVariant _ _ -> "enum_variant"
                    )
                <> (case cap of Cap.Unspecified -> mempty; _ -> space <> brackets capDoc)
                <+> nameDoc
                <> genericsDoc
                <> fieldsDoc
      where
        prettyGenerics [] = pure mempty
        prettyGenerics gs = do
            gsDocs <- mapM prettyGeneric gs
            pure $ angles (commaSep gsDocs)

        prettyGeneric (n, GenericID gid) = do
            nDoc <- prettyColored n
            pure $ nDoc <> "@" <> pretty gid

        prettyFields (Unnamed fs) = do
            fsDocs <- mapM prettyUnnamedField (V.toList fs)
            pure $ parens (commaSep fsDocs)
        prettyFields (Variants vs) = do
            vsDocs <- mapM (\(WithSpan v _ _) -> prettyColored v) (V.toList vs)
            pure $ braces (nest 4 (hardline <> vsep (punctuate comma vsDocs)) <> hardline)
        prettyFields (Named fs) = do
            fsDocs <- mapM prettyField (V.toList fs)
            pure $ braces (nest 4 (hardline <> vsep (punctuate comma fsDocs)) <> hardline)

        prettyField (WithSpan (n, t, fld) _ _) = do
            nDoc <- prettyColored n
            fldDoc <- prettyFieldFlag fld
            tDoc <- prettyColored t
            pure $ nDoc <> operator ":" <+> fldDoc <> tDoc

        prettyUnnamedField (WithSpan (t, fld) _ _) = do
            fldDoc <- prettyFieldFlag fld
            tDoc <- prettyColored t
            pure $ fldDoc <> tDoc

        prettyFieldFlag ::
            (IOE :> es, Prim :> es) => FieldFlag -> Eff es (Doc Style)
        prettyFieldFlag False = pure mempty
        prettyFieldFlag True = pure $ brackets (keyword "field") <> space

instance PrettyColored FunctionProto where
    prettyColored (FunctionProto vis name generics params retType isRegional bodyRef _span) = do
        visDoc <- prettyColored vis
        nameDoc <- prettyColored name
        genericsDoc <- prettyGenerics generics
        paramsDoc <- zipWithM prettyArg params [0 :: Int ..]
        retDoc <- prettyColored retType
        bodyExpr <- readIORef' bodyRef
        bodyDoc <- case bodyExpr of
            Just b -> do
                bDoc <- prettyColored b
                pure $ braces (nest 4 (hardline <> bDoc) <> hardline)
            Nothing -> pure $ operator ";"

        pure $
            visDoc
                <> (if isRegional then keyword "regional" <> space else mempty)
                <> keyword "fn"
                <+> nameDoc
                <> genericsDoc
                <> parens (commaSep paramsDoc)
                <+> operator "->"
                <+> retDoc
                <+> bodyDoc
      where
        prettyGenerics [] = pure mempty
        prettyGenerics gs = do
            gsDocs <- mapM prettyGeneric gs
            pure $ angles (commaSep gsDocs)

        prettyGeneric (n, GenericID gid) = do
            nDoc <- prettyColored n
            pure $ nDoc <> "@" <> pretty gid

        prettyArg (n, t) idx = do
            nDoc <- prettyColored n
            tDoc <- prettyColored t
            let indexAnnotated = "v" <> pretty idx <> space <> parens nDoc
            pure $ indexAnnotated <> operator ":" <+> tDoc

commaSep :: [Doc Style] -> Doc Style
commaSep = concatWith (surround (comma <> space))

-- Conversion Functions

styleToAnsi :: Style -> AnsiStyle
styleToAnsi StyleKeyword = color Blue <> bold
styleToAnsi StyleTypeName = color Green
styleToAnsi StyleLiteral = color Yellow
styleToAnsi StyleVariable = color White
styleToAnsi StyleOperator = color Cyan
styleToAnsi StyleComment = color Black <> bold

styleToSGR :: Style -> [SGR]
styleToSGR StyleKeyword = [SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
styleToSGR StyleTypeName = [SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
styleToSGR StyleLiteral = [SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
styleToSGR StyleVariable = [SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
styleToSGR StyleOperator = [SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
styleToSGR StyleComment = [SetColor ANSI.Foreground ANSI.Dull ANSI.Black, ANSI.SetConsoleIntensity ANSI.BoldIntensity] -- Adjusted to look like gray

renderAnsi :: Doc Style -> Doc AnsiStyle
renderAnsi = reAnnotate styleToAnsi

docToFormattedText :: Doc Style -> [TextWithFormat]
docToFormattedText doc = go (layoutPretty defaultLayoutOptions doc) []
  where
    go :: SimpleDocStream Style -> [Style] -> [TextWithFormat]
    go SFail _ = []
    go SEmpty _ = []
    go (SChar c rest) styles =
        let txt = T.singleton c
            current = TextWithFormat txt (concatMap styleToSGR (reverse styles))
        in merge current (go rest styles)
    go (SText _ t rest) styles =
        let current = TextWithFormat t (concatMap styleToSGR (reverse styles))
        in merge current (go rest styles)
    go (SLine i rest) styles =
        let txt = "\n" <> T.replicate i " "
            current = TextWithFormat txt (concatMap styleToSGR (reverse styles))
        in merge current (go rest styles)
    go (SAnnPush style rest) styles = go rest (style : styles)
    go (SAnnPop rest) (_:styles) = go rest styles
    go (SAnnPop rest) [] = go rest [] -- Should not happen

    merge :: TextWithFormat -> [TextWithFormat] -> [TextWithFormat]
    merge t [] = [t]
    merge (TextWithFormat t1 s1) (TextWithFormat t2 s2 : rest)
        | s1 == s2 = merge (TextWithFormat (t1 <> t2) s1) rest
        | otherwise = TextWithFormat t1 s1 : TextWithFormat t2 s2 : rest
