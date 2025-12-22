{-# LANGUAGE OverloadedStrings #-}

module Reussir.Diagnostic.Display where

import Data.Int (Int64)
import Data.String (fromString)
import Data.Text.Builder.Linear (Builder, fromChar, fromText, runBuilder)
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy qualified as LazyText
import Effectful (Eff, IOE, liftIO, (:>))
import GHC.IO.Handle (Handle)
import Reussir.Diagnostic.LineCache (SelectedLine (..))
import Reussir.Diagnostic.Report (CodeReference (..), Label (..), Report (..), TextWithFormat (textFormats), textContent)
import Reussir.Diagnostic.Repository (Repository, lookupRepository)
import System.Console.ANSI.Codes (SGR, setSGRCode)
import System.Console.ANSI.Types qualified as ANSI
import UnliftIO.IO (hIsTerminalDevice)

data TableCodec
    = Standard
    | Extended
    deriving (Show, Eq)

verticalLine :: TableCodec -> Char
verticalLine Standard = '|'
verticalLine Extended = '│'

horitonzalLine :: TableCodec -> Char
horitonzalLine Standard = '-'
horitonzalLine Extended = '─'

dashedVerticalLine :: TableCodec -> Char
dashedVerticalLine Standard = '|'
dashedVerticalLine Extended = '┆'

dashedHorizontalLine :: TableCodec -> Char
dashedHorizontalLine Standard = '-'
dashedHorizontalLine Extended = '┄'

ulCorner :: TableCodec -> Char
ulCorner Standard = '+'
ulCorner Extended = '┌'

urCorner :: TableCodec -> Char
urCorner Standard = '+'
urCorner Extended = '┐'

llCorner :: TableCodec -> Char
llCorner Standard = '+'
llCorner Extended = '└'

lrCorner :: TableCodec -> Char
lrCorner Standard = '+'
lrCorner Extended = '┘'

roundULCorner :: TableCodec -> Char
roundULCorner Standard = '+'
roundULCorner Extended = '╭'

roundURCorner :: TableCodec -> Char
roundURCorner Standard = '+'
roundURCorner Extended = '╮'

roundLLCorner :: TableCodec -> Char
roundLLCorner Standard = '+'
roundLLCorner Extended = '╰'

roundLRCorner :: TableCodec -> Char
roundLRCorner Standard = '+'
roundLRCorner Extended = '╯'

leftTee :: TableCodec -> Char
leftTee Standard = '*'
leftTee Extended = '├'

rightTee :: TableCodec -> Char
rightTee Standard = '*'
rightTee Extended = '┤'

topTee :: TableCodec -> Char
topTee Standard = '*'
topTee Extended = '┬'

bottomTee :: TableCodec -> Char
bottomTee Standard = '*'
bottomTee Extended = '┴'

cross :: TableCodec -> Char
cross Standard = '*'
cross Extended = '┼'

warningIcon :: TableCodec -> Char
warningIcon Standard = '!'
warningIcon Extended = '⚠'

infoIcon :: TableCodec -> Char
infoIcon Standard = 'i'
infoIcon Extended = '🛈'

errorIcon :: TableCodec -> Char
errorIcon Standard = 'x'
errorIcon Extended = '✖'

hintIcon :: TableCodec -> Char
hintIcon Standard = '?'
hintIcon Extended = '💡'

data FormatEnv = FormatEnv
    { formatConfigTableCodec :: TableCodec
    , indentation :: Int64
    , indentationWidth :: Int64
    , ignoreSGR :: Bool
    , repository :: Repository
    }
    deriving (Show)

createFmtEnv ::
    TableCodec ->
    Int64 ->
    Bool ->
    Repository ->
    FormatEnv
createFmtEnv tc indentWidth ignore repo =
    FormatEnv
        { formatConfigTableCodec = tc
        , indentation = 0
        , indentationWidth = indentWidth
        , ignoreSGR = ignore
        , repository = repo
        }

incIndentation :: FormatEnv -> FormatEnv
incIndentation env =
    env{indentation = indentation env + indentationWidth env}

sgrBuilder :: [SGR] -> FormatEnv -> Builder -> Builder
sgrBuilder sgr env builder =
    if ignoreSGR env
        then builder
        else fromString (setSGRCode sgr) <> builder <> fromString (setSGRCode [])

labelToBuilderText :: FormatEnv -> Label -> Builder
labelToBuilderText env Info = "[ " <> fromChar (infoIcon (formatConfigTableCodec env)) <> " INFO ]"
labelToBuilderText env Warning = "[ " <> fromChar (warningIcon (formatConfigTableCodec env)) <> " WARNING ]"
labelToBuilderText env Error = "[ " <> fromChar (errorIcon (formatConfigTableCodec env)) <> " ERROR ]"
labelToBuilderText env Hint = "[ " <> fromChar (hintIcon (formatConfigTableCodec env)) <> " HINT ]"

labelToSGR :: Label -> SGR
labelToSGR Info = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue
labelToSGR Warning = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow
labelToSGR Error = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red
labelToSGR Hint = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan

labelToBuilder :: FormatEnv -> Label -> Builder
labelToBuilder env label =
    sgrBuilder [labelToSGR label] env (labelToBuilderText env label)

indentBuilder :: FormatEnv -> Builder
indentBuilder env = fromString (replicate (fromIntegral (indentation env)) ' ')

formatToBuilder :: FormatEnv -> Report -> Builder
formatToBuilder _ ReportNil = mempty
formatToBuilder env (ReportSeq r1 r2) =
    formatToBuilder env r1 <> "\n" <> formatToBuilder env r2
formatToBuilder env (Nested rpt) =
    formatToBuilder (incIndentation env) rpt
formatToBuilder env (Labeled label rpt) =
    indentBuilder env <> labelToBuilder env label <> fromChar ' ' <> formatToBuilder env rpt
formatToBuilder env (FormattedText segments) =
    indentBuilder env <> mconcat [sgrBuilder sgr env (fromText $ textContent txt) | txt <- segments, let sgr = textFormats txt]
formatToBuilder env (CodeRef ref maybeText) =
    header <> "\n" <> body
  where
    selectedLines :: [SelectedLine]
    selectedLines = lookupRepository (repository env) (codeFilePath ref, codeStartOffset ref, codeEndOffset ref)

    minLineNumber :: Int64 -- first line number
    minLineNumber = case selectedLines of
        [] -> 0
        x : _ -> lineNumber x

    maxLineNumber :: Int64 -- last line number
    maxLineNumber = case selectedLines of
        [] -> 0
        _ -> lineNumber (last selectedLines)

    maxLineNumberWidth :: Int64 -- width of the line number column
    maxLineNumberWidth = fromIntegral (length (show maxLineNumber))

    maxCodeWidth :: Int64
    maxCodeWidth = case selectedLines of
        [] -> 0
        _ -> maximum (map (LazyText.length . lineText) selectedLines)

    header :: Builder
    header =
        indentBuilder env
            <> sgrBuilder [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan] env "--> "
            <> fromString (codeFilePath ref)
            <> ":"
            <> fromString (show minLineNumber)
            <> ":"
            <> fromString (show (case selectedLines of [] -> 0; l : _ -> lineColStart l))

    gutter :: Maybe Int64 -> Builder
    gutter maybeLineNum =
        indentBuilder env
            <> sgrBuilder
                [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
                env
                ( case maybeLineNum of
                    Just ln ->
                        let s = show ln
                         in fromString (replicate (fromIntegral maxLineNumberWidth - length s) ' ') <> fromString s
                    Nothing ->
                        fromString (replicate (fromIntegral maxLineNumberWidth) ' ')
                )
            <> fromChar ' '
            <> sgrBuilder [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White] env (fromChar (verticalLine (formatConfigTableCodec env)))
            <> fromChar ' '

    codeContent :: SelectedLine -> Builder
    codeContent line =
        let txt = lineText line
            start = fromIntegral (lineColStart line) - 1
            len = fromIntegral (lineColEnd line) - start
            (pre, rest) = LazyText.splitAt start txt
            (mid, post) = LazyText.splitAt len rest
         in fromText (LazyText.toStrict pre)
                <> sgrBuilder (codeFormats ref) env (fromText (LazyText.toStrict mid))
                <> fromText (LazyText.toStrict post)

    -- if there is a single line, no suffix
    -- if there is a multiple line with annotation (maybeText is Just):
    -- - on the minLine: " " + roundUpperRightCorner
    -- - on the maxLine: " " + roundLowerRightCorner
    -- - in the middle lines: " " + vertical line
    -- - all these suffixes have the same SGR as annotation
    codeSuffix :: SelectedLine -> Builder
    codeSuffix line =
        case maybeText of
            Nothing -> mempty
            Just twf
                | minLineNumber == maxLineNumber -> mempty
                | otherwise ->
                    let suffixChar
                            | lineNumber line == minLineNumber = roundURCorner (formatConfigTableCodec env)
                            | lineNumber line == maxLineNumber = roundLRCorner (formatConfigTableCodec env)
                            | otherwise = verticalLine (formatConfigTableCodec env)
                        suffixBuilder = fromChar ' ' <> fromChar suffixChar
                     in sgrBuilder (textFormats twf) env suffixBuilder

    renderLine :: SelectedLine -> Builder
    renderLine line = gutter (Just (lineNumber line)) <> codeContent line <> padding <> codeSuffix line
      where
        padding =
            case maybeText of
                Just _
                    | minLineNumber /= maxLineNumber ->
                        fromString (replicate (fromIntegral (maxCodeWidth - LazyText.length (lineText line))) ' ')
                _ -> mempty

    renderCaret :: Builder
    renderCaret =
        case selectedLines of
            [line] ->
                let start = (lineColStart line) - 1
                    len = fromIntegral (lineColEnd line) - start
                    spaces = fromString (replicate (fromIntegral start) ' ')
                    connector =
                        if len == 1
                            then fromChar (bottomTee (formatConfigTableCodec env))
                            else
                                fromChar (roundLLCorner (formatConfigTableCodec env))
                                    <> fromString (replicate (fromIntegral len - 2) (horitonzalLine (formatConfigTableCodec env)))
                                    <> fromChar (roundLRCorner (formatConfigTableCodec env))
                    msg = case maybeText of
                        Nothing -> mempty
                        Just twf -> fromChar ' ' <> sgrBuilder (textFormats twf) env (fromText (textContent twf))
                 in gutter Nothing <> spaces <> sgrBuilder (codeFormats ref) env connector <> msg
            _ ->
                case maybeText of
                    Nothing -> mempty
                    Just twf -> gutter Nothing <> sgrBuilder (textFormats twf) env (fromText (textContent twf))

    joinWithNewlines :: [Builder] -> Builder
    joinWithNewlines [] = mempty
    joinWithNewlines [x] = x
    joinWithNewlines (x : xs) = x <> fromChar '\n' <> joinWithNewlines xs

    body :: Builder
    body = joinWithNewlines (map renderLine selectedLines ++ [renderCaret | not (null selectedLines)])

displayReport :: (IOE :> es) => Report -> Repository -> Int64 -> Handle -> Eff es ()
displayReport report repo indentWidth hdl = do
    isTTY <- hIsTerminalDevice hdl
    let codec = if isTTY then Extended else Standard
        env = createFmtEnv codec indentWidth (not isTTY) repo
        builder = formatToBuilder env report
        text = runBuilder builder
    liftIO $ TextIO.hPutStr hdl text
