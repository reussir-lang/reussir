module Reussir.Diagnostic.Report where

import Data.Int (Int64)
import Data.Text (Text)

import System.Console.ANSI.Types qualified as ANSI

data TextWithFormat = TextWithFormat
    { textContent :: Text
    , textFormats :: [ANSI.SGR]
    }
    deriving (Show, Eq)

data CodeReference = CodeReference
    { codeFilePath :: FilePath
    , codeStartOffset :: Int64
    , codeEndOffset :: Int64
    , codeFormats :: [ANSI.SGR]
    }
    deriving (Show, Eq)

data Label
    = Info
    | Warning
    | Error
    | Hint
    deriving (Show, Eq)

data Report
    = Nested Report --- a nested report with a deeper indentation level
    | Labeled Label Report --- a report with default set of labels
    | CodeRef CodeReference (Maybe TextWithFormat) --- a code reference
    | FormattedText [TextWithFormat] --- formatted text segments (no newline in between)
    | ReportNil --- empty report
    | ReportSeq Report Report --- sequence of two reports
    deriving (Show, Eq)

defaultText :: Text -> TextWithFormat
defaultText txt = TextWithFormat txt []

addSGRToText ::
    ANSI.SGR ->
    TextWithFormat ->
    TextWithFormat
addSGRToText sgr twf =
    twf{textFormats = sgr : textFormats twf}

addForegroundColorToText ::
    ANSI.Color ->
    ANSI.ColorIntensity ->
    TextWithFormat ->
    TextWithFormat
addForegroundColorToText color intensity =
    addSGRToText (ANSI.SetColor ANSI.Foreground intensity color)

addBackgroundColorToText ::
    ANSI.Color ->
    ANSI.ColorIntensity ->
    TextWithFormat ->
    TextWithFormat
addBackgroundColorToText color intensity =
    addSGRToText (ANSI.SetColor ANSI.Background intensity color)

addItalicToText ::
    TextWithFormat ->
    TextWithFormat
addItalicToText = addSGRToText (ANSI.SetItalicized True)

addBoldToText ::
    TextWithFormat ->
    TextWithFormat
addBoldToText = addSGRToText (ANSI.SetConsoleIntensity ANSI.BoldIntensity)

addUnderlineToText ::
    TextWithFormat ->
    TextWithFormat
addUnderlineToText = addSGRToText (ANSI.SetUnderlining ANSI.SingleUnderline)

addDoubleUnderlineToText ::
    TextWithFormat ->
    TextWithFormat
addDoubleUnderlineToText =
    addSGRToText (ANSI.SetUnderlining ANSI.DoubleUnderline)

defaultCodeRef :: FilePath -> Int64 -> Int64 -> CodeReference
defaultCodeRef path start end = CodeReference path start end []

addSGRToCodeRef ::
    ANSI.SGR ->
    CodeReference ->
    CodeReference
addSGRToCodeRef sgr cr = cr{codeFormats = sgr : codeFormats cr}

addForegroundColorToCodeRef ::
    ANSI.Color ->
    ANSI.ColorIntensity ->
    CodeReference ->
    CodeReference
addForegroundColorToCodeRef color intensity =
    addSGRToCodeRef (ANSI.SetColor ANSI.Foreground intensity color)

addBackgroundColorToCodeRef ::
    ANSI.Color ->
    ANSI.ColorIntensity ->
    CodeReference ->
    CodeReference
addBackgroundColorToCodeRef color intensity =
    addSGRToCodeRef (ANSI.SetColor ANSI.Background intensity color)

addItalicToCodeRef ::
    CodeReference ->
    CodeReference
addItalicToCodeRef = addSGRToCodeRef (ANSI.SetItalicized True)

addBoldToCodeRef ::
    CodeReference ->
    CodeReference
addBoldToCodeRef = addSGRToCodeRef (ANSI.SetConsoleIntensity ANSI.BoldIntensity)

addUnderlineToCodeRef ::
    CodeReference ->
    CodeReference
addUnderlineToCodeRef = addSGRToCodeRef (ANSI.SetUnderlining ANSI.SingleUnderline)

addDoubleUnderlineToCodeRef ::
    CodeReference ->
    CodeReference
addDoubleUnderlineToCodeRef =
    addSGRToCodeRef (ANSI.SetUnderlining ANSI.DoubleUnderline)

instance Semigroup Report where
    ReportNil <> r = r
    r <> ReportNil = r
    r1 <> r2 = ReportSeq r1 r2

instance Monoid Report where
    mempty = ReportNil

codeRef :: CodeReference -> Report
codeRef cr = CodeRef cr Nothing

annotatedCodeRef :: CodeReference -> TextWithFormat -> Report
annotatedCodeRef cr twf = CodeRef cr (Just twf)
