{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Full.Error where

import Data.Function ((&))
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (
    Report (..),
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )

import Data.Text qualified as T
import System.Console.ANSI.Types qualified as ANSI

import Reussir.Core.Data.Full.Error (ErrorKind (..))
import Reussir.Core.Data.UniqueID (GenericID (..))

import Reussir.Core.Data.Full.Error qualified as Full

errorToReport :: Full.Error -> FilePath -> Report
errorToReport (Full.Error (start, end) kind) file =
    let cr =
            defaultCodeRef file start end
                & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid
        msgText =
            defaultText (errorKindToText kind)
                & addForegroundColorToText ANSI.Red ANSI.Vivid
     in Labeled Error (FormattedText [defaultText "Full Elaboration Error"])
            <> Nested (annotatedCodeRef cr msgText)

errorKindToText :: ErrorKind -> T.Text
errorKindToText (InvalidRecordField path args idx) =
    "Invalid record field at index "
        <> T.pack (show idx)
        <> " for record "
        <> T.pack (show path)
        <> " with args "
        <> T.pack (show args)
errorKindToText (InvalidNullableType ty) =
    "Invalid nullable type: " <> T.pack (show ty)
errorKindToText (UnknownGeneric (GenericID gid)) =
    "Unknown generic ID: " <> T.pack (show gid)
errorKindToText (InvalidCapability path cap) =
    "Invalid capability "
        <> T.pack (show cap)
        <> " for record "
        <> T.pack (show path)
errorKindToText InvalidAssignSourceCapability =
    "Assignment source must be Nullable<Rc<T, Flex>> (flex capability required)"
errorKindToText InvalidAssignSourceNotRegional =
    "Assignment source inner type must be a regional record"
