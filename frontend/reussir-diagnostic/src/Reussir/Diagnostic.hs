module Reussir.Diagnostic (
    -- * Diagnostic Reporting
    -- $doc
    module Reussir.Diagnostic.Report,
    module Reussir.Diagnostic.Display,
    module Reussir.Diagnostic.Repository,
) where

import Reussir.Diagnostic.Display
import Reussir.Diagnostic.Report
import Reussir.Diagnostic.Repository

{- $doc
This library provides a way to generate diagnostic reports for the Reussir compiler.
It supports rich text formatting, code snippets, and annotations.

The main types are:

* 'Report': A diagnostic report, which can be a sequence of reports, a labeled report, or a code reference.
* 'Repository': A repository of source files, used to look up code snippets.
* 'Display': Functions to display reports to the console.
-}
