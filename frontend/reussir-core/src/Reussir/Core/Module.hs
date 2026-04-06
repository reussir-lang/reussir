{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reussir.Core.Module
Description : Declaration-driven module resolution for packages.

This module implements Rust-like module resolution driven by @mod@ declarations
in the source code. Given a package root and name, it parses the root @lib.rr@,
collects @mod@ declarations, resolves them to file paths, and recursively
processes sub-modules.

== Module Path Convention

@
\/root\/lib.rr            → [pkgName]          (root file, always included)
mod math;  in lib.rr      → [pkgName, "math"]  (resolves math.rr or math\/mod.rr)
mod utils; in lib.rr      → [pkgName, "utils"] (resolves utils.rr or utils\/mod.rr)
mod sub;   in utils/mod.rr→ [pkgName, "utils", "sub"]
@
-}
module Reussir.Core.Module (
    PackageInfo (..),
    computeModulePath,
    discoverPackageFiles,
) where

import Data.List (sort)
import Reussir.Parser.Prog (parseProg)
import Reussir.Parser.Types.Lexer (Identifier (..), WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import System.Directory (
    canonicalizePath,
    doesFileExist,
 )
import System.FilePath (
    dropExtension,
    makeRelative,
    splitDirectories,
    takeDirectory,
    (</>),
 )
import Text.Megaparsec (runParser)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Information about a package being compiled.
data PackageInfo = PackageInfo
    { packageRoot :: FilePath
    -- ^ Absolute path to the package root directory
    , packageName :: Identifier
    -- ^ The root name of the package (e.g., "mylib")
    }

{- | Compute the module path segments for a source file given its package context.

The mapping from filesystem to module path follows Rust conventions:

  * @lib.rr@ at the root maps to @[packageName]@ (crate root)
  * @foo.rr@ at the root maps to @[packageName, "foo"]@
  * @foo\/mod.rr@ maps to @[packageName, "foo"]@
  * @foo\/bar.rr@ maps to @[packageName, "foo", "bar"]@

Both @filePath@ and @packageRoot@ should be canonicalized before calling.
-}
computeModulePath :: PackageInfo -> FilePath -> [Identifier]
computeModulePath pkg filePath =
    let rel = makeRelative (packageRoot pkg) filePath
        parts = splitDirectories (dropExtension rel)
        -- Filter out "." from splitDirectories output
        cleanParts = filter (/= ".") parts
        -- Convert to identifiers, handling mod.rr convention
        moduleSegs = case cleanParts of
            -- lib.rr at root → just package name
            ["lib"] -> []
            -- mod.rr in a subdir → use the directory as the module name
            segs
                | not (null segs) && last segs == "mod" -> init segs
                | otherwise -> segs
     in packageName pkg : map (Identifier . T.pack) moduleSegs

{- | Discover package files by following @mod@ declarations starting from @lib.rr@.

Parses the root file, collects @mod name;@ statements, resolves each to
@name.rr@ or @name\/mod.rr@ relative to the declaring file, and recurses.
Returns pairs of @(absoluteFilePath, modulePath)@ sorted by file path.
-}
discoverPackageFiles :: PackageInfo -> IO [(FilePath, [Identifier])]
discoverPackageFiles pkg = do
    root <- canonicalizePath (packageRoot pkg)
    let pkg' = pkg{packageRoot = root}
    let rootFile = root </> "lib.rr"
    rootExists <- doesFileExist rootFile
    if rootExists
        then do
            rootCanon <- canonicalizePath rootFile
            children <- resolveModDecls pkg' rootCanon
            let all' = (rootCanon, computeModulePath pkg' rootCanon) : children
            return $ sort all'
        else return []

{- | Parse a file and recursively resolve its @mod@ declarations.

For each @mod name;@ found, tries @name.rr@ then @name\/mod.rr@ relative
to the declaring file's directory. Successfully resolved modules are
recursively processed for their own @mod@ declarations.
-}
resolveModDecls :: PackageInfo -> FilePath -> IO [(FilePath, [Identifier])]
resolveModDecls pkg filePath = do
    content <- TIO.readFile filePath
    case runParser parseProg filePath content of
        Left _ -> return []
        Right stmts -> do
            let modNames = collectModDecls stmts
            let parentDir = takeDirectory filePath
            concat <$> mapM (resolveOneModule pkg parentDir) modNames

-- | Extract module names from @mod@ declarations in a list of statements.
collectModDecls :: [Syn.Stmt] -> [Identifier]
collectModDecls = concatMap go
  where
    go (Syn.ModStmt _vis name) = [name]
    go (Syn.SpannedStmt (WithSpan s _ _)) = go s
    go _ = []

{- | Resolve a single @mod name;@ declaration to a file path.

Tries @parentDir\/name.rr@ first, then @parentDir\/name\/mod.rr@.
If found, computes the module path and recurses into the resolved file.
-}
resolveOneModule :: PackageInfo -> FilePath -> Identifier -> IO [(FilePath, [Identifier])]
resolveOneModule pkg parentDir (Identifier name) = do
    let nameStr = T.unpack name
    let fileCandidate = parentDir </> nameStr ++ ".rr"
    let dirCandidate = parentDir </> nameStr </> "mod.rr"
    fileExists <- doesFileExist fileCandidate
    if fileExists
        then do
            canon <- canonicalizePath fileCandidate
            let modPath = computeModulePath pkg canon
            children <- resolveModDecls pkg canon
            return $ (canon, modPath) : children
        else do
            dirExists <- doesFileExist dirCandidate
            if dirExists
                then do
                    canon <- canonicalizePath dirCandidate
                    let modPath = computeModulePath pkg canon
                    children <- resolveModDecls pkg canon
                    return $ (canon, modPath) : children
                else return []
